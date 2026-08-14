let atom value = Csexp.Atom value
let list values = Csexp.List values
let field name value = list [ atom name; value ]
let atom_field name value = field name (atom value)

let write sexp =
  Csexp.to_channel stdout sexp;
  flush stdout
;;

let directives ?(disable_warning_8 = false) ?preprocess ?(suffix = ".ml .mli") () =
  let suffix = list [ atom "SUFFIX"; atom suffix ] in
  let flags =
    (if disable_warning_8 then [ "-w"; "-8" ] else [])
    @
    match preprocess with
    | None -> []
    | Some command -> [ "-pp"; command ]
  in
  match flags with
  | [] -> [ suffix ]
  | flags -> [ list [ atom "FLG"; list (List.map atom flags) ]; suffix ]
;;

let configuration ~mode ~is_default ?(kind = "implementation") ?counterpart directives =
  list
    ([ atom "CONFIG"
     ; atom_field "MODE" mode
     ; atom_field "DEFAULT" (if is_default then "true" else "false")
     ; atom_field "KIND" kind
     ]
     @ (match counterpart with
        | None -> []
        | Some path -> [ atom_field "COUNTERPART" path ])
     @ [ field "DIRECTIVES" (list directives) ])
;;

let configurations values = list [ atom "CONFIGURATIONS"; list values ]

let two_configurations =
  configurations
    [ configuration ~mode:"ocaml" ~is_default:true (directives ())
    ; configuration
        ~mode:"melange"
        ~is_default:false
        (directives ~disable_warning_8:true ())
    ]
;;

let divergent_suffix_configurations =
  configurations
    [ configuration ~mode:"ocaml" ~is_default:true (directives ())
    ; configuration
        ~mode:"melange"
        ~is_default:false
        (directives ~suffix:".ml .melange.mli" ())
    ]
;;

let legacy_config = list (directives ())

let preprocessed_configuration mode ~is_default =
  let executable = Sys.getenv "FAKE_OCAML_MERLIN_EXE" |> Filename.quote in
  let preprocess = Printf.sprintf "%s --pp %s" executable mode in
  configuration ~mode ~is_default (directives ~preprocess ())
;;

let preprocessed_configurations ?(include_native = false) () =
  let values =
    [ preprocessed_configuration "ocaml" ~is_default:true
    ; preprocessed_configuration "melange" ~is_default:false
    ]
  in
  let values =
    if include_native
    then values @ [ preprocessed_configuration "native" ~is_default:false ]
    else values
  in
  configurations values
;;

let reversed_preprocessed_configurations () =
  configurations
    [ preprocessed_configuration "melange" ~is_default:false
    ; preprocessed_configuration "ocaml" ~is_default:true
    ]
;;

let extended_preprocessed_configuration () =
  configurations [ preprocessed_configuration "extended" ~is_default:true ]
;;

let exclusive_preprocessed_configuration () =
  let executable = Sys.getenv "FAKE_OCAML_MERLIN_EXE" |> Filename.quote in
  let preprocess = Printf.sprintf "%s --pp melange" executable in
  configurations
    [ configuration ~mode:"melange" ~is_default:false (directives ~preprocess ()) ]
;;

let counterpart path mode =
  let extension = Filename.extension path in
  let stem = Filename.remove_extension path in
  match extension with
  | ".ml" -> stem ^ "." ^ mode ^ ".mli"
  | ".mli" -> stem ^ "." ^ mode ^ ".ml"
  | _ -> path ^ "." ^ mode
;;

let counterpart_configurations path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  let kind =
    match Filename.extension path with
    | ".mli" -> "interface"
    | _ -> "implementation"
  in
  configurations
    [ configuration
        ~mode:"ocaml"
        ~is_default:true
        ~kind
        ~counterpart:(counterpart path "ocaml")
        (directives ())
    ; configuration
        ~mode:"melange"
        ~is_default:false
        ~kind
        ~counterpart:(counterpart path "melange")
        (directives ())
    ]
;;

let disjoint_counterpart_configurations path =
  if String.ends_with ~suffix:".mli" path
  then counterpart_configurations path
  else (
    let mode = if String.ends_with ~suffix:".ocaml.ml" path then "melange" else "ocaml" in
    configurations
      [ configuration ~mode ~is_default:true ~kind:"implementation" (directives ()) ])
;;

let protocol =
  match Sys.getenv_opt "FAKE_OCAML_MERLIN_PROTOCOL" with
  | None | Some "plural" -> `Plural
  | Some "exclusive" -> `Exclusive
  | Some "exclusive-preprocessed" -> `Exclusive_preprocessed
  | Some "legacy" -> `Legacy
  | Some "malformed" -> `Malformed
  | Some "empty" -> `Empty
  | Some "error" -> `Error
  | Some "by-path" -> `By_path
  | Some "preprocessed" -> `Preprocessed
  | Some "reversed-preprocessed" -> `Reversed_preprocessed
  | Some "extended-preprocessed" -> `Extended_preprocessed
  | Some "slow-preprocessed" -> `Slow_preprocessed
  | Some "three-preprocessed" -> `Three_preprocessed
  | Some "divergent-suffixes" -> `Divergent_suffixes
  | Some "counterparts" -> `Counterparts
  | Some "disjoint-counterparts" -> `Disjoint_counterparts
  | Some protocol -> failwith ("unknown fake protocol: " ^ protocol)
;;

let response_for_path path =
  match protocol with
  | `Plural -> two_configurations
  | `Exclusive ->
    configurations [ configuration ~mode:"melange" ~is_default:false (directives ()) ]
  | `Exclusive_preprocessed -> exclusive_preprocessed_configuration ()
  | `Malformed -> atom "malformed"
  | `Empty -> configurations []
  | `Error -> list [ atom "CONFIGURATIONS-ERROR"; atom "configuration failed" ]
  | `By_path ->
    let mode = Filename.basename path |> Filename.remove_extension in
    configurations [ configuration ~mode ~is_default:true (directives ()) ]
  | `Preprocessed | `Slow_preprocessed -> preprocessed_configurations ()
  | `Reversed_preprocessed -> reversed_preprocessed_configurations ()
  | `Extended_preprocessed -> extended_preprocessed_configuration ()
  | `Three_preprocessed -> preprocessed_configurations ~include_native:true ()
  | `Divergent_suffixes -> divergent_suffix_configurations
  | `Counterparts -> counterpart_configurations path
  | `Disjoint_counterparts -> disjoint_counterpart_configurations path
  | `Legacy ->
    list [ list [ atom "ERROR"; atom ("Bad input: (File-Configurations " ^ path ^ ")") ] ]
;;

let log_request request =
  match Sys.getenv_opt "FAKE_OCAML_MERLIN_LOG" with
  | None -> ()
  | Some path ->
    let channel = open_out_gen [ Open_creat; Open_text; Open_append ] 0o600 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
         Csexp.to_channel channel request;
         output_char channel '\n')
;;

let rec loop () =
  match Csexp.input_opt stdin with
  | Error message -> failwith message
  | Ok None | Ok (Some (Atom "Halt")) -> ()
  | Ok (Some (List [ Atom "File-Configurations"; Atom path ] as request)) ->
    log_request request;
    if protocol = `By_path && String.equal (Filename.basename path) "a.ml"
    then Unix.sleepf 0.1;
    write (response_for_path path);
    loop ()
  | Ok (Some (List [ Atom "File"; Atom _ ] as request)) ->
    log_request request;
    write legacy_config;
    loop ()
  | Ok (Some request) ->
    log_request request;
    write legacy_config;
    loop ()
;;

let has_substring_at text pattern offset =
  let rec loop index =
    index = String.length pattern
    || (Char.equal text.[offset + index] pattern.[index] && loop (index + 1))
  in
  offset + String.length pattern <= String.length text && loop 0
;;

let replace_all text ~pattern ~replacement =
  if String.length pattern <> String.length replacement
  then invalid_arg ("non-position-preserving replacement for " ^ pattern);
  let result = Bytes.of_string text in
  let rec loop offset =
    if offset + String.length pattern > String.length text
    then ()
    else if has_substring_at text pattern offset
    then (
      Bytes.blit_string replacement 0 result offset (String.length replacement);
      loop (offset + String.length pattern))
    else loop (offset + 1)
  in
  loop 0;
  Bytes.unsafe_to_string result
;;

let padded pattern replacement =
  let padding = String.length pattern - String.length replacement in
  if padding < 0 then invalid_arg ("replacement is longer than " ^ pattern);
  replacement ^ String.make padding ' '
;;

let log_preprocess mode =
  match Sys.getenv_opt "FAKE_OCAML_MERLIN_PP_LOG" with
  | None -> ()
  | Some path ->
    let channel = open_out_gen [ Open_creat; Open_text; Open_append ] 0o600 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel (mode ^ "\n"))
;;

let preprocess mode path =
  log_preprocess mode;
  if protocol = `Slow_preprocessed && String.equal mode "melange" then Unix.sleepf 5.0;
  let input = In_channel.with_open_bin path In_channel.input_all in
  if String.equal mode "extended"
  then print_string (input ^ "\nlet =")
  else (
    let mode_replacements =
      match mode with
      | "ocaml" ->
        [ "MODE_EXPR", "1"
        ; "MODE_SYMBOL_EXPR", "123456"
        ; "MODE_NAME", "ocamlOnly"
        ; "MODE_LAMBDA________________", "fun a b -> a + b"
        ; "MODE_CALL__________________", "apply 1"
        ; "OCAML_ON", ""
        ; "OCAML_OFF", ""
        ; "MELANGE_ON", "(*"
        ; "MELANGE_OFF", "*)"
        ]
      | "melange" ->
        [ "MODE_EXPR", "\"text\""
        ; "MODE_SYMBOL_EXPR", "\"text\""
        ; "MODE_NAME", "melanOnly"
        ; "MODE_LAMBDA________________", "fun x a b -> x + a + b"
        ; "MODE_CALL__________________", "apply 0 1"
        ; "OCAML_ON", "(*"
        ; "OCAML_OFF", "*)"
        ; "MELANGE_ON", ""
        ; "MELANGE_OFF", ""
        ]
      | "native" ->
        [ "MODE_EXPR", "true"
        ; "MODE_SYMBOL_EXPR", "(true)"
        ; "MODE_NAME", "nativeOne"
        ; "MODE_LAMBDA________________", "fun a b -> a + b"
        ; "MODE_CALL__________________", "apply 1"
        ; "OCAML_ON", ""
        ; "OCAML_OFF", ""
        ; "MELANGE_ON", ""
        ; "MELANGE_OFF", ""
        ]
      | mode -> invalid_arg ("unknown preprocessing mode: " ^ mode)
    in
    List.fold_left
      (fun source (pattern, replacement) ->
         replace_all source ~pattern ~replacement:(padded pattern replacement))
      input
      mode_replacements
    |> print_string)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; "--pp"; mode; path ] -> preprocess mode path
  | _ -> loop ()
;;
