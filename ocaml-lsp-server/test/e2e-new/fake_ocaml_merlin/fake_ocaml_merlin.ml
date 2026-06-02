let atom s = Csexp.Atom s
let list xs = Csexp.List xs
let directive tag args = list (atom tag :: args)

let write sexp =
  Csexp.to_channel stdout sexp;
  flush stdout
;;

let legacy_config =
  list [ directive "SUFFIX" [ atom ".ml" ]; directive "SUFFIX" [ atom ".mli" ] ]
;;

let directives ?(disable_warning_8 = false) () =
  let suffixes = [ directive "SUFFIX" [ atom ".ml" ]; directive "SUFFIX" [ atom ".mli" ] ] in
  if disable_warning_8
  then directive "FLG" [ list [ atom "-w"; atom "-8" ] ] :: suffixes
  else suffixes
;;

let configuration ~id ?mode ~default directives =
  let fields =
    [ atom "CONFIG"
    ; list [ atom "ID"; atom id ]
    ; list [ atom "DEFAULT"; atom (if default then "true" else "false") ]
    ; list [ atom "DIRECTIVES"; list directives ]
    ]
  in
  let fields =
    match mode with
    | None -> fields
    | Some mode -> fields @ [ list [ atom "MODE"; atom mode ] ]
  in
  list fields
;;

let configurations =
  list
    [ configuration ~id:"ocaml" ~mode:"ocaml" ~default:true (directives ())
    ; configuration
        ~id:"melange"
        ~mode:"melange"
        ~default:false
        (directives ~disable_warning_8:true ())
    ]
;;

let protocol =
  match Sys.getenv_opt "FAKE_OCAML_MERLIN_PROTOCOL" with
  | Some "new" -> `New
  | Some "old" | None -> `Old
  | Some protocol -> failwith ("unknown fake ocaml-merlin protocol: " ^ protocol)
;;

let rec loop () =
  match Csexp.input_opt stdin with
  | Error msg -> failwith msg
  | Ok None -> ()
  | Ok (Some (Atom "Halt")) -> ()
  | Ok (Some (List [ Atom "File-Configurations"; Atom _ ])) ->
    (match protocol with
     | `New -> write configurations
     | `Old -> write legacy_config);
    loop ()
  | Ok (Some (List [ Atom "File"; Atom _ ])) ->
    write legacy_config;
    loop ()
  | Ok (Some _) ->
    write legacy_config;
    loop ()
;;

let () = loop ()
