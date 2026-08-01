open Import

let ocamllsp_source = "ocamllsp"
let dune_source = "dune"

let is_ocamllsp_source = function
  | None -> false
  | Some source ->
    String.equal source ocamllsp_source
    || String.is_prefix source ~prefix:(ocamllsp_source ^ " (")
;;

module Provenance = struct
  type classification =
    [ `External
    | `Malformed
    | `Modes of string list
    ]

  let data modes =
    `Assoc
      [ ( "ocamllsp"
        , `Assoc
            [ "version", `Int 1
            ; "modes", `List (List.map modes ~f:(fun mode -> `String mode))
            ] )
      ]
  ;;

  let classify (diagnostic : Diagnostic.t) =
    if not (is_ocamllsp_source diagnostic.source)
    then `External
    else (
      match diagnostic.data with
      | Some (`Assoc top) ->
        (match List.Assoc.find top "ocamllsp" ~equal:String.equal with
         | Some (`Assoc fields) ->
           (match
              ( List.Assoc.find fields "version" ~equal:String.equal
              , List.Assoc.find fields "modes" ~equal:String.equal )
            with
            | Some (`Int 1), Some (`List modes) ->
              let modes =
                List.map modes ~f:(function
                  | `String mode -> Some mode
                  | _ -> None)
              in
              if List.for_all modes ~f:Option.is_some
              then `Modes (List.filter_opt modes)
              else `Malformed
            | _ -> `Malformed)
         | _ -> `Malformed)
      | _ -> `Malformed)
  ;;
end

module Id = struct
  include Drpc.Diagnostic.Id

  let compare_ordering = compare
  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let compare x y = Ordering.to_int (compare_ordering x y)
end

module Dune = struct
  module Id = Import.Id.Make ()

  module T = struct
    type t =
      { pid : Pid.t
      ; id : Id.t
      }

    let compare x y =
      match Int.compare (Pid.to_int x.pid) (Pid.to_int y.pid) |> Ordering.of_int with
      | Eq -> Id.compare x.id y.id
      | r -> r
    ;;

    let hash { pid; id } = Poly.hash (Pid.to_int pid, id)
  end

  include T

  let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_opaque
  let gen pid = { pid; id = Id.gen () }
  let compare x y = Ordering.to_int (T.compare x y)
end

let equal_message =
  (* because the compiler and merlin wrap messages differently *)
  let is_space = function
    | ' ' | '\r' | '\n' | '\t' -> true
    | _ -> false
  in
  let eat_space s i =
    while !i < String.length s && is_space s.[!i] do
      incr i
    done
  in
  fun m1 m2 ->
    let i = ref 0 in
    let j = ref 0 in
    try
      eat_space m1 i;
      eat_space m2 j;
      while !i < String.length m1 && !j < String.length m2 do
        let ci = m1.[!i] in
        let cj = m2.[!j] in
        if is_space ci && is_space cj
        then (
          eat_space m1 i;
          eat_space m2 j)
        else if Char.equal ci cj
        then (
          incr i;
          incr j)
        else raise_notrace Exit
      done;
      eat_space m1 i;
      eat_space m2 j;
      (* we make sure that everything is consumed *)
      !i = String.length m1 && !j = String.length m2
    with
    | Exit -> false
;;

type t =
  { dune : (Dune.t, (Drpc.Diagnostic.Id.t, Uri.t * Diagnostic.t) Hashtbl.t) Hashtbl.t
  ; merlin : (Uri.t, Diagnostic.t list) Hashtbl.t
  ; merlin_generations : (Uri.t, int) Hashtbl.t
  ; send : PublishDiagnosticsParams.t list -> unit Fiber.t
  ; mutable dirty_uris : (Uri.t, Uri.comparator_witness) Set.t
  ; related_information : bool
  ; tags : DiagnosticTag.t list
  ; mutable report_dune_diagnostics : bool
  ; mutable shorten_merlin_diagnostics : bool
  }

let create
      (capabilities : ClientCapabilities.t)
      send
      ~report_dune_diagnostics
      ~shorten_merlin_diagnostics
  =
  let related_information =
    Capabilities.publish_diagnostics_related_information_support capabilities
  in
  let tags =
    Capabilities.publish_diagnostics_tag_support capabilities |> Option.value ~default:[]
  in
  { dune = Hashtbl.create (module Dune)
  ; merlin = Hashtbl.create (module Uri)
  ; merlin_generations = Hashtbl.create (module Uri)
  ; dirty_uris = Set.empty (module Uri)
  ; send
  ; related_information
  ; tags
  ; report_dune_diagnostics
  ; shorten_merlin_diagnostics
  }
;;

let begin_merlin_generation t uri =
  Hashtbl.update t.merlin_generations uri ~f:(function
    | None -> 1
    | Some generation -> generation + 1);
  Hashtbl.find_exn t.merlin_generations uri
;;

let merlin_generation_is_current t uri generation =
  Hashtbl.find t.merlin_generations uri |> Option.exists ~f:(Int.equal generation)
;;

module Range_map = Stdlib.MoreLabels.Map.Make (struct
    include Range

    let compare = Range.compare
  end)

let range_map_of_diagnostics diagnostics =
  List.rev_map diagnostics ~f:(fun (d : Diagnostic.t) -> d.range, d)
  |> List.fold_left ~init:Range_map.empty ~f:(fun acc (key, diagnostic) ->
    Range_map.update acc ~key ~f:(function
      | None -> Some [ diagnostic ]
      | Some diagnostics -> Some (diagnostic :: diagnostics)))
;;

(* TODO deduplicate related errors as well *)
let add_dune_diagnostic diagnostics (diagnostic : Diagnostic.t) =
  Range_map.update diagnostics ~key:diagnostic.range ~f:(fun existing ->
    Some
      (match existing with
       | None -> [ diagnostic ]
       | Some existing ->
         if
           List.exists existing ~f:(fun (merlin : Diagnostic.t) ->
             match merlin.source with
             | Some source when is_ocamllsp_source (Some source) ->
               (match merlin.message, diagnostic.message with
                | `String m1, `String m2 -> equal_message m1 m2
                | `MarkupContent { kind; value }, `MarkupContent mc ->
                  Poly.equal kind mc.kind && equal_message value mc.value
                | _, _ -> false)
             | None | Some _ -> false)
         then existing
         else diagnostic :: existing))
;;

let diagnostics_of_range_map diagnostics =
  Range_map.to_list diagnostics |> List.concat_map ~f:snd
;;

let merge ~merlin ~dune =
  List.fold_left dune ~init:(range_map_of_diagnostics merlin) ~f:add_dune_diagnostic
  |> diagnostics_of_range_map
;;

let send t which =
  Fiber.of_thunk (fun () ->
    let add_pending_dune_diagnostic pending uri diagnostic =
      let diagnostics =
        Hashtbl.find pending uri |> Option.value ~default:Range_map.empty
      in
      let diagnostics = add_dune_diagnostic diagnostics diagnostic in
      Hashtbl.set pending ~key:uri ~data:diagnostics
    in
    let dirty_uris =
      match which with
      | `All -> t.dirty_uris
      | `One uri -> Set.singleton (module Uri) uri
    in
    let pending = Hashtbl.create (module Uri) in
    Set.iter dirty_uris ~f:(fun uri ->
      let diagnostics = Hashtbl.find_multi t.merlin uri in
      Hashtbl.set pending ~key:uri ~data:(range_map_of_diagnostics diagnostics));
    let set_dune_source =
      let annotate_dune_pid = Hashtbl.length t.dune > 1 in
      if annotate_dune_pid
      then
        fun pid (d : Diagnostic.t) ->
          let source = Some (sprintf "dune (pid=%d)" (Pid.to_int pid)) in
          { d with source }
      else fun _pid x -> x
    in
    if t.report_dune_diagnostics
    then
      Hashtbl.fold ~init:() t.dune ~f:(fun ~key:dune ~data:per_dune () ->
        Hashtbl.iter per_dune ~f:(fun (uri, diagnostic) ->
          if Set.mem dirty_uris uri
          then (
            let diagnostic = set_dune_source dune.pid diagnostic in
            add_pending_dune_diagnostic pending uri diagnostic)));
    t.dirty_uris
    <- (match which with
        | `All -> Set.empty (module Uri)
        | `One uri -> Set.remove t.dirty_uris uri);
    Hashtbl.fold pending ~init:[] ~f:(fun ~key:uri ~data:diagnostics acc ->
      let diagnostics = diagnostics_of_range_map diagnostics in
      (* we don't include a version because some of the diagnostics might
         come from dune which reads from the file system and not from the
         editor's view *)
      PublishDiagnosticsParams.create ~uri ~diagnostics () :: acc)
    |> t.send)
;;

let set t what =
  let uri =
    match what with
    | `Dune (_, _, uri, _) -> uri
    | `Merlin (uri, _) -> uri
  in
  t.dirty_uris <- Set.add t.dirty_uris uri;
  match what with
  | `Merlin (uri, diagnostics) -> Hashtbl.set t.merlin ~key:uri ~data:diagnostics
  | `Dune (dune, id, uri, diagnostics) ->
    let dune_table =
      Hashtbl.find_or_add t.dune dune ~default:(fun _ -> Hashtbl.create (module Id))
    in
    Hashtbl.set dune_table ~key:id ~data:(uri, diagnostics)
;;

let remove t = function
  | `Dune (dune, diagnostic) ->
    Hashtbl.find t.dune dune
    |> Option.iter ~f:(fun dune ->
      Hashtbl.find dune diagnostic
      |> Option.iter ~f:(fun (uri, _) ->
        Hashtbl.remove dune diagnostic;
        t.dirty_uris <- Set.add t.dirty_uris uri))
  | `Merlin uri ->
    t.dirty_uris <- Set.add t.dirty_uris uri;
    Hashtbl.remove t.merlin uri;
    Hashtbl.remove t.merlin_generations uri
;;

let disconnect t dune =
  Hashtbl.find t.dune dune
  |> Option.iter ~f:(fun dune_diagnostics ->
    Hashtbl.iter dune_diagnostics ~f:(fun (uri, _) ->
      t.dirty_uris <- Set.add t.dirty_uris uri);
    Hashtbl.remove t.dune dune)
;;

let tags_of_message =
  let tags_of_message ~src message : DiagnosticTag.t option =
    match src with
    | `Dune when String.is_prefix message ~prefix:"unused" -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_unused_var_warning message -> Some Unnecessary
    | `Merlin when Diagnostic_util.is_deprecated_warning message -> Some Deprecated
    | `Dune | `Merlin -> None
  in
  fun t ~src message ->
    match tags_of_message ~src message with
    | None -> None
    | Some tag ->
      Option.some_if
        (Deprecation.tag_supported
           t.tags
           ~tag
           ~equal:(fun (left : DiagnosticTag.t) right ->
             match left, right with
             | Unnecessary, Unnecessary | Deprecated, Deprecated -> true
             | Unnecessary, _ | Deprecated, _ -> false))
        [ tag ]
;;

let extract_related_errors uri raw_message =
  match Ocamlc_loc.parse_raw raw_message with
  | `Message message :: related ->
    let string_of_message message = String.strip message in
    let related =
      let rec loop acc = function
        | `Loc loc :: `Message m :: xs -> loop ((loc, m) :: acc) xs
        | [] -> List.rev acc
        | _ ->
          (* give up when we see something unexpected *)
          Log.log ~section:"debug" (fun () ->
            Log.msg "unable to parse error" [ "error", `String raw_message ]);
          []
      in
      loop [] related
    in
    let related =
      match related with
      | [] -> None
      | related ->
        let make_related ({ Ocamlc_loc.path = _; lines; chars }, message) =
          let location =
            let start, end_ =
              let line_start, line_end =
                match lines with
                | Single i -> i, i
                | Range (i, j) -> i, j
              in
              let char_start, char_end =
                match chars with
                | None -> 1, 1
                | Some (x, y) -> x, y
              in
              ( Position.of_logical ~line:line_start ~character:char_start
              , Position.of_logical ~line:line_end ~character:char_end )
            in
            let range = Range.create ~start ~end_ in
            Location.create ~range ~uri
          in
          let message = string_of_message message in
          DiagnosticRelatedInformation.create ~location ~message
        in
        Some (List.map related ~f:make_related)
    in
    string_of_message message, related
  | _ -> raw_message, None
;;

let clamp_range_to_source source ({ Range.start; end_ } : Range.t) =
  let clamp position =
    let offset = Msource.get_offset source (Position.logical position) in
    let (`Logical (line, character)) = Msource.get_logical source offset in
    Position.of_logical ~line ~character
  in
  Range.create ~start:(clamp start) ~end_:(clamp end_)
;;

let first_n_lines_of_range (range : Range.t) n =
  if range.end_.line - range.start.line < n
  then range
  else (
    let start = Position.create ~character:range.start.character ~line:range.start.line
    and end_ = Position.create ~character:0 ~line:(range.start.line + n) in
    Range.create ~start ~end_)
;;

let error_to_diagnostics ~diagnostics ~merlin error =
  let doc = Document.Merlin.to_doc merlin in
  let create_diagnostic = Diagnostic.create ~source:ocamllsp_source in
  let uri = Document.uri doc |> Source_path.uri in
  let loc = Loc.loc_of_report error in
  let source = Document.Merlin.source merlin in
  let original_range = Range.of_loc loc |> clamp_range_to_source source in
  let range =
    if diagnostics.shorten_merlin_diagnostics
    then first_n_lines_of_range original_range 1
    else original_range
  in
  let severity =
    match error.source with
    | Warning -> DiagnosticSeverity.Warning
    | _ -> DiagnosticSeverity.Error
  in
  let make_message ppf m = String.strip (Format.asprintf "%a@." ppf m) in
  let message = make_message Loc.print_main error in
  let message, related_information =
    match diagnostics.related_information with
    | false -> message, None
    | true ->
      (match error.sub with
       | [] -> extract_related_errors uri message
       | _ :: _ ->
         ( message
         , Some
             (List.map error.sub ~f:(fun (sub : Loc.msg) ->
                let location =
                  let range = Range.of_loc sub.loc |> clamp_range_to_source source in
                  Location.create ~range ~uri
                in
                let message = make_message Loc.print_sub_msg sub in
                DiagnosticRelatedInformation.create ~location ~message)) ))
  in
  let maybe_extra_range_information =
    match diagnostics.shorten_merlin_diagnostics with
    | false -> None
    | true ->
      let start_location = Location.create ~range:original_range ~uri in
      Some
        [ DiagnosticRelatedInformation.create
            ~location:start_location
            ~message:"Original error span"
        ]
  in
  let relatedInformation =
    Option.merge maybe_extra_range_information related_information ~f:( @ )
  in
  let tags = tags_of_message diagnostics ~src:`Merlin message in
  create_diagnostic
    ?tags
    ?relatedInformation
    ~range
    ~message:(`String message)
    ~severity
    ()
;;

let canonicalize_diagnostic (diagnostic : Diagnostic.t) =
  let relatedInformation =
    Option.map diagnostic.relatedInformation ~f:(fun related ->
      List.map related ~f:(fun (info : DiagnosticRelatedInformation.t) ->
        { info with location = Source_path.location info.location }))
  in
  { diagnostic with relatedInformation; source = None; data = None }
;;

let merge_configured_diagnostics configurations configured =
  let all_configurations = Merlin_config.configuration_list configurations in
  let rec add configuration diagnostic = function
    | [] -> [ canonicalize_diagnostic diagnostic, [ configuration ] ]
    | (candidate, contributors) :: rest ->
      let diagnostic = canonicalize_diagnostic diagnostic in
      if Poly.equal candidate diagnostic
      then (
        let contributors =
          if List.exists contributors ~f:(fun contributor -> contributor == configuration)
          then contributors
          else configuration :: contributors
        in
        (candidate, contributors) :: rest)
      else (candidate, contributors) :: add configuration diagnostic rest
  in
  let groups, failures =
    Merlin_dot_protocol.Nonempty_list.to_list configured
    |> List.fold_left
         ~init:([], [])
         ~f:
           (fun
             (groups, failures)
             ({ configuration; result } : _ Document.Merlin.configured_result)
           ->
           match result with
           | Error error -> groups, (configuration, error) :: failures
           | Ok diagnostics ->
             ( List.fold_left diagnostics ~init:groups ~f:(fun groups diagnostic ->
                 add configuration diagnostic groups)
             , failures ))
  in
  List.iter failures ~f:(fun (configuration, error) ->
    Log.log ~section:"merlin" (fun () ->
      Log.msg
        "Merlin diagnostics configuration failed"
        [ "mode", `String (Merlin_config.configuration_label configuration)
        ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
        ]));
  List.map groups ~f:(fun (diagnostic, contributors) ->
    let contributors =
      List.filter all_configurations ~f:(fun configuration ->
        List.exists contributors ~f:(fun contributor -> contributor == configuration))
    in
    let source =
      if List.length contributors = List.length all_configurations
      then Some ocamllsp_source
      else
        Some
          (sprintf
             "%s (%s)"
             ocamllsp_source
             (List.map contributors ~f:Merlin_config.configuration_label
              |> String.concat ~sep:", "))
    in
    let modes =
      List.filter_map contributors ~f:(fun configuration ->
        Merlin_config.configuration_mode configuration
        |> Option.map ~f:Merlin_config.mode_key)
    in
    let data = Option.some_if (not (List.is_empty modes)) (Provenance.data modes) in
    { diagnostic with source; data })
  |> List.sort ~compare:(fun (left : Diagnostic.t) right ->
    Lsp.Range.compare left.range right.range)
;;

let merlin_diagnostics diagnostics merlin ~generation =
  let doc = Document.Merlin.to_doc merlin in
  let uri = Document.uri doc in
  let source = Document.Merlin.source merlin in
  let create_diagnostic = Diagnostic.create ~source:ocamllsp_source in
  let open Fiber.O in
  let* context = Document.Merlin.configuration_context merlin in
  match context with
  | Error errors ->
    Log.log ~section:"merlin" (fun () ->
      Log.msg
        "Merlin diagnostics configuration lookup failed"
        [ "errors", `List (List.map errors ~f:(fun error -> `String error)) ]);
    let current = merlin_generation_is_current diagnostics uri generation in
    if current then set diagnostics (`Merlin (uri, []));
    Fiber.return current
  | Ok { configurations; _ } ->
    let command =
      Query_protocol.Errors { lexing = true; parsing = true; typing = true }
    in
    let+ configured =
      Document.Merlin.with_configurations
        ~name:"diagnostics"
        merlin
        ~configurations
        (fun _ pipeline ->
           match Query_commands.dispatch pipeline command with
           | exception Merlin_extend.Extend_main.Handshake.Error error ->
             let message =
               `String
                 (sprintf
                    "%s.\nHint: install the following packages: merlin-extend, reason"
                    error)
             in
             [ create_diagnostic ~range:Lsp.Range.first_line ~message () ]
           | errors ->
             let merlin_diagnostics =
               List.rev_map errors ~f:(error_to_diagnostics ~diagnostics ~merlin)
             in
             let holes_as_err_diags =
               Query_commands.dispatch pipeline Holes
               |> List.rev_map ~f:(fun (loc, typ) ->
                 let range = Range.of_loc loc |> clamp_range_to_source source in
                 let severity = DiagnosticSeverity.Error in
                 let message =
                   "This typed hole should be replaced with an expression of type " ^ typ
                 in
                 create_diagnostic
                   ~code:(`String "hole")
                   ~range
                   ~message:(`String message)
                   ~severity
                   ())
             in
             List.rev_append holes_as_err_diags merlin_diagnostics)
    in
    let all_diagnostics = merge_configured_diagnostics configurations configured in
    let current = merlin_generation_is_current diagnostics uri generation in
    if current then set diagnostics (`Merlin (uri, all_diagnostics));
    current
;;

let set_report_dune_diagnostics t ~report_dune_diagnostics =
  if t.report_dune_diagnostics = report_dune_diagnostics
  then Fiber.return ()
  else (
    t.report_dune_diagnostics <- report_dune_diagnostics;
    Hashtbl.iter t.dune ~f:(fun per_dune ->
      Hashtbl.iter per_dune ~f:(fun (uri, _diagnostic) ->
        t.dirty_uris <- Set.add t.dirty_uris uri));
    send t `All)
;;

let set_shorten_merlin_diagnostics t ~shorten_merlin_diagnostics =
  t.shorten_merlin_diagnostics <- shorten_merlin_diagnostics;
  Fiber.return ()
;;
