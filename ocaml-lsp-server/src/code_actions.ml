open Import
open Fiber.O

module Code_action_error = struct
  type t =
    | Initial
    | Need_merlin_extend of string
    | Exn of Exn_with_backtrace.t

  let empty = Initial

  let combine x y =
    match x, y with
    | Initial, _ -> y (* [Initial] cedes to any *)
    | _, Initial -> x
    | Exn _, _ -> x (* [Exn] takes over any *)
    | _, Exn _ -> y
    | Need_merlin_extend _, Need_merlin_extend _ -> y
  ;;
end

module Code_action_error_monoid = struct
  type t = Code_action_error.t

  include Monoid.Make (Code_action_error)
end

let client_can_resolve_edits (state : State.t) =
  let capabilities = State.client_capabilities state in
  Capabilities.code_action_data_support capabilities
  &&
  match Capabilities.code_action_resolve_properties capabilities with
  | Some properties -> List.mem properties "edit" ~equal:String.equal
  | None -> false
;;

let compute_ocaml_code_actions (params : CodeActionParams.t) state doc =
  let destruct_dispatch = Document.merlin_exn doc |> Action_destruct.cached_dispatch in
  let enabled_actions =
    List.filter
      ~f:(fun (_, (action : Code_action.t)) ->
        Lsp.Code_action.kind_is_requested params.context.only action.kind)
      [ "destruct-line", Action_destruct_line.t ~dispatch:destruct_dispatch state
      ; "destruct", Action_destruct.t ~dispatch:destruct_dispatch state
      ; "update-signature", Action_update_signature.t state
      ; "combine-cases", Action_combine_cases.t
      ; "inferred-interface", Action_inferred_intf.t state
      ; "type-annotate", Action_type_annotate.t
      ; "remove-type-annotation", Action_remove_type_annotation.t
      ; "construct", Action_construct.t
      ; "refactor-open-unqualify", Action_refactor_open.unqualify
      ; "refactor-open-qualify", Action_refactor_open.qualify
      ; "add-rec", Action_add_rec.t
      ; "mark-unused", Action_mark_remove_unused.mark
      ; "remove-unused", Action_mark_remove_unused.remove
      ; ( "inline"
        , if client_can_resolve_edits state
          then Action_inline.unresolved
          else Action_inline.t )
      ; "extract-local", Action_extract.local
      ; "extract-function", Action_extract.function_
      ]
  in
  let batchable, non_batchable =
    List.partition_map enabled_actions ~f:(fun (id, ca) ->
      match ca.run with
      | `Batchable f -> Either.First (id, f)
      | `Non_batchable f -> Second (id, f))
  in
  let* batch_results =
    if List.is_empty batchable
    then Fiber.return []
    else
      Document.Merlin.with_pipeline_exn
        ~name:"batched-code-actions"
        (Document.merlin_exn doc)
        (fun pipeline ->
           List.filter_map batchable ~f:(fun (id, code_action) ->
             try
               Option.map (code_action pipeline doc params) ~f:(fun action -> id, action)
             with
             | Merlin_extend.Extend_main.Handshake.Error _ -> None))
  in
  let code_action (id, code_action) =
    let+ res =
      Fiber.map_reduce_errors
        ~on_error:(fun (exn : Exn_with_backtrace.t) ->
          match exn.exn with
          | Merlin_extend.Extend_main.Handshake.Error error ->
            Fiber.return (Code_action_error.Need_merlin_extend error)
          | _ -> Fiber.return (Code_action_error.Exn exn))
        (module Code_action_error_monoid)
        (fun () -> code_action doc params)
    in
    match res with
    | Ok res -> Option.map res ~f:(fun action -> id, action)
    | Error Initial -> assert false
    | Error (Need_merlin_extend _) -> None
    | Error (Exn exn) -> Exn_with_backtrace.reraise exn
  in
  let+ non_batch_results =
    Fiber.parallel_map non_batchable ~f:code_action |> Fiber.map ~f:List.filter_opt
  in
  batch_results @ non_batch_results
;;

let filter_diagnostics configuration (params : CodeActionParams.t) =
  let mode_label = Merlin_config.configuration_label configuration in
  let mode_key =
    Merlin_config.configuration_mode configuration |> Option.map ~f:Merlin_config.mode_key
  in
  let diagnostics =
    List.filter params.context.diagnostics ~f:(fun diagnostic ->
      match Diagnostics.Provenance.classify diagnostic with
      | `External -> true
      | `Modes modes ->
        Option.exists mode_key ~f:(fun mode -> List.mem modes mode ~equal:String.equal)
      | `Malformed ->
        (match configuration.Merlin_config.origin with
         | Legacy_file -> true
         | Plural _ ->
           Log.log ~section:"code-actions" (fun () ->
             Log.msg
               "Dropping ocaml-lsp diagnostic with malformed mode provenance"
               [ "mode", `String mode_label ]);
           false))
  in
  { params with context = { params.context with diagnostics } }
;;

let reraise_cancellation errors =
  List.find errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
    match exn with
    | Jsonrpc.Response.Error.E { code = RequestCancelled; _ } -> true
    | _ -> false)
  |> Option.iter ~f:Exn_with_backtrace.reraise
;;

let ranges_overlap (left : Range.t) (right : Range.t) =
  let compare = Lsp.Position.compare in
  compare left.start right.end_ < 0 && compare right.start left.end_ < 0
;;

let edits_do_not_overlap edits =
  let ranges =
    List.map edits ~f:(function
      | `TextEdit edit -> edit.TextEdit.range
      | `AnnotatedTextEdit edit -> edit.Lsp.Types.AnnotatedTextEdit.range
      | `SnippetTextEdit edit -> edit.Lsp.Types.SnippetTextEdit.range)
  in
  let rec loop = function
    | [] -> true
    | range :: rest -> (not (List.exists rest ~f:(ranges_overlap range))) && loop rest
  in
  loop ranges
;;

let canonical_workspace_edit (edit : WorkspaceEdit.t) =
  let changes =
    Option.map edit.changes ~f:(fun changes ->
      List.map changes ~f:(fun (uri, edits) -> Source_path.uri uri, edits))
  in
  let documentChanges =
    Option.map edit.documentChanges ~f:(fun changes ->
      List.map changes ~f:(function
        | `TextDocumentEdit edit ->
          let textDocument =
            { edit.TextDocumentEdit.textDocument with
              uri = Source_path.uri edit.textDocument.uri
            }
          in
          `TextDocumentEdit { edit with textDocument }
        | `CreateFile file ->
          `CreateFile { file with CreateFile.uri = Source_path.uri file.uri }
        | `RenameFile file ->
          `RenameFile
            { file with
              Lsp.Types.RenameFile.oldUri = Source_path.uri file.oldUri
            ; newUri = Source_path.uri file.newUri
            }
        | `DeleteFile file ->
          `DeleteFile { file with Lsp.Types.DeleteFile.uri = Source_path.uri file.uri }))
  in
  let valid_changes =
    Option.for_all changes ~f:(fun changes ->
      List.for_all changes ~f:(fun (_, edits) ->
        edits |> List.map ~f:(fun edit -> `TextEdit edit) |> edits_do_not_overlap))
  in
  let valid_document_changes =
    Option.for_all documentChanges ~f:(fun changes ->
      List.for_all changes ~f:(function
        | `TextDocumentEdit edit -> edits_do_not_overlap edit.edits
        | `CreateFile _ | `RenameFile _ | `DeleteFile _ -> true))
  in
  if valid_changes && valid_document_changes
  then Some { edit with changes; documentChanges }
  else None
;;

let canonical_code_action (action : CodeAction.t) =
  match action.edit with
  | None -> Some action
  | Some edit ->
    Option.map (canonical_workspace_edit edit) ~f:(fun edit ->
      { action with edit = Some edit })
;;

let union_diagnostics actions =
  List.concat_map actions ~f:(fun (action : CodeAction.t) ->
    Option.value action.diagnostics ~default:[])
  |> List.fold_left ~init:[] ~f:(fun diagnostics diagnostic ->
    if List.exists diagnostics ~f:(Poly.equal diagnostic)
    then diagnostics
    else diagnostic :: diagnostics)
  |> List.rev
;;

let consensus_action actions =
  let actions = List.map actions ~f:canonical_code_action in
  if List.exists actions ~f:Option.is_none
  then None
  else (
    let actions = List.filter_opt actions in
    match actions with
    | [] -> None
    | first :: rest ->
      if
        Option.is_some first.disabled
        || List.exists rest ~f:(fun action -> Option.is_some action.disabled)
        || not
             (List.for_all rest ~f:(fun action ->
                String.equal action.title first.title
                && Poly.equal action.kind first.kind
                && Poly.equal action.edit first.edit
                && Poly.equal action.command first.command
                && Poly.equal action.data first.data
                && Poly.equal action.tags first.tags))
      then None
      else (
        let diagnostics =
          match union_diagnostics actions with
          | [] -> None
          | diagnostics -> Some diagnostics
        in
        let isPreferred =
          match actions with
          | [ action ] -> action.isPreferred
          | _ ->
            Option.some_if
              (List.for_all actions ~f:(fun action ->
                 Poly.equal action.isPreferred (Some true)))
              true
        in
        Some { first with diagnostics; isPreferred }))
;;

let consensus_actions configured =
  match configured with
  | [] -> []
  | (_, primary) :: rest ->
    List.filter_map primary ~f:(fun (provider, action) ->
      let matches =
        List.map rest ~f:(fun (_, actions) ->
          List.Assoc.find actions provider ~equal:String.equal)
      in
      if List.exists matches ~f:Option.is_none
      then None
      else consensus_action (action :: List.filter_opt matches))
;;

let compute_configured_code_actions params state doc =
  let merlin = Document.merlin_exn doc in
  let* { Document.Merlin.configurations; _ } =
    Document.Merlin.configuration_context_exn merlin
  in
  let rec loop results = function
    | [] -> Fiber.return (List.rev results)
    | configuration :: rest ->
      let configured_doc = Document.with_merlin_configuration doc configuration in
      let params = filter_diagnostics configuration params in
      let* result =
        Fiber.collect_errors (fun () ->
          compute_ocaml_code_actions params state configured_doc)
      in
      let actions =
        match result with
        | Ok actions -> actions
        | Error errors ->
          reraise_cancellation errors;
          List.iter errors ~f:(fun error ->
            Log.log ~section:"code-actions" (fun () ->
              Log.msg
                "Configured code action computation failed"
                [ "mode", `String (Merlin_config.configuration_label configuration)
                ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
                ]));
          []
      in
      loop ((configuration, actions) :: results) rest
  in
  let+ configured = loop [] (Merlin_config.configuration_list configurations) in
  consensus_actions configured
;;

let compute server (params : CodeActionParams.t) =
  let state : State.t = Server.state server in
  let uri = params.textDocument.uri in
  let doc =
    let store = state.store in
    Document_store.get_opt store uri
  in
  let kind_is_requested = Lsp.Code_action.kind_is_requested params.context.only in
  let dune_actions =
    if kind_is_requested CodeActionKind.QuickFix
    then Dune.code_actions (State.dune state) params.textDocument.uri
    else []
  in
  let actions xs =
    let xs =
      match params.context.only with
      | None -> xs
      | Some _ ->
        List.filter xs ~f:(fun (action : CodeAction.t) ->
          Option.exists action.kind ~f:kind_is_requested)
    in
    match xs with
    | [] -> None
    | xs -> Some (List.map ~f:(fun a -> `CodeAction a) xs)
  in
  match doc with
  | None -> Fiber.return (Reply.now (actions dune_actions), state)
  | Some doc ->
    let client_capabilities = State.client_capabilities state in
    let capabilities = Capabilities.show_document client_capabilities in
    let* open_related =
      if kind_is_requested Action_open_related.kind
      then (
        let can_create_file =
          Capabilities.workspace_edit_resource_operation
            client_capabilities
            ~operation:ResourceOperationKind.Create
        in
        Action_open_related.for_uri ~can_create_file capabilities doc)
      else Fiber.return []
    in
    let open_dune =
      if kind_is_requested Action_open_dune.kind
      then Action_open_dune.for_uri capabilities uri
      else []
    in
    (match Document.syntax doc with
     | Ocamllex | Menhir | Cram | Dune ->
       Fiber.return (Reply.now (actions (dune_actions @ open_related @ open_dune)), state)
     | Ocaml | Reason | Mlx ->
       let* merlin_jumps =
         match state.configuration.data.merlin_jump_code_actions with
         | Some { enable = true } -> Action_jump.code_actions doc params capabilities
         | Some { enable = false } | None -> Fiber.return []
       in
       let reply () =
         let+ code_action_results = compute_configured_code_actions params state doc in
         List.concat
           [ code_action_results; dune_actions; open_related; open_dune; merlin_jumps ]
         |> actions
       in
       let later f =
         Fiber.return
           ( Reply.later (fun k ->
               let* resp = f () in
               k resp)
           , state )
       in
       later reply)
;;

let resolve state action =
  match Action_inline.resolve state action with
  | Some resolved -> resolved
  | None -> Fiber.return action
;;
