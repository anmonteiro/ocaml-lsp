open Import
module Version = Version
module Diagnostics = Diagnostics
module Position = Position
module Doc_to_md = Doc_to_md
module Diff = Diff

module For_tests = struct
  module Dune = Dune.For_tests
end

module Testing = Testing
open Fiber.O

let make_error = Jsonrpc.Response.Error.make
let view_metrics_command_name = "ocamllsp/view-metrics"

let view_metrics server =
  let* json = Metrics.dump () in
  let+ () =
    Client.show_document_contents server ~prefix:"lsp-metrics" ~suffix:".json" json
  in
  `Null
;;

let initialize_info (client_capabilities : ClientCapabilities.t) : InitializeResult.t =
  let codeActionProvider =
    match Capabilities.code_action_literal_support client_capabilities with
    | Some _ ->
      let codeActionKinds =
        [ Action_destruct_line.kind
        ; Action_destruct.kind
        ; Action_update_signature.kind
        ; Action_combine_cases.kind
        ; Action_inferred_intf.kind
        ; CodeActionKind.Other "switch"
        ; Action_open_dune.kind
        ; CodeActionKind.QuickFix
        ; CodeActionKind.RefactorExtract
        ]
        @ Action_jump.kinds
        @ List.map
            ~f:(fun (c : Code_action.t) -> c.kind)
            [ Action_type_annotate.t
            ; Action_remove_type_annotation.t
            ; Action_construct.t
            ; Action_refactor_open.unqualify
            ; Action_refactor_open.qualify
            ; Action_inline.t
            ]
        |> List.dedup_and_sort ~compare:Poly.compare
      in
      `CodeActionOptions
        (CodeActionOptions.create ~codeActionKinds ~resolveProvider:true ())
    | _ -> `Bool true
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Incremental
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let codeLensProvider = CodeLensOptions.create ~resolveProvider:false () in
  let completionProvider =
    CompletionOptions.create ~triggerCharacters:[ "."; "#" ] ~resolveProvider:true ()
  in
  let signatureHelpProvider =
    SignatureHelpOptions.create ~triggerCharacters:[ " "; "~"; "?"; ":"; "(" ] ()
  in
  let renameProvider = `RenameOptions (RenameOptions.create ~prepareProvider:true ()) in
  let workspace =
    let workspaceFolders =
      WorkspaceFoldersServerCapabilities.create
        ~supported:true
        ~changeNotifications:(`Bool true)
        ()
    in
    WorkspaceOptions.create ~workspaceFolders ()
  in
  let capabilities =
    let experimental =
      `Assoc
        [ ( "ocamllsp"
          , `Assoc
              [ "interfaceSpecificLangId", `Bool true
              ; Req_switch_impl_intf.capability
              ; Req_infer_intf.capability
              ; Req_typed_holes.capability
              ; Req_jump_to_typed_hole.capability
              ; Req_wrapping_ast_node.capability
              ; Dune.view_promotion_capability
              ; Req_hover_extended.capability
              ; Req_merlin_call_compatible.capability
              ; Req_merlin_configurations.capability
              ; Req_type_enclosing.capability
              ; Req_get_documentation.capability
              ; Req_construct.capability
              ; Req_type_search.capability
              ; Req_merlin_jump.capability
              ; Req_phrase.capability
              ; Req_type_expression.capability
              ; Req_locate.capability
              ; Req_destruct.capability
              ; Req_locate_types.capability
              ; Req_refactor_extract.capability
              ] )
        ]
    in
    let executeCommandProvider =
      let commands =
        if Action_open_related.available (Capabilities.show_document client_capabilities)
        then
          view_metrics_command_name
          :: Action_open_related.command_name
          :: Action_open_dune.command_name
          :: Action_jump.command_name
          :: Document_text_command.command_name
          :: Merlin_config_command.command_name
          :: Dune.commands
        else Dune.commands
      in
      ExecuteCommandOptions.create ~commands ()
    in
    let semanticTokensProvider =
      let open Option.O in
      let* semantic_tokens = Capabilities.semantic_tokens client_capabilities in
      let supports_relative =
        Capabilities.supported semantic_tokens.formats ~tag:Relative ~equal:Poly.equal
      in
      let* full_request = semantic_tokens.requests.full in
      let* full =
        match supports_relative, full_request with
        | false, _ | true, `Bool false -> None
        | true, `Bool true -> Some (`Bool true)
        | true, `ClientSemanticTokensRequestFullDelta { delta } ->
          Some
            (match delta with
             | None | Some false -> `Bool true
             | Some true ->
               `SemanticTokensFullDelta (SemanticTokensFullDelta.create ~delta:true ()))
      in
      let config = Semantic_highlighting.create_config semantic_tokens in
      Some
        (`SemanticTokensOptions
            (SemanticTokensOptions.create
               ~legend:(Semantic_highlighting.legend config)
               ~full
               ()))
    in
    let positionEncoding =
      let open Option.O in
      let* options = Capabilities.position_encodings client_capabilities in
      List.find_map
        ([ UTF8; UTF16 ] : PositionEncodingKind.t list)
        ~f:(fun encoding ->
          Option.some_if
            (Capabilities.supported options ~tag:encoding ~equal:Poly.equal)
            encoding)
    in
    ServerCapabilities.create
      ~textDocumentSync
      ~hoverProvider:(`Bool true)
      ~declarationProvider:(`Bool true)
      ~definitionProvider:(`Bool true)
      ~typeDefinitionProvider:(`Bool true)
      ~completionProvider
      ~signatureHelpProvider
      ~codeActionProvider
      ~codeLensProvider
      ~referencesProvider:(`Bool true)
      ~documentHighlightProvider:(`Bool true)
      ~documentFormattingProvider:(`Bool true)
      ~documentRangeFormattingProvider:(`Bool true)
      ~documentOnTypeFormattingProvider:
        (DocumentOnTypeFormattingOptions.create ~firstTriggerCharacter:"\n" ())
      ~selectionRangeProvider:(`Bool true)
      ~documentSymbolProvider:(`Bool true)
      ~workspaceSymbolProvider:(`Bool true)
      ~foldingRangeProvider:(`Bool true)
      ?semanticTokensProvider
      ~experimental
      ~renameProvider
      ~inlayHintProvider:(`Bool true)
      ~workspace
      ~executeCommandProvider
      ?positionEncoding
      ()
  in
  let serverInfo =
    let version = Version.get () in
    ServerInfo.create ~name:"ocamllsp" ~version ()
  in
  InitializeResult.create ~capabilities ~serverInfo ()
;;

let ocamlmerlin_reason = "ocamlmerlin-reason"

let set_diagnostics detached diagnostics doc =
  let uri = Document.uri doc in
  match Document.kind doc with
  | `Other -> Fiber.return ()
  | `Merlin merlin ->
    let generation = Diagnostics.begin_merlin_generation diagnostics uri in
    let async send =
      let+ () =
        task_if_running detached ~f:(fun () ->
          let timer = Document.Merlin.timer merlin in
          let* () = Lev_fiber.Timer.Wheel.cancel timer in
          let* () = Lev_fiber.Timer.Wheel.reset timer in
          let* res = Lev_fiber.Timer.Wheel.await timer in
          match res with
          | `Cancelled -> Fiber.return ()
          | `Ok -> send ())
      in
      ()
    in
    (match Document.syntax doc with
     | Dune | Cram | Menhir | Ocamllex -> Fiber.return ()
     | Reason when Option.is_none (Bin.which ocamlmerlin_reason) ->
       let no_reason_merlin =
         let message =
           `String
             (sprintf "Could not detect %s. Please install reason" ocamlmerlin_reason)
         in
         Diagnostic.create
           ~source:Diagnostics.ocamllsp_source
           ~range:Range.first_line
           ~message
           ()
       in
       Diagnostics.set diagnostics (`Merlin (uri, [ no_reason_merlin ]));
       async (fun () -> Diagnostics.send diagnostics (`One uri))
     | Reason | Ocaml | Mlx ->
       async (fun () ->
         let* current = Diagnostics.merlin_diagnostics diagnostics merlin ~generation in
         if current then Diagnostics.send diagnostics (`One uri) else Fiber.return ()))
;;

let register_dune_and_cram_text_document_sync server (capabilities : ClientCapabilities.t)
  =
  if Capabilities.text_document_sync_dynamic_registration capabilities
  then (
    let documentSelector =
      [ "cram"; "dune"; "dune-project"; "dune-workspace" ]
      |> List.map ~f:(fun language ->
        `TextDocumentFilter
          (`TextDocumentFilterLanguage (TextDocumentFilterLanguage.create ~language ())))
    in
    let registerOptions =
      TextDocumentRegistrationOptions.create ~documentSelector ()
      |> TextDocumentRegistrationOptions.yojson_of_t
    in
    let make method_ =
      let id = "ocamllsp-cram-dune-files/" ^ method_ in
      Registration.create ~id ~method_ ~registerOptions ()
    in
    let registrations = [ make "textDocument/didOpen"; make "textDocument/didClose" ] in
    let params = RegistrationParams.create ~registrations in
    Server.request server (Server_request.ClientRegisterCapability params))
  else Fiber.return ()
;;

let on_initialize server (ip : InitializeParams.t) =
  let state : State.t = Server.state server in
  let workspaces = Workspaces.create ip in
  let diagnostics =
    let report_dune_diagnostics =
      Configuration.report_dune_diagnostics state.configuration
    in
    let shorten_merlin_diagnostics =
      Configuration.shorten_merlin_diagnostics state.configuration
    in
    Diagnostics.create
      ~report_dune_diagnostics
      ~shorten_merlin_diagnostics
      ip.capabilities
      (function
      | [] -> Fiber.return ()
      | diagnostics ->
        let state = Server.state server in
        task_if_running state.detached ~f:(fun () ->
          let batch = Server.Batch.create server in
          List.iter diagnostics ~f:(fun d ->
            Server.Batch.notification batch (PublishDiagnostics d));
          Server.Batch.submit batch))
  in
  let+ dune =
    let progress =
      Progress.create
        ip.capabilities
        ~report_progress:(fun progress ->
          Server.notification server (Server_notification.WorkDoneProgress progress))
        ~create_task:(fun task ->
          Server.request server (Server_request.WorkDoneProgressCreate task))
    in
    let dune =
      Dune.create
        workspaces
        ip.capabilities
        diagnostics
        progress
        state.store
        ~log:(State.log_msg server)
        ~trace:(State.log_trace server)
    in
    Fiber.return dune
  in
  let initialize_info = initialize_info ip.capabilities in
  let state =
    let position_encoding =
      match initialize_info.capabilities.positionEncoding with
      | None | Some UTF16 -> `UTF16
      | Some UTF8 -> `UTF8
      | Some UTF32 | Some (Other _) -> assert false
    in
    State.initialize state ~position_encoding ip workspaces dune diagnostics
  in
  let state =
    match ip.trace with
    | None -> state
    | Some trace -> { state with trace }
  in
  let response =
    Reply.later (fun send ->
      let* () = send initialize_info in
      task_if_running state.detached ~f:(fun () -> Dune.run dune))
  in
  response, state
;;

module Formatter = struct
  let jsonrpc_error (e : Ocamlformat.error) =
    let message = Ocamlformat.message e in
    let code : Jsonrpc.Response.Error.Code.t =
      match e with
      | Unsupported_syntax _ | Unknown_extension _ | Missing_binary _ -> InvalidRequest
      | Unexpected_result _ -> InternalError
    in
    make_error ~code ~message ()
  ;;

  let run_ocamlformat rpc format =
    let* res =
      let* cancel = Server.cancel_token () in
      format cancel
    in
    match res with
    | Ok result -> Fiber.return (Some result)
    | Error e ->
      let+ () =
        let state : State.t = Server.state rpc in
        let msg =
          let message = Ocamlformat.message e in
          ShowMessageParams.create ~message ~type_:Warning
        in
        task_if_running state.detached ~f:(fun () ->
          Server.notification rpc (ShowMessage msg))
      in
      Jsonrpc.Response.Error.raise (jsonrpc_error e)
  ;;

  let run_dune rpc doc =
    let state : State.t = Server.state rpc in
    match Dune.for_doc (State.dune state) doc with
    | [] ->
      let message =
        sprintf
          "No dune instance found. Please run dune in watch mode for %s"
          (Uri.to_path (Document.Dune.uri doc))
      in
      Jsonrpc.Response.Error.raise (make_error ~code:InvalidRequest ~message ())
    | dune :: rest ->
      let* () =
        match rest with
        | [] -> Fiber.return ()
        | _ :: _ ->
          let message =
            sprintf
              "More than one dune instance detected for %s. Selecting one at random"
              (Uri.to_path (Document.Dune.uri doc))
          in
          State.log_msg rpc ~type_:MessageType.Warning ~message
      in
      let+ to_ = Dune.Instance.format_dune_file dune doc in
      Some (Diff.edit ~from:(Document.Dune.text doc) ~to_)
  ;;

  let workspace_root rpc doc =
    let state : State.t = Server.state rpc in
    Workspaces.find_workspace_folder (State.workspaces state) (Document.uri doc)
    |> Option.map ~f:(fun (folder : WorkspaceFolder.t) -> folder.uri)
  ;;

  let run rpc doc =
    match Document.kind doc with
    | `Merlin merlin ->
      let workspace_root = workspace_root rpc doc in
      run_ocamlformat rpc (Ocamlformat.run ~workspace_root merlin)
    | `Other ->
      (match Document.dune doc with
       | None -> Fiber.return None
       | Some dune -> run_dune rpc dune)
  ;;

  let run_on_range rpc doc range =
    match Document.syntax doc with
    | Dune | Cram -> Fiber.return None
    | Ocaml | Reason | Mlx | Ocamllex | Menhir ->
      let workspace_root = workspace_root rpc doc in
      Ocamlformat.run_on_range ~workspace_root doc range |> run_ocamlformat rpc
  ;;
end

let text_document_lens
      (state : State.t)
      { CodeLensParams.textDocument = { uri }; _ }
      ~for_nested_bindings
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin doc ->
    let* { Document.Merlin.configurations; kind } =
      Document.Merlin.configuration_context_exn doc
    in
    if kind = Intf
    then Fiber.return []
    else
      let+ configured =
        Document.Merlin.dispatch_all ~name:"outline" doc ~configurations Outline
      in
      let rec symbol_info_of_outline_item (item : Query_protocol.item) =
        let children =
          if for_nested_bindings
          then List.concat_map item.children ~f:symbol_info_of_outline_item
          else []
        in
        match item.outline_type with
        | None -> children
        | Some typ ->
          let loc = item.location in
          let info =
            let range = Range.of_loc loc in
            let command = Command.create ~title:typ ~command:"" () in
            CodeLens.create ~range ~command ()
          in
          info :: children
      in
      let lenses, failures, successes =
        Merlin_dot_protocol.Nonempty_list.to_list configured
        |> List.fold_left
             ~init:([], [], 0)
             ~f:
               (fun
                 (lenses, failures, successes)
                 ({ configuration; result } : _ Document.Merlin.configured_result)
               ->
               match result with
               | Error error -> lenses, (configuration, error) :: failures, successes
               | Ok outline ->
                 let found = List.concat_map ~f:symbol_info_of_outline_item outline in
                 ( List.rev_append
                     (List.map found ~f:(fun lens -> configuration, lens))
                     lenses
                 , failures
                 , successes + 1 ))
      in
      List.iter failures ~f:(fun (configuration, error) ->
        Log.log ~section:"merlin" (fun () ->
          Log.msg
            "Merlin code lens configuration failed"
            [ "mode", `String (Merlin_config.configuration_label configuration)
            ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
            ]));
      if successes = 0
      then (
        let primary = Merlin_config.primary configurations in
        match
          List.find failures ~f:(fun (configuration, _) -> configuration == primary)
        with
        | Some (_, error) -> Exn_with_backtrace.reraise error
        | None -> invalid_arg "configured code lens query had no result");
      let rec add_lens configuration lens = function
        | [] -> [ lens, [ configuration ] ]
        | (candidate, contributors) :: rest ->
          if Poly.equal lens candidate
          then (
            let contributors =
              if
                List.exists contributors ~f:(fun contributor ->
                  contributor == configuration)
              then contributors
              else configuration :: contributors
            in
            (candidate, contributors) :: rest)
          else (candidate, contributors) :: add_lens configuration lens rest
      in
      let groups =
        List.fold_left (List.rev lenses) ~init:[] ~f:(fun groups (configuration, lens) ->
          add_lens configuration lens groups)
      in
      let all_configurations = Merlin_config.configuration_list configurations in
      List.map groups ~f:(fun (lens, contributors) ->
        if List.length contributors = List.length all_configurations
        then lens
        else (
          let labels =
            List.filter all_configurations ~f:(fun configuration ->
              List.exists contributors ~f:(fun contributor ->
                contributor == configuration))
            |> List.map ~f:Merlin_config.configuration_label
            |> String.concat ~sep:", "
          in
          let command =
            Option.map lens.CodeLens.command ~f:(fun command ->
              { command with title = command.title ^ " (" ^ labels ^ ")" })
          in
          { lens with command }))
;;

let selection_range
      (state : State.t)
      { SelectionRangeParams.textDocument = { uri }; positions; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return []
  | `Merlin merlin ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    (* TODO: Convert selection-range inputs and outputs using the negotiated
       position encoding instead of Merlin's UTF-8 byte columns. *)
    let source = Document.Merlin.source merlin in
    let ranges_for_position position enclosings =
      List.filter_map enclosings ~f:(fun enclosing ->
        let range = Range.clamp_to_source (Range.of_loc enclosing) source in
        Option.some_if (Range.contains_position range position ~inclusive_end:true) range)
    in
    let selection_range_of_ranges position ranges =
      List.rev ranges
      |> List.fold_left ~init:None ~f:(fun parent range ->
        Some { SelectionRange.range; parent })
      |> Option.value
           ~default:
             { SelectionRange.range = { start = position; end_ = position }
             ; parent = None
             }
    in
    let+ configured =
      Document.Merlin.with_configurations
        ~name:"shape"
        merlin
        ~configurations
        (fun _ pipeline ->
           List.map positions ~f:(fun position ->
             Query_commands.dispatch
               pipeline
               (Enclosing (Position.logical position, None))))
    in
    let failures =
      Merlin_dot_protocol.Nonempty_list.to_list configured
      |> List.filter_map
           ~f:(fun ({ configuration; result } : _ Document.Merlin.configured_result) ->
             Result.error result |> Option.map ~f:(fun error -> configuration, error))
    in
    if not (List.is_empty failures)
    then (
      List.iter failures ~f:(fun (configuration, error) ->
        Log.log ~section:"merlin" (fun () ->
          Log.msg
            "Merlin selection range configuration failed"
            [ "mode", `String (Merlin_config.configuration_label configuration)
            ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
            ]));
      let modes =
        List.map failures ~f:(fun (configuration, _) ->
          Merlin_config.configuration_label configuration)
        |> String.concat ~sep:", "
      in
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:RequestFailed
           ~message:("Selection range failed for configurations: " ^ modes)
           ()));
    let all_enclosings =
      Merlin_dot_protocol.Nonempty_list.to_list configured
      |> List.map ~f:(fun ({ result; _ } : _ Document.Merlin.configured_result) ->
        match result with
        | Ok value -> value
        | Error _ -> invalid_arg "failed selection range survived validation")
    in
    List.mapi positions ~f:(fun index position ->
      let chains =
        List.map all_enclosings ~f:(fun per_position ->
          List.nth_exn per_position index |> ranges_for_position position)
      in
      let primary_chain = List.hd_exn chains in
      let common =
        List.filter primary_chain ~f:(fun range ->
          List.for_all (List.tl_exn chains) ~f:(fun chain ->
            List.exists chain ~f:(Poly.equal range)))
      in
      selection_range_of_ranges position common)
;;

let references
      rpc
      (state : State.t)
      { ReferenceParams.textDocument = { uri }; position; context; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn doc
    in
    let* configured =
      Document.Merlin.dispatch_all
        ~name:"occurrences"
        doc
        ~configurations
        (Occurrences (`Ident_at (Position.logical position), `Project))
    in
    let locations, out_of_sync, failures, successes =
      Merlin_dot_protocol.Nonempty_list.to_list configured
      |> List.fold_left
           ~init:([], [], [], 0)
           ~f:
             (fun
               (locations, out_of_sync, failures, successes)
               ({ configuration; result } : _ Document.Merlin.configured_result)
             ->
             match result with
             | Error error ->
               locations, out_of_sync, (configuration, error) :: failures, successes
             | Ok (occurrences, synced) ->
               let out_of_sync =
                 match synced with
                 | `Out_of_sync _ -> configuration :: out_of_sync
                 | _ -> out_of_sync
               in
               let found =
                 List.filter_map
                   occurrences
                   ~f:(fun ({ loc; is_stale } : Query_protocol.occurrence) ->
                     if is_stale
                     then None
                     else (
                       let range = Range.of_loc loc in
                       let target_uri =
                         match loc.loc_start.pos_fname with
                         | "" -> uri
                         | path -> Source_path.of_path path
                       in
                       Some (Source_path.location { Location.uri = target_uri; range })))
               in
               List.rev_append found locations, out_of_sync, failures, successes + 1)
    in
    List.iter failures ~f:(fun (configuration, error) ->
      Log.log ~section:"merlin" (fun () ->
        Log.msg
          "Merlin occurrences configuration failed"
          [ "mode", `String (Merlin_config.configuration_label configuration)
          ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
          ]));
    if successes = 0
    then (
      let primary = Merlin_config.primary configurations in
      let _, error =
        List.find failures ~f:(fun (configuration, _) -> configuration == primary)
        |> Option.value ~default:(List.hd_exn failures)
      in
      Exn_with_backtrace.reraise error);
    let* declarations =
      if context.includeDeclaration
      then Fiber.return []
      else (
        let* configured =
          Document.Merlin.dispatch_all
            ~name:"reference-declaration"
            doc
            ~configurations
            (Locate (None, `ML, Position.logical position))
        in
        let declarations, failures, successes =
          Merlin_dot_protocol.Nonempty_list.to_list configured
          |> List.fold_left
               ~init:([], [], 0)
               ~f:
                 (fun
                   (declarations, failures, successes)
                   ({ configuration; result } : _ Document.Merlin.configured_result)
                 ->
                 match result with
                 | Error error ->
                   declarations, (configuration, error) :: failures, successes
                 | Ok result ->
                   let declaration =
                     match result with
                     | `At_origin -> Some (`At_origin (Source_path.uri uri, position))
                     | `Found (path, lexical_position) ->
                       Position.of_lexical_position lexical_position
                       |> Option.map ~f:(fun position ->
                         let uri =
                           Option.value_map
                             path
                             ~default:(Source_path.uri uri)
                             ~f:Source_path.of_path
                         in
                         `Found (uri, position))
                     | `Builtin _
                     | `File_not_found _
                     | `Invalid_context
                     | `Not_found _
                     | `Not_in_env _ -> None
                   in
                   Option.to_list declaration @ declarations, failures, successes + 1)
        in
        List.iter failures ~f:(fun (configuration, error) ->
          Log.log ~section:"merlin" (fun () ->
            Log.msg
              "Merlin reference declaration configuration failed"
              [ "mode", `String (Merlin_config.configuration_label configuration)
              ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
              ]));
        if successes = 0
        then (
          let primary = Merlin_config.primary configurations in
          let _, error =
            List.find failures ~f:(fun (configuration, _) -> configuration == primary)
            |> Option.value ~default:(List.hd_exn failures)
          in
          Exn_with_backtrace.reraise error);
        Fiber.return declarations)
    in
    let+ () =
      match out_of_sync with
      | _ :: _ ->
        let msg =
          let message =
            "The index might be out-of-sync.  If you use Dune you can build the target \
             `@ocaml-index` to refresh the index. Affected configurations: "
            ^ String.concat
                ~sep:", "
                (List.rev_map out_of_sync ~f:Merlin_config.configuration_label)
          in
          ShowMessageParams.create ~message ~type_:Warning
        in
        task_if_running state.detached ~f:(fun () ->
          Server.notification rpc (ShowMessage msg))
      | [] -> Fiber.return ()
    in
    let is_declaration ({ Location.uri; range } : Location.t) =
      List.exists declarations ~f:(function
        | `At_origin (declaration_uri, position) ->
          Uri.equal uri declaration_uri
          && Range.contains_position range position ~inclusive_end:true
        | `Found (declaration_uri, position) ->
          Uri.equal uri declaration_uri && Position.compare range.start position = 0)
    in
    List.rev locations
    |> List.filter ~f:(Fn.non is_declaration)
    |> Source_path.deduplicate_locations
    |> Option.some
;;

let highlight
      (state : State.t)
      { DocumentHighlightParams.textDocument = { uri }; position; _ }
  =
  let store = state.store in
  let doc = Document_store.get store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin m ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn m
    in
    let+ configured =
      Document.Merlin.dispatch_all
        ~name:"occurrences"
        m
        ~configurations
        (Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let lsp_locs, failures, successes =
      Merlin_dot_protocol.Nonempty_list.to_list configured
      |> List.fold_left
           ~init:([], [], 0)
           ~f:
             (fun
               (locations, failures, successes)
               ({ configuration; result } : _ Document.Merlin.configured_result)
             ->
             match result with
             | Error error -> locations, (configuration, error) :: failures, successes
             | Ok (occurrences, _synced) ->
               let found =
                 List.filter_map
                   occurrences
                   ~f:(fun (occurrence : Query_protocol.occurrence) ->
                     let range = Range.of_loc occurrence.loc in
                     if Lsp.Range.is_single_line range
                     then
                       Some
                         (DocumentHighlight.create
                            ~range
                            ~kind:DocumentHighlightKind.Text
                            ())
                     else None)
               in
               List.rev_append found locations, failures, successes + 1)
    in
    List.iter failures ~f:(fun (configuration, error) ->
      Log.log ~section:"merlin" (fun () ->
        Log.msg
          "Merlin highlight configuration failed"
          [ "mode", `String (Merlin_config.configuration_label configuration)
          ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
          ]));
    if successes = 0
    then (
      let primary = Merlin_config.primary configurations in
      let _, error =
        List.find failures ~f:(fun (configuration, _) -> configuration == primary)
        |> Option.value ~default:(List.hd_exn failures)
      in
      Exn_with_backtrace.reraise error);
    Some (List.dedup_and_sort ~compare:Poly.compare lsp_locs)
;;

let document_symbol (state : State.t) uri =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let client_capabilities = State.client_capabilities state in
  Document_symbol.run client_capabilities doc uri
;;

let on_request
  : type resp.
    State.t Server.t -> resp Client_request.t -> (resp Reply.t * State.t) Fiber.t
  =
  fun server req ->
  let rpc = server in
  let state : State.t = Server.state server in
  let store = state.store in
  let now res = Fiber.return (Reply.now res, state) in
  let later f req =
    Fiber.return
      ( Reply.later (fun k ->
          let* resp = f state req in
          k resp)
      , state )
  in
  match req with
  | Client_request.UnknownRequest { meth; params } ->
    (match
       List.Assoc.find
         [ Req_switch_impl_intf.meth, Req_switch_impl_intf.on_request
         ; Req_infer_intf.meth, Req_infer_intf.on_request
         ; Req_typed_holes.meth, Req_typed_holes.on_request
         ; Req_jump_to_typed_hole.meth, Req_jump_to_typed_hole.on_request
         ; Req_merlin_call_compatible.meth, Req_merlin_call_compatible.on_request
         ; Req_merlin_configurations.meth, Req_merlin_configurations.on_request
         ; Req_type_enclosing.meth, Req_type_enclosing.on_request
         ; Req_get_documentation.meth, Req_get_documentation.on_request
         ; Req_merlin_jump.meth, Req_merlin_jump.on_request
         ; Req_wrapping_ast_node.meth, Req_wrapping_ast_node.on_request
         ; Req_type_search.meth, Req_type_search.on_request
         ; Req_construct.meth, Req_construct.on_request
         ; ( Semantic_highlighting.Debug.meth_request_full
           , Semantic_highlighting.Debug.on_request_full )
         ; ( Req_hover_extended.meth
           , fun ~params _ -> Req_hover_extended.on_request ~params rpc )
         ; Req_phrase.meth, Req_phrase.on_request
         ; Req_type_expression.meth, Req_type_expression.on_request
         ; Req_locate.meth, Req_locate.on_request
         ; Req_destruct.meth, Req_destruct.on_request
         ; Req_locate_types.meth, Req_locate_types.on_request
         ; Req_refactor_extract.meth, Req_refactor_extract.on_request
         ]
         meth
         ~equal:String.equal
     with
     | None ->
       Jsonrpc.Response.Error.raise
         (make_error
            ~code:MethodNotFound
            ~message:"Unknown method"
            ~data:(`Assoc [ "method", `String meth ])
            ())
     | Some handler ->
       Fiber.return
         ( Reply.later (fun send ->
             let* res = handler ~params state in
             send res)
         , state ))
  | Initialize ip ->
    let+ res, state = on_initialize server ip in
    res, state
  | DebugTextDocumentGet { textDocument = { uri }; position = _ } ->
    (match Document_store.get_opt store uri with
     | None -> now None
     | Some doc -> now (Some (Msource.text (Document.source doc))))
  | DebugEcho params -> now params
  | Shutdown -> Fiber.return (Reply.now (), state)
  | WorkspaceSymbol req ->
    later
      (fun state () ->
         Workspace_symbol.run state req
         >>| Option.map ~f:(fun symbols -> `SymbolInformation symbols))
      ()
  | CodeActionResolve ca -> later (fun state () -> Code_actions.resolve state ca) ()
  | ExecuteCommand command ->
    if String.equal command.command Merlin_config_command.command_name
    then
      later
        (fun state server ->
           let store = state.store in
           let+ () = Merlin_config_command.command_run server store in
           `Null)
        server
    else if String.equal command.command Document_text_command.command_name
    then
      later
        (fun state server ->
           let store = state.store in
           let+ () = Document_text_command.command_run server store command.arguments in
           `Null)
        server
    else if String.equal command.command view_metrics_command_name
    then later (fun _state server -> view_metrics server) server
    else if String.equal command.command Action_open_related.command_name
    then
      later (fun _state server -> Action_open_related.command_run server command) server
    else if String.equal command.command Action_open_dune.command_name
    then later (fun _state server -> Action_open_dune.command_run server command) server
    else if String.equal command.command Action_jump.command_name
    then later (fun _state server -> Action_jump.command_run server command) server
    else
      later
        (fun state () ->
           let dune = State.dune state in
           Dune.on_command dune command)
        ()
  | CompletionItemResolve ci ->
    later
      (fun state () ->
         let markdown =
           Capabilities.supports_markdown
             (Capabilities.completion_documentation_format
                (State.client_capabilities state))
         in
         let resolve = Compl.Resolve.of_completion_item ci in
         match resolve with
         | None -> Fiber.return ci
         | Some resolve ->
           let doc =
             let uri = Compl.Resolve.uri resolve in
             Document_store.get state.store uri
           in
           (match Document.kind doc with
            | `Other -> Fiber.return ci
            | `Merlin doc -> Compl.resolve state doc ci resolve ~markdown))
      ()
  | CodeAction params -> Code_actions.compute server params
  | InlayHint params -> later (fun state () -> Inlay_hints.compute state params) ()
  | TextDocumentColor _ -> now []
  | TextDocumentColorPresentation _ -> now []
  | TextDocumentHover req ->
    (match state.configuration.data.standard_hover with
     | Some { enable = false } -> now None
     | Some { enable = true } | None ->
       let mode =
         match state.configuration.data.extended_hover with
         | Some { enable = true } -> Hover_req.Extended_variable
         | Some _ | None -> Hover_req.Default
       in
       later (fun (_ : State.t) () -> Hover_req.handle rpc req mode) ())
  | TextDocumentReferences req -> later (references rpc) req
  | TextDocumentCodeLensResolve codeLens -> now codeLens
  | TextDocumentCodeLens req ->
    (match state.configuration.data.codelens with
     | Some { enable = true; for_nested_bindings } ->
       later
         (fun state req ->
            let+ result = text_document_lens ~for_nested_bindings state req in
            Some result)
         req
     | _ -> now (Some []))
  | TextDocumentHighlight req -> later highlight req
  | DocumentSymbol { textDocument = { uri }; _ } -> later document_symbol uri
  | TextDocumentDeclaration { textDocument = { uri }; position; _ } ->
    later (fun state () -> Definition_query.run `Declaration state uri position) ()
  | TextDocumentDefinition { textDocument = { uri }; position; _ } ->
    later (fun state () -> Definition_query.run `Definition state uri position) ()
  | TextDocumentTypeDefinition { textDocument = { uri }; position; _ } ->
    later (fun state () -> Definition_query.run `Type_definition state uri position) ()
  | TextDocumentCompletion params -> later (fun _ () -> Compl.complete state params) ()
  | TextDocumentPrepareRename req ->
    later
      (fun state req ->
         let+ result = Rename.prepare state req in
         Option.map result ~f:(fun range -> `Range range))
      req
  | TextDocumentRename req ->
    later
      (fun state req ->
         let+ result = Rename.rename state req in
         Some result)
      req
  | TextDocumentFoldingRange req -> later Folding_range.compute req
  | SignatureHelp req ->
    later
      (fun state req ->
         let+ result = Signature_help.run state req in
         Some result)
      req
  | TextDocumentLinkResolve l -> now l
  | TextDocumentLink _ -> now None
  | WillSaveWaitUntilTextDocument _ -> now None
  | TextDocumentFormatting { textDocument = { uri }; options = _; _ } ->
    later
      (fun _ () ->
         let doc = Document_store.get store uri in
         Formatter.run rpc doc)
      ()
  | TextDocumentRangeFormatting { textDocument = { uri }; range; _ } ->
    later
      (fun _ () ->
         let doc = Document_store.get store uri in
         Formatter.run_on_range rpc doc range)
      ()
  | TextDocumentOnTypeFormatting { textDocument = { uri }; position; ch; _ } ->
    (match ch with
     | "\n" ->
       later
         (fun state () ->
            let doc = Document_store.get store uri in
            Ocp_indent.format_on_type state.ocp_indent doc position)
         ()
     | _ -> now (Some []))
  | SelectionRange req ->
    later
      (fun state req ->
         let+ result = selection_range state req in
         Some result)
      req
  | TextDocumentImplementation _ -> Server.not_supported ()
  | SemanticTokensFull p -> later Semantic_highlighting.on_request_full p
  | SemanticTokensDelta p -> later Semantic_highlighting.on_request_full_delta p
  | TextDocumentMoniker _ -> Server.not_supported ()
  | TextDocumentPrepareCallHierarchy _ -> Server.not_supported ()
  | CallHierarchyIncomingCalls _ -> Server.not_supported ()
  | CallHierarchyOutgoingCalls _ -> Server.not_supported ()
  | SemanticTokensRange _ -> Server.not_supported ()
  | LinkedEditingRange _ -> Server.not_supported ()
  | WillCreateFiles _ -> Server.not_supported ()
  | WillRenameFiles _ -> Server.not_supported ()
  | WillDeleteFiles _ -> Server.not_supported ()
  | InlayHintResolve _ -> Server.not_supported ()
  | TextDocumentDiagnostic _ -> Server.not_supported ()
  | TextDocumentInlineCompletion _ -> Server.not_supported ()
  | TextDocumentInlineValue _ -> Server.not_supported ()
  | WorkspaceSymbolResolve _ -> Server.not_supported ()
  | WorkspaceDiagnostic _ -> Server.not_supported ()
  | TextDocumentRangesFormatting _ -> Server.not_supported ()
  | TextDocumentPrepareTypeHierarchy _ -> Server.not_supported ()
  | TypeHierarchySupertypes _ -> Server.not_supported ()
  | TypeHierarchySubtypes _ -> Server.not_supported ()
  | WorkspaceTextDocumentContent _ -> Server.not_supported ()
;;

let on_notification server (notification : Client_notification.t) : State.t Fiber.t =
  let state : State.t = Server.state server in
  let store = state.store in
  match notification with
  | TextDocumentDidOpen params ->
    let* doc =
      let position_encoding = State.position_encoding state in
      Document.make
        ~position_encoding
        (State.wheel state)
        state.merlin_config
        state.merlin
        params
    in
    let* () = Document_store.open_document store doc in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | TextDocumentDidClose { textDocument = { uri } } ->
    let+ () =
      Diagnostics.remove (State.diagnostics state) (`Merlin uri);
      let* () = Document_store.close_document store uri in
      task_if_running state.detached ~f:(fun () ->
        Diagnostics.send (State.diagnostics state) (`One uri))
    in
    state
  | TextDocumentDidChange { textDocument = { uri; version }; contentChanges } ->
    let doc =
      Document_store.change_document store uri ~f:(fun prev_doc ->
        Document.update_text ~version prev_doc contentChanges)
    in
    let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
    state
  | CancelRequest _ -> Fiber.return state
  | ChangeConfiguration req ->
    let previous_shorten_merlin_diagnostics =
      Configuration.shorten_merlin_diagnostics state.configuration
    in
    let* configuration = Configuration.update state.configuration req in
    let diagnostics = State.diagnostics state in
    let* () =
      let report_dune_diagnostics = Configuration.report_dune_diagnostics configuration in
      Diagnostics.set_report_dune_diagnostics ~report_dune_diagnostics diagnostics
    in
    let shorten_merlin_diagnostics =
      Configuration.shorten_merlin_diagnostics configuration
    in
    let* () =
      Diagnostics.set_shorten_merlin_diagnostics ~shorten_merlin_diagnostics diagnostics
    in
    let+ () =
      if previous_shorten_merlin_diagnostics = shorten_merlin_diagnostics
      then Fiber.return ()
      else
        let* () =
          Document_store.parallel_iter store ~f:(fun doc ->
            match Document.kind doc, Document.syntax doc with
            | `Other, _ -> Fiber.return ()
            | `Merlin _, Reason when Option.is_none (Bin.which ocamlmerlin_reason) ->
              Fiber.return ()
            | `Merlin merlin, (Reason | Ocaml | Mlx) ->
              let uri = Document.Merlin.to_doc merlin |> Document.uri in
              let generation = Diagnostics.begin_merlin_generation diagnostics uri in
              let+ (_ : bool) =
                Diagnostics.merlin_diagnostics diagnostics merlin ~generation
              in
              ()
            | `Merlin _, (Dune | Cram | Menhir | Ocamllex) -> assert false)
        in
        Diagnostics.send diagnostics `All
    in
    { state with configuration }
  | DidSaveTextDocument { textDocument = { uri }; _ } ->
    let state = Server.state server in
    (match Document_store.get_opt state.store uri with
     | None ->
       (Log.log ~section:"on receive DidSaveTextDocument"
        @@ fun () -> Log.msg "saved document is not in the store" []);
       Fiber.return state
     | Some doc ->
       let+ () = set_diagnostics state.detached (State.diagnostics state) doc in
       state)
  | ChangeWorkspaceFolders change ->
    let state =
      State.modify_workspaces state ~f:(fun ws -> Workspaces.on_change ws change)
    in
    Dune.update_workspaces (State.dune state) (State.workspaces state);
    Fiber.return state
  | Initialized _ ->
    let+ () =
      task_if_running state.detached ~f:(fun () ->
        register_dune_and_cram_text_document_sync server (State.client_capabilities state))
    in
    state
  | DidChangeWatchedFiles _
  | DidCreateFiles _
  | DidDeleteFiles _
  | DidRenameFiles _
  | WillSaveTextDocument _
  | WorkDoneProgressCancel _
  | WorkDoneProgress _
  | NotebookDocumentDidOpen _
  | NotebookDocumentDidChange _
  | NotebookDocumentDidSave _
  | NotebookDocumentDidClose _
  | Exit -> Fiber.return state
  | SetTrace { value } -> Fiber.return { state with trace = value }
  | UnknownNotification req ->
    let+ () =
      State.log_msg server ~type_:Error ~message:("Unknown notication " ^ req.method_)
    in
    state
;;

let start stream =
  let detached = Fiber.Pool.create () in
  let server = Fdecl.create () in
  let store = Document_store.make server detached in
  let handler =
    let on_request = { Server.Handler.on_request } in
    Server.Handler.make ~on_request ~on_notification ()
  in
  let ocamlformat_rpc = Ocamlformat_rpc.create () in
  let ocp_indent = Ocp_indent.create () in
  let* configuration = Configuration.default () in
  let wheel = Configuration.wheel configuration in
  let* merlin = Lev_fiber.Thread.create () in
  let server =
    let symbols_thread = Lazy_fiber.create Lev_fiber.Thread.create in
    Fdecl.set
      server
      (Server.make
         handler
         stream
         (State.create
            ~store
            ~merlin
            ~ocamlformat_rpc
            ~ocp_indent
            ~configuration
            ~detached
            ~symbols_thread
            ~wheel
            ~trace:(fun ~message ~verbose ->
              State.log_trace (Fdecl.get server) ~message ~verbose)));
    Fdecl.get server
  in
  let state = Server.state server in
  let with_log_errors what f =
    let+ (_ : (unit, unit) result) =
      Fiber.map_reduce_errors
        (module Monoid.Unit)
        f
        ~on_error:(fun exn ->
          Format.eprintf "%s: %a@." what Exn_with_backtrace.pp_uncaught exn;
          Fiber.return ())
    in
    ()
  in
  let run_ocamlformat_rpc () =
    let* state = Ocamlformat_rpc.run ~logger:(State.log_msg server) ocamlformat_rpc in
    let message =
      match state with
      | Error `Binary_not_found ->
        Some
          "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be \
           well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 \
           or, otherwise, 'ocamlformat-rpc' package."
      | Error `Disabled | Ok () -> None
    in
    match message with
    | None -> Fiber.return ()
    | Some message ->
      let* (_ : InitializeParams.t) = Server.initialized server in
      let state = Server.state server in
      task_if_running state.detached ~f:(fun () ->
        let log = ShowMessageParams.create ~type_:Info ~message in
        Server.notification server (Server_notification.ShowMessage log))
  in
  let run () =
    let run_detached () =
      with_log_errors "detached" (fun () -> Fiber.Pool.run detached)
    in
    let cleanup () =
      let finalize =
        [ Document_store.close_all store
        ; Fiber.Pool.stop detached
        ; Ocamlformat_rpc.stop ocamlformat_rpc
        ; Ocp_indent.stop ocp_indent
        ; Lev_fiber.Timer.Wheel.stop wheel
        ; Merlin_config.DB.stop state.merlin_config
        ; Fiber.of_thunk (fun () ->
            Lev_fiber.Thread.close merlin;
            Fiber.return ())
        ]
      in
      let finalize =
        match (Server.state server).init with
        | Uninitialized -> finalize
        | Initialized init -> Dune.stop init.dune :: finalize
      in
      Fiber.all_concurrently_unit finalize
    in
    let serve () = Fiber.finalize (fun () -> Server.start server) ~finally:cleanup in
    let run_server () =
      Fiber.finalize
        (fun () -> Fiber.fork_and_join_unit run_detached serve)
        ~finally:(fun () -> Server.close server)
    in
    Fiber.all_concurrently_unit
      [ Lev_fiber.Timer.Wheel.run wheel
      ; with_log_errors "merlin" (fun () -> Merlin_config.DB.run state.merlin_config)
      ; run_server ()
      ; with_log_errors "ocamlformat-rpc" run_ocamlformat_rpc
      ]
  in
  let metrics = Metrics.create () in
  Metrics.with_metrics metrics run
;;

let socket sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let fd =
    Lev_fiber.Fd.create
      (Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0)
      (`Non_blocking false)
  in
  let* () = Lev_fiber.Socket.connect fd sockaddr in
  Lev_fiber.Io.create_rw fd
;;

let stream_of_channel : Lsp.Cli.Channel.t -> _ = function
  | Stdio ->
    let* stdin = Lev_fiber.Io.stdin in
    let+ stdout = Lev_fiber.Io.stdout in
    stdin, stdout
  | Pipe path ->
    if Sys.win32
    then (
      Format.eprintf "windows pipes are not supported";
      exit 1)
    else (
      let sockaddr = Unix.ADDR_UNIX path in
      socket sockaddr)
  | Socket port ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    socket sockaddr
;;

(* Merlin uses [Sys.command] to run preprocessors and ppxes. We provide an
   alternative version using the Spawn library for unixes.

   TODO: Currently PPX config is passed to Merlin in the form of a quoted shell
   command. The [prog_is_quoted] argument in Merlin's API is meant to allow
   supporting a way to launch ppx executables without using the shell.

   This will require additionnal changes of the API so there is no need to deal
   with the [prog_is_quoted] argument until this happen. *)
let run_in_directory ~prog ~prog_is_quoted:_ ~args ~cwd ?stdin ?stdout ?stderr () =
  (* Currently we assume that [prog] is always quoted and might contain
     arguments such as [-as-ppx]. This is due to the way Merlin gets its
     configuration. Thus we cannot rely on [Filename.quote_command]. *)
  let args = String.concat ~sep:" " @@ List.map ~f:Filename.quote args in
  let cmd = Format.sprintf "%s %s" prog args in
  let prog = "/bin/sh" in
  let argv = [ "sh"; "-c"; cmd ] in
  let stdin =
    match stdin with
    | Some file -> Unix.openfile file [ Unix.O_RDONLY ] 0o664
    | None -> Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0o777
  in
  let stdout, should_close_stdout =
    match stdout with
    | Some file -> Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664, true
    | None ->
      (* Runned programs should never output to stdout since it is the channel
         used by LSP to communicate with the editor *)
      Unix.stderr, false
  in
  let stderr =
    Option.map stderr ~f:(fun file ->
      Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT ] 0o664)
  in
  let pid =
    let cwd : Spawn.Working_dir.t = Path cwd in
    Spawn.spawn ~cwd ~prog ~argv ~stdin ~stdout ?stderr () |> Pid.of_int
  in
  let _, status = Unix.waitpid [] (Pid.to_int pid) in
  let res =
    match (status : Unix.process_status) with
    | WEXITED n -> n
    | WSIGNALED _ -> -1
    | WSTOPPED _ -> -1
  in
  Unix.close stdin;
  if should_close_stdout then Unix.close stdout;
  Option.iter stderr ~f:Unix.close;
  `Finished res
;;

let run_in_directory =
  (* Merlin has specific stubs for Windows, we reuse them *)
  let for_windows = !Merlin_utils.Std.System.run_in_directory in
  fun () -> if Sys.win32 then for_windows else run_in_directory
;;

let run channel ~prefer_dot_merlin () =
  Merlin_utils.Lib_config.set_program_name "ocamllsp";
  Merlin_utils.Lib_config.System.set_run_in_directory (run_in_directory ());
  Merlin_config.prefer_dot_merlin := prefer_dot_merlin;
  Unix.putenv "__MERLIN_MASTER_PID" (string_of_int (Unix.getpid ()));
  Lev_fiber.run ~sigpipe:`Ignore (fun () ->
    let* input, output = stream_of_channel channel in
    start (Lsp_fiber.Fiber_io.make input output))
  |> Lev_fiber.Error.ok_exn
;;

module Custom_request = Custom_request
