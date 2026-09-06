open Import
open Fiber.O

let completion_identity (item : CompletionItem.t) =
  { item with
    commitCharacters = None
  ; data = None
  ; deprecated = None
  ; detail = None
  ; documentation = None
  ; filterText = None
  ; labelDetails = None
  ; preselect = None
  ; sortText = None
  ; tags = None
  }
;;

module Resolve = struct
  type t =
    { params : CompletionParams.t
    ; version : int option
    ; identity : CompletionItem.t option
    }

  let uri t = t.params.textDocument.uri

  let yojson_of_t { params; version; identity } =
    `Assoc
      [ "params", CompletionParams.yojson_of_t params
      ; "version", `Int (Option.value_exn version)
      ; "identity", CompletionItem.yojson_of_t (Option.value_exn identity)
      ]
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    match member "params" json with
    | `Null ->
      { params = CompletionParams.t_of_yojson json; version = None; identity = None }
    | params ->
      { params = CompletionParams.t_of_yojson params
      ; version = Some (member "version" json |> to_int)
      ; identity = Some (member "identity" json |> CompletionItem.t_of_yojson)
      }
  ;;

  let of_completion_item (ci : CompletionItem.t) = Option.map ci.data ~f:t_of_yojson
end

let completion_kind ~supports_enum_member kind : CompletionItemKind.t option =
  match kind with
  | `Value -> Some Value
  | `Variant -> Some (if supports_enum_member then EnumMember else Constructor)
  | `Label -> Some Field
  | `Module -> Some Module
  | `Modtype -> Some Interface
  | `MethodCall -> Some Method
  | `Keyword -> Some Keyword
  | `Constructor -> Some Constructor
  | `Type -> Some TypeParameter
;;

let ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\128' .. '\255' | '\'' | '_' -> true
  | _ -> false
;;

let path_start_char char =
  ident_char char
  ||
  match char with
  | '~' | '?' | '`' -> true
  | _ -> false
;;

let prefix_of_position ~short_path source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let end_of_prefix =
      let (`Offset index) = Msource.get_offset source position in
      min (String.length text - 1) (index - 1)
    in
    let pos =
      (*clamp the length of a line to process at 500 chars, this is just a
        reasonable limit for regex performance*)
      max 0 (end_of_prefix - 500)
    in
    let reconstructed_prefix =
      Prefix_parser.parse ~pos ~len:(end_of_prefix + 1 - pos) text
      |> Option.value ~default:""
      (* We remove the whitespace because merlin expects no whitespace and it's
         semantically meaningless *)
      |> String.filter ~f:(function
        | ' ' | '\n' | '\r' | '\t' | '\012' -> false
        | _ -> true)
    in
    let starts_like_a_path =
      (not (String.is_empty reconstructed_prefix))
      && path_start_char reconstructed_prefix.[0]
    in
    if short_path && starts_like_a_path
    then (
      match String.split reconstructed_prefix ~on:'.' |> List.last with
      | Some s -> s
      | None -> reconstructed_prefix)
    else reconstructed_prefix
;;

let suffix_of_position ~is_char source position =
  match Msource.text source with
  | "" -> ""
  | text ->
    let (`Offset index) = Msource.get_offset source position in
    let len = String.length text in
    if index >= len
    then ""
    else (
      let from = index in
      let len =
        let until =
          String.lfindi ~pos:from text ~f:(fun _ c -> not (is_char c))
          |> Option.value ~default:len
        in
        until - from
      in
      String.sub text ~pos:from ~len)
;;

let reconstruct_ident source position =
  let prefix = prefix_of_position ~short_path:false source position in
  let suffix = suffix_of_position ~is_char:ident_char source position in
  let ident = prefix ^ suffix in
  Option.some_if (ident <> "") ident
;;

let range_prefix (lsp_position : Position.t) prefix : Range.t =
  let start =
    let len = String.length prefix in
    let character = lsp_position.character - len in
    { lsp_position with character }
  in
  { Range.start; end_ = lsp_position }
;;

let edit_range doc pos =
  let source = Document.Merlin.source doc in
  let logical_pos = Position.logical pos in
  let range = range_prefix pos (prefix_of_position ~short_path:true source logical_pos) in
  let suffix =
    let text_document = Document.Merlin.to_doc doc |> Document.text_document in
    let offset = Text_document.absolute_position text_document pos in
    suffix_of_position ~is_char:ident_char source (`Offset offset)
  in
  { range with end_ = { pos with character = pos.character + String.length suffix } }
;;

let sortText_width item_count =
  max 4 (String.length (Int.to_string (max 0 (item_count - 1))))
;;

let sortText_of_index ~width idx = Printf.sprintf "%0*d" width idx

module For_tests = struct
  let sortText_of_index ~item_count idx =
    sortText_of_index ~width:(sortText_width item_count) idx
  ;;
end

let reindex_sortText completion_items =
  let width = sortText_width (List.length completion_items) in
  List.mapi completion_items ~f:(fun idx (ci : CompletionItem.t) ->
    let sortText = Some (sortText_of_index ~width idx) in
    { ci with sortText })
;;

module Complete_by_prefix = struct
  let completionItem_of_completion_entry
        idx
        (entry : Query_protocol.Compl.entry)
        ~range
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~sort_text_width
    =
    let kind = completion_kind ~supports_enum_member entry.kind in
    let { Deprecation.deprecated; tags } =
      Deprecation.create
        ~deprecated:entry.deprecated
        ~tag:CompletionItemTag.Deprecated
        ~supports_tag:supports_deprecated_tag
        ~supports_deprecated_field
    in
    let textEdit = `TextEdit { TextEdit.range; newText = entry.name } in
    CompletionItem.create
      ~label:entry.name
      ?kind
      ~detail:entry.desc
      ?deprecated
      ?tags
        (* Without this field the client is not forced to respect the order
           provided by merlin. *)
      ~sortText:(sortText_of_index ~width:sort_text_width idx)
      ~textEdit
      ()
  ;;

  let dispatch_cmd ~prefix position pipeline =
    let complete = Query_protocol.Complete_prefix (prefix, position, [], false, true) in
    Query_commands.dispatch pipeline complete
  ;;

  let process_dispatch_resp
        ~supports_deprecated_field
        ~supports_deprecated_tag
        ~supports_enum_member
        ~prefix
        doc
        pos
        (completion : Query_protocol.completions)
    =
    let range = edit_range doc pos in
    let completion_entries =
      match completion.context with
      | `Unknown -> completion.entries
      | `Application { Query_protocol.Compl.labels; argument_type = _ } ->
        completion.entries
        @ List.map labels ~f:(fun (name, typ) ->
          let name =
            if String.is_prefix prefix ~prefix:"~" && String.is_prefix name ~prefix:"?"
            then "~" ^ String.chop_prefix_if_exists name ~prefix:"?"
            else name
          in
          { Query_protocol.Compl.name
          ; kind = `Label
          ; desc = typ
          ; info = ""
          ; deprecated = false (* TODO this is wrong *)
          })
    in
    let sort_text_width = sortText_width (List.length completion_entries) in
    List.mapi
      completion_entries
      ~f:
        (completionItem_of_completion_entry
           ~supports_deprecated_field
           ~supports_deprecated_tag
           ~supports_enum_member
           ~range
           ~sort_text_width)
  ;;

  let complete_keywords completion_position prefix =
    match prefix with
    | "" | "i" | "in" ->
      let ci_for_in =
        CompletionItem.create
          ~label:"in"
          ~textEdit:
            (`TextEdit
                (TextEdit.create
                   ~newText:"in"
                   ~range:(range_prefix completion_position prefix)))
          ~kind:CompletionItemKind.Keyword
          ()
      in
      [ ci_for_in ]
    | _ -> []
  ;;
end

module Complete_with_construct = struct
  let dispatch_cmd position pipeline =
    match
      Exn_with_backtrace.try_with (fun () ->
        let command = Query_protocol.Construct (position, None, None) in
        Query_commands.dispatch pipeline command)
    with
    | Ok (loc, exprs) -> Some (loc, exprs)
    | Error { Exn_with_backtrace.exn = Merlin_analysis.Construct.Not_a_hole; _ } -> None
    | Error exn -> Exn_with_backtrace.reraise exn
  ;;

  let process_dispatch_resp ~supportsJumpToNextHole ~fallback_range ~position = function
    | None -> []
    | Some (loc, constructed_exprs) ->
      let range =
        let range = Range.of_loc loc in
        if
          range.start.line = range.end_.line
          && Range.contains_position range position ~inclusive_end:true
        then range
        else fallback_range
      in
      let sort_text_width = sortText_width (List.length constructed_exprs) in
      let deparen_constr_expr expr =
        if
          (not (String.equal expr "()"))
          && String.is_prefix expr ~prefix:"("
          && String.is_suffix expr ~suffix:")"
        then
          expr
          |> String.chop_prefix_if_exists ~prefix:"("
          |> String.chop_suffix_if_exists ~suffix:")"
        else expr
      in
      let completionItem_of_constructed_expr idx expr =
        let expr_wo_parens = deparen_constr_expr expr in
        let edit = { TextEdit.range; newText = expr } in
        let command =
          if supportsJumpToNextHole
          then
            Some
              (Client.Custom_commands.next_hole
                 ~in_range:(Range.resize_for_edit edit)
                 ~notify_if_no_hole:false
                 ())
          else None
        in
        CompletionItem.create
          ~label:expr_wo_parens
          ~textEdit:(`TextEdit edit)
          ~filterText:("_" ^ expr)
          ~kind:CompletionItemKind.Text
          ~sortText:(sortText_of_index ~width:sort_text_width idx)
          ?command
          ()
      in
      List.mapi constructed_exprs ~f:completionItem_of_constructed_expr
  ;;
end

type completion_capabilities =
  { resolve : bool
  ; supports_deprecated_field : bool
  ; supports_deprecated_tag : bool
  ; supports_enum_member : bool
  ; supports_preselect : bool
  }

let completion_capabilities (state : State.t) =
  let capabilities = State.client_capabilities state in
  let resolve =
    match Capabilities.completion_resolve_properties capabilities with
    | None -> false
    | Some properties -> List.mem properties "documentation" ~equal:String.equal
  in
  let supports_deprecated_tag =
    match Capabilities.completion_tag_support capabilities with
    | None -> false
    | Some value_set ->
      Deprecation.tag_supported
        value_set
        ~tag:CompletionItemTag.Deprecated
        ~equal:(fun CompletionItemTag.Deprecated Deprecated -> true)
  in
  let supports_deprecated_field =
    (not supports_deprecated_tag)
    && Capabilities.completion_deprecated_support capabilities
  in
  let supports_enum_member =
    Option.value_map
      (Capabilities.completion_item_kind_support capabilities)
      ~default:false
      ~f:(fun value_set ->
        Capabilities.supported
          value_set
          ~tag:CompletionItemKind.EnumMember
          ~equal:Poly.equal)
  in
  let supports_preselect = Capabilities.completion_preselect_support capabilities in
  { resolve
  ; supports_deprecated_field
  ; supports_deprecated_tag
  ; supports_enum_member
  ; supports_preselect
  }
;;

type configured_completion =
  | Suppressed
  | Items of CompletionItem.t list

let run_completion_pass
      state
      merlin
      capabilities
      ~check_comments
      ~absolute_position
      ~position
      ~lsp_position
      ~prefix
      ~is_hole
      pipeline
  =
  let inside_comment =
    check_comments
    && Mpipeline.reader_comments pipeline
       |> List.exists ~f:(fun (_, (loc : Loc.t)) ->
         loc.loc_start.pos_cnum <= absolute_position
         && absolute_position <= loc.loc_end.pos_cnum)
  in
  if inside_comment
  then Suppressed
  else (
    let construct =
      if is_hole then Complete_with_construct.dispatch_cmd position pipeline else None
    in
    let completion = Complete_by_prefix.dispatch_cmd ~prefix position pipeline in
    let constructed_items =
      if is_hole
      then (
        let supportsJumpToNextHole =
          Experimental.bool
            (State.experimental_client_capabilities state)
            "jumpToNextHole"
        in
        Complete_with_construct.process_dispatch_resp
          ~supportsJumpToNextHole
          ~fallback_range:(edit_range merlin lsp_position)
          ~position:lsp_position
          construct)
      else []
    in
    let completion_items =
      Complete_by_prefix.process_dispatch_resp
        ~supports_deprecated_field:capabilities.supports_deprecated_field
        ~supports_deprecated_tag:capabilities.supports_deprecated_tag
        ~supports_enum_member:capabilities.supports_enum_member
        ~prefix
        merlin
        lsp_position
        completion
    in
    Items (constructed_items @ completion_items))
;;

let log_failure configuration error =
  Log.log ~section:"merlin" (fun () ->
    Log.msg
      "Merlin configuration failed while computing completion"
      [ "mode", `String (Merlin_config.configuration_label configuration)
      ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
      ])
;;

let deduplicate_items items =
  List.fold_left items ~init:[] ~f:(fun items item ->
    let identity = completion_identity item in
    if
      List.exists items ~f:(fun candidate ->
        Poly.equal (completion_identity candidate) identity)
    then items
    else item :: items)
  |> List.rev
;;

let merge_detail
      (configured_items : (Merlin_config.configuration * CompletionItem.t) list)
  =
  match configured_items with
  | [] -> None
  | (_, first) :: rest
    when List.for_all rest ~f:(fun (_, item) -> Poly.equal item.detail first.detail) ->
    first.detail
  | configured_items ->
    Some
      (List.map configured_items ~f:(fun (configuration, item) ->
         let mode = Merlin_config.configuration_label configuration in
         sprintf "%s: %s" mode (Option.value item.detail ~default:"<none>"))
       |> String.concat ~sep:"\n")
;;

let intersect_items
      (configured_items : (Merlin_config.configuration * CompletionItem.t list) list)
  =
  match configured_items with
  | [] -> []
  | (primary_configuration, primary_items) :: rest ->
    deduplicate_items primary_items
    |> List.filter_map ~f:(fun primary_item ->
      let identity = completion_identity primary_item in
      let matches =
        List.map rest ~f:(fun (configuration, items) ->
          List.find items ~f:(fun item -> Poly.equal (completion_identity item) identity)
          |> Option.map ~f:(fun item -> configuration, item))
      in
      if List.exists matches ~f:Option.is_none
      then None
      else (
        let configured_items =
          (primary_configuration, primary_item) :: List.filter_opt matches
        in
        let detail = merge_detail configured_items in
        Some { primary_item with detail }))
;;

let with_resolve_data params version item =
  let data =
    Resolve.yojson_of_t
      { params; version = Some version; identity = Some (completion_identity item) }
  in
  { item with CompletionItem.data = Some data }
;;

let completion_list items =
  Some (`CompletionList (CompletionList.create ~isIncomplete:false ~items ()))
;;

let complete (state : State.t) (params : CompletionParams.t) =
  Fiber.of_thunk (fun () ->
    let { CompletionParams.textDocument = { uri }; position = pos; context; _ } =
      params
    in
    let doc = Document_store.get state.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let capabilities = completion_capabilities state in
      let* { Document.Merlin.configurations; kind } =
        Document.Merlin.configuration_context_exn merlin
      in
      let position = Position.logical pos in
      let prefix = prefix_of_position ~short_path:false (Document.source doc) position in
      let is_hole = Merlin_analysis.Typed_hole.can_be_hole prefix in
      let check_comments =
        match context with
        | Some { triggerKind = TriggerCharacter; _ } -> true
        | Some { triggerKind = Invoked | TriggerForIncompleteCompletions; _ } | None ->
          false
      in
      let absolute_position =
        Text_document.absolute_position (Document.text_document doc) pos
      in
      let* results =
        Document.Merlin.with_configurations
          ~name:"completion"
          merlin
          ~configurations
          (fun _ pipeline ->
             run_completion_pass
               state
               merlin
               capabilities
               ~check_comments
               ~absolute_position
               ~position
               ~lsp_position:pos
               ~prefix
               ~is_hole
               pipeline)
      in
      let results = Merlin_dot_protocol.Nonempty_list.to_list results in
      let failure =
        List.find_map results ~f:(fun { Document.Merlin.configuration; result } ->
          match result with
          | Ok _ -> None
          | Error error ->
            log_failure configuration error;
            Some ())
      in
      if Option.is_some failure
      then Fiber.return (completion_list [])
      else if
        List.exists results ~f:(fun { Document.Merlin.result; _ } ->
          match result with
          | Ok Suppressed -> true
          | Ok (Items _) | Error _ -> false)
      then Fiber.return None
      else (
        let configured_items =
          List.map results ~f:(fun { Document.Merlin.configuration; result } ->
            match result with
            | Ok (Items items) -> configuration, items
            | Ok Suppressed | Error _ -> assert false)
        in
        let primary = Merlin_config.primary configurations in
        let primary_items, other_items =
          List.partition_tf configured_items ~f:(fun (configuration, _) ->
            configuration == primary)
        in
        let configured_items =
          match primary_items with
          | [ primary_items ] -> primary_items :: other_items
          | [] | _ :: _ :: _ ->
            invalid_arg "Compl.complete: missing primary configuration"
        in
        let items = intersect_items configured_items in
        let items =
          if capabilities.resolve
          then List.map items ~f:(with_resolve_data params (Document.version doc))
          else items
        in
        let keyword_items =
          match kind with
          | Document.Kind.Impl -> Complete_by_prefix.complete_keywords pos prefix
          | Intf -> []
        in
        let items = keyword_items @ items |> reindex_sortText in
        let items =
          if is_hole && capabilities.supports_preselect
          then (
            match items with
            | [] -> []
            | item :: rest -> { item with CompletionItem.preselect = Some true } :: rest)
          else items
        in
        Fiber.return (completion_list items)))
;;

let format_doc ~markdown doc =
  match markdown with
  | false -> `String doc
  | true ->
    `MarkupContent
      (match Doc_to_md.translate doc with
       | Markdown value -> { kind = MarkupKind.Markdown; MarkupContent.value }
       | Raw value -> { kind = MarkupKind.PlainText; MarkupContent.value })
;;

let mode_documentation ~markdown configured_docs =
  match configured_docs with
  | [] -> None
  | (_, first) :: rest when List.for_all rest ~f:(fun (_, doc) -> Poly.equal doc first) ->
    Option.map first ~f:(format_doc ~markdown)
  | configured_docs ->
    let render = function
      | None -> ""
      | Some doc ->
        if markdown
        then (
          match Doc_to_md.translate doc with
          | Markdown value | Raw value -> value)
        else doc
    in
    let value =
      List.map configured_docs ~f:(fun (configuration, doc) ->
        let mode = Merlin_config.configuration_label configuration in
        let doc = render doc in
        if markdown then sprintf "### %s\n\n%s" mode doc else sprintf "%s:\n%s" mode doc)
      |> String.concat ~sep:(if markdown then "\n\n---\n\n" else "\n\n")
    in
    if markdown
    then Some (`MarkupContent { MarkupContent.kind = MarkupKind.Markdown; value })
    else Some (`String value)
;;

let completion_change (identity : CompletionItem.t) =
  match identity.textEdit with
  | None -> None
  | Some (`TextEdit { TextEdit.range; newText }) ->
    Some
      (`TextDocumentContentChangePartial
          (TextDocumentContentChangePartial.create ~range ~text:newText ()))
  | Some (`InsertReplaceEdit { Lsp.Types.InsertReplaceEdit.replace = range; newText; _ })
    ->
    Some
      (`TextDocumentContentChangePartial
          (TextDocumentContentChangePartial.create ~range ~text:newText ()))
;;

let legacy_completion_change doc position label =
  let logical_position = Position.logical position in
  let source = Document.Merlin.source doc in
  let prefix = prefix_of_position ~short_path:true source logical_position in
  let suffix =
    let is_operator =
      (not (String.is_empty prefix))
      && String.for_all prefix ~f:Ocaml_operator.is_symbolic_character
    in
    let is_char =
      if is_operator then Ocaml_operator.is_symbolic_character else ident_char
    in
    suffix_of_position ~is_char source logical_position
  in
  let start = { position with character = position.character - String.length prefix } in
  let end_ = { position with character = position.character + String.length suffix } in
  `TextDocumentContentChangePartial
    (TextDocumentContentChangePartial.create
       ~range:(Range.create ~start ~end_)
       ~text:label
       ())
;;

let resolve_legacy doc (compl : CompletionItem.t) params ~markdown =
  let position = params.CompletionParams.position in
  let logical_position = Position.logical position in
  let* { Document.Merlin.configurations; _ } =
    Document.Merlin.configuration_context_exn doc
  in
  let change = legacy_completion_change doc position compl.label in
  let doc =
    Document.Merlin.to_doc doc
    |> (fun doc ->
    Document.with_merlin_configuration doc (Merlin_config.primary configurations))
    |> (fun doc -> Document.update_text doc [ change ])
    |> Document.merlin_exn
  in
  let+ documentation = Document.Merlin.doc_comment doc logical_position in
  let documentation = Option.map documentation ~f:(format_doc ~markdown) in
  { compl with documentation; data = None }
;;

let resolve
      (state : State.t)
      doc
      (compl : CompletionItem.t)
      (resolve : Resolve.t)
      ~markdown
  =
  Fiber.of_thunk (fun () ->
    match resolve.version, resolve.identity with
    | None, None -> resolve_legacy doc compl resolve.params ~markdown
    | None, Some _ | Some _, None -> Fiber.return compl
    | Some version, Some identity ->
      if
        Document.version (Document.Merlin.to_doc doc) <> version
        || not (Poly.equal (completion_identity compl) identity)
      then Fiber.return compl
      else (
        let params = resolve.params in
        let { CompletionParams.position; context; _ } = params in
        let logical_position = Position.logical position in
        let prefix =
          prefix_of_position
            ~short_path:false
            (Document.Merlin.source doc)
            logical_position
        in
        let is_hole = Merlin_analysis.Typed_hole.can_be_hole prefix in
        let check_comments =
          match context with
          | Some { triggerKind = TriggerCharacter; _ } -> true
          | Some { triggerKind = Invoked | TriggerForIncompleteCompletions; _ } | None ->
            false
        in
        let absolute_position =
          Text_document.absolute_position
            (Document.Merlin.to_doc doc |> Document.text_document)
            position
        in
        let capabilities = completion_capabilities state in
        let* { Document.Merlin.configurations; _ } =
          Document.Merlin.configuration_context_exn doc
        in
        let* passes =
          Document.Merlin.with_configurations
            ~name:"completion-resolve-revalidate"
            doc
            ~configurations
            (fun _ pipeline ->
               run_completion_pass
                 state
                 doc
                 capabilities
                 ~check_comments
                 ~absolute_position
                 ~position:logical_position
                 ~lsp_position:position
                 ~prefix
                 ~is_hole
                 pipeline)
        in
        let portable =
          Merlin_dot_protocol.Nonempty_list.to_list passes
          |> List.for_all ~f:(fun { Document.Merlin.configuration; result } ->
            match result with
            | Error error ->
              log_failure configuration error;
              false
            | Ok Suppressed -> false
            | Ok (Items items) ->
              List.exists items ~f:(fun item ->
                Poly.equal (completion_identity item) identity))
        in
        if not portable
        then Fiber.return compl
        else (
          match completion_change identity with
          | None -> Fiber.return compl
          | Some change ->
            let updated =
              Document.update_text (Document.Merlin.to_doc doc) [ change ]
              |> Document.merlin_exn
            in
            let* docs =
              Document.Merlin.with_configurations
                ~name:"completion-resolve-documentation"
                updated
                ~configurations
                (fun _ pipeline ->
                   match
                     Query_commands.dispatch
                       pipeline
                       (Query_protocol.Document (None, logical_position))
                   with
                   | `Found doc | `Builtin doc -> Some doc
                   | _ -> None)
            in
            let docs = Merlin_dot_protocol.Nonempty_list.to_list docs in
            if
              List.exists docs ~f:(fun { Document.Merlin.configuration; result } ->
                match result with
                | Ok _ -> false
                | Error error ->
                  log_failure configuration error;
                  true)
            then Fiber.return compl
            else (
              let documentation =
                List.map docs ~f:(fun { Document.Merlin.configuration; result } ->
                  match result with
                  | Ok doc -> configuration, doc
                  | Error _ -> assert false)
                |> mode_documentation ~markdown
              in
              Fiber.return { compl with documentation; data = None }))))
;;
