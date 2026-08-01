open Import
open Fiber.O

let position_of_offset source offset =
  let (`Logical (line, character)) = Msource.get_logical source (`Offset offset) in
  Position.of_logical ~line ~character
;;

let compare_text_edit (a : TextEdit.t) (b : TextEdit.t) =
  match Range.compare a.range b.range with
  | 0 -> String.compare a.newText b.newText
  | ordering -> ordering
;;

let is_parenthesized source ~start_offset ~end_offset =
  end_offset - start_offset >= 3
  && Char.equal source.[start_offset] '('
  && Char.equal source.[end_offset - 1] ')'
;;

(* Merlin includes the syntactically required parentheses in symbolic operator
   locations. Rename only the operator so that clients do not use the
   parentheses as part of its name. *)
let identifier_range source (range : Range.t) =
  let (`Offset start_offset) = Msource.get_offset source (Position.logical range.start) in
  let (`Offset end_offset) = Msource.get_offset source (Position.logical range.end_) in
  let source_text = Msource.text source in
  match is_parenthesized source_text ~start_offset ~end_offset with
  | false -> range
  | true ->
    let rec skip_whitespace_forward offset =
      if offset < end_offset - 1 && Char.is_whitespace source_text.[offset]
      then skip_whitespace_forward (offset + 1)
      else offset
    in
    let operator_start = skip_whitespace_forward (start_offset + 1) in
    let rec skip_whitespace_backward offset =
      if operator_start < offset && Char.is_whitespace source_text.[offset - 1]
      then skip_whitespace_backward (offset - 1)
      else offset
    in
    let operator_end = skip_whitespace_backward (end_offset - 1) in
    let rec contains_only_operator_characters offset =
      offset = operator_end
      || (Ocaml_operator.is_symbolic_character source_text.[offset]
          && contains_only_operator_characters (offset + 1))
    in
    if operator_start < operator_end && contains_only_operator_characters operator_start
    then
      { Range.start = position_of_offset source operator_start
      ; end_ = position_of_offset source operator_end
      }
    else range
;;

let request_failed message =
  Jsonrpc.Response.Error.raise
    (Jsonrpc.Response.Error.make ~code:RequestFailed ~message ())
;;

let configured_values ~operation results =
  let values, errors =
    Merlin_dot_protocol.Nonempty_list.to_list results
    |> List.fold_left ~init:([], []) ~f:(fun (values, errors) result ->
      let { Document.Merlin.configuration; result } = result in
      match result with
      | Ok value -> (configuration, value) :: values, errors
      | Error error -> values, (configuration, error) :: errors)
  in
  match errors with
  | [] -> List.rev values
  | errors ->
    List.iter errors ~f:(fun (configuration, error) ->
      Log.log ~section:"merlin" (fun () ->
        Log.msg
          ("Merlin configuration failed while computing " ^ operation)
          [ "mode", `String (Merlin_config.configuration_label configuration)
          ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
          ]));
    let modes =
      List.rev_map errors ~f:(fun (configuration, _) ->
        Merlin_config.configuration_label configuration)
      |> String.concat ~sep:", "
    in
    request_failed (sprintf "%s failed for modes: %s" operation modes)
;;

let prepare
      (state : State.t)
      { PrepareRenameParams.textDocument = { uri }; position; workDoneToken = _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let+ results =
      Document.Merlin.dispatch_all
        ~name:"occurrences"
        merlin
        ~configurations
        (Query_protocol.Occurrences (`Ident_at (Position.logical position), `Buffer))
    in
    let source = Document.source doc in
    let ranges =
      configured_values ~operation:"prepare rename" results
      |> List.map ~f:(fun (configuration, (occurrences, _)) ->
        let range =
          List.find_map occurrences ~f:(fun (occurrence : Query_protocol.occurrence) ->
            if occurrence.is_stale
            then None
            else (
              let range = Range.of_loc occurrence.loc |> identifier_range source in
              if Lsp.Range.contains_position range position ~inclusive_end:true
              then Some range
              else None))
        in
        configuration, range)
    in
    (match ranges with
     | [] -> None
     | (_, None) :: _ -> None
     | (_, Some first) :: rest ->
       if
         List.for_all rest ~f:(function
           | _, Some range -> Poly.equal range first
           | _, None -> false)
       then Some first
       else request_failed "The applicable modes produced different rename ranges")
;;

let workspace_edit_of_locations ~document_changes ~documents ~new_name locations =
  let edits =
    List.fold_left
      locations
      ~init:(Map.empty (module Uri))
      ~f:(fun acc (uri, range) -> Map.add_multi acc ~key:uri ~data:range)
    |> Map.mapi ~f:(fun ~key:doc_uri ~data:ranges ->
      let source =
        match Map.find documents doc_uri with
        | Some document -> Document.source document
        | None ->
          let source_path = Uri.to_path doc_uri in
          In_channel.with_open_text source_path In_channel.input_all |> Msource.make
      in
      List.map ranges ~f:(fun range ->
        let edit =
          let range = identifier_range source range in
          TextEdit.create ~range ~newText:new_name
        in
        match edit.range.start with
        | { character = 0; _ } -> edit
        | pos ->
          let (`Offset index) =
            let mpos = Position.logical pos in
            Msource.get_offset source mpos
          in
          assert (index > 0)
          (* [index = 0] if we pass [`Logical (1, 0)], but we handle the case
              when [character = 0] in a separate matching branch *);
          let source_txt = Msource.text source in
          (* TODO: handle record field puning *)
          (match source_txt.[index - 1] with
           | '~' (* the occurrence is a named argument *)
           | '?' (* is an optional argument *) ->
             let empty_range_at_occur_end =
               let occur_end_pos = edit.range.end_ in
               { edit.range with start = occur_end_pos }
             in
             TextEdit.create ~range:empty_range_at_occur_end ~newText:(":" ^ new_name)
           | _ -> edit))
      |> List.stable_dedup ~compare:compare_text_edit)
  in
  if document_changes
  then (
    let documentChanges =
      Map.to_alist edits
      |> List.map ~f:(fun (uri, edits) ->
        let textDocument =
          let version = Map.find documents uri |> Option.map ~f:Document.version in
          OptionalVersionedTextDocumentIdentifier.create ~uri ?version ()
        in
        let edits = List.map edits ~f:(fun e -> `TextEdit e) in
        `TextDocumentEdit (TextDocumentEdit.create ~textDocument ~edits))
    in
    WorkspaceEdit.create ~documentChanges ())
  else (
    let changes = Map.to_alist edits in
    WorkspaceEdit.create ~changes ())
;;

let rename (state : State.t) { RenameParams.textDocument = { uri }; position; newName; _ }
  =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return (WorkspaceEdit.create ())
  | `Merlin merlin ->
    let documents =
      Document_store.fold
        state.store
        ~init:(Map.empty (module Uri))
        ~f:(fun document documents ->
          let uri = Document.uri document |> Source_path.uri in
          Map.set documents ~key:uri ~data:document)
    in
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let command =
      Query_protocol.Occurrences (`Ident_at (Position.logical position), `Renaming)
    in
    let+ results =
      Document.Merlin.dispatch_all ~name:"rename" merlin ~configurations command
    in
    let canonical_uri = Source_path.uri uri in
    let document_changes =
      Capabilities.workspace_edit_document_changes (State.client_capabilities state)
    in
    let edits =
      configured_values ~operation:"rename" results
      |> List.map ~f:(fun (configuration, (occurrences, _)) ->
        let locations =
          List.filter_map occurrences ~f:(fun (occurrence : Query_protocol.occurrence) ->
            if occurrence.is_stale
            then None
            else (
              let loc = occurrence.loc in
              let uri =
                match loc.loc_start.pos_fname with
                | "" -> canonical_uri
                | path -> Source_path.of_path path
              in
              Some (uri, Range.of_loc loc)))
        in
        let edit =
          workspace_edit_of_locations
            ~document_changes
            ~documents
            ~new_name:newName
            locations
        in
        configuration, edit)
    in
    (match edits with
     | [] -> WorkspaceEdit.create ()
     | (_, first) :: rest ->
       if List.for_all rest ~f:(fun (_, edit) -> Poly.equal edit first)
       then first
       else request_failed "The applicable modes produced different rename targets")
;;
