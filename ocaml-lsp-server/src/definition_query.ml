open Import
open Fiber.O

let location_of_merlin_loc uri : _ -> (Location.t list, string) result = function
  | `At_origin -> Error "Already at definition point"
  | `Builtin s ->
    Error (sprintf "%S is a builtin, it is not possible to jump to its definition" s)
  | `File_not_found s -> Error (sprintf "File_not_found: %s" s)
  | `Invalid_context -> Error "Not a valid identifier"
  | `Not_found (ident, where) ->
    let msg =
      let msg = sprintf "%S not found." ident in
      Option.value_map where ~default:msg ~f:(sprintf "%s last looked in %s" msg)
    in
    Error msg
  | `Not_in_env m -> Error (sprintf "Not in environment: %s" m)
  | `Found (path, lex_position) ->
    let locations =
      Position.of_lexical_position lex_position
      |> Option.to_list
      |> List.map ~f:(fun position ->
        let range = { Range.start = position; end_ = position } in
        let uri = Option.value_map path ~default:uri ~f:Source_path.of_path in
        Source_path.location { Location.uri; range })
    in
    Ok locations
;;

let run kind (state : State.t) ?prefix uri position =
  let* () = Fiber.return () in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let command, name =
      let pos = Position.logical position in
      match kind with
      | `Definition -> Query_protocol.Locate (prefix, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (prefix, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn doc
    in
    let+ results = Document.Merlin.dispatch_all ~name doc ~configurations command in
    let locations =
      Document.Merlin.successful_results ~name results
      |> List.concat_map ~f:(fun (configuration, result) ->
        match location_of_merlin_loc uri result with
        | Ok locations -> locations
        | Error message ->
          Log.log ~section:"debug" (fun () ->
            Log.msg
              "locate failed"
              [ "kind", `String name
              ; "mode", `String (Merlin_config.configuration_label configuration)
              ; "error", `String message
              ]);
          [])
    in
    Source_path.deduplicate_locations locations
    |> (function
     | [] -> None
     | locations -> Some (`Location locations))
;;

let run_primary kind (state : State.t) ?prefix uri position =
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let command, name =
      let pos = Position.logical position in
      match kind with
      | `Definition -> Query_protocol.Locate (prefix, `ML, pos), "definition"
      | `Declaration -> Query_protocol.Locate (prefix, `MLI, pos), "declaration"
      | `Type_definition -> Query_protocol.Locate_type pos, "type definition"
    in
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let configuration = Merlin_config.primary configurations in
    let merlin =
      Document.Merlin.to_doc merlin
      |> (fun doc -> Document.with_merlin_configuration doc configuration)
      |> Document.merlin_exn
    in
    let+ result = Document.Merlin.dispatch_exn ~name merlin command in
    (match location_of_merlin_loc uri result with
     | Ok locations -> Some (`Location (Source_path.deduplicate_locations locations))
     | Error message ->
       Log.log ~section:"debug" (fun () ->
         Log.msg "locate failed" [ "kind", `String name; "error", `String message ]);
       None)
;;
