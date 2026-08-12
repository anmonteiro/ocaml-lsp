open Import
open Fiber.O

let command_name = "ocamllsp/merlin-jump-to-target"

let targets =
  [ "fun"; "match"; "let"; "module"; "module-type"; "match-next-case"; "match-prev-case" ]
;;

let rename_target target = String.chop_prefix_if_exists target ~prefix:"match-"
let kind target = CodeActionKind.Other (sprintf "merlin-jump-%s" (rename_target target))
let kinds = List.map targets ~f:kind

let available (capabilities : ShowDocumentClientCapabilities.t option) =
  match capabilities with
  | Some { support } -> support
  | None -> false
;;

let error message =
  Jsonrpc.Response.Error.raise
  @@ Jsonrpc.Response.Error.make
       ~code:Jsonrpc.Response.Error.Code.InvalidParams
       ~message
       ()
;;

let command_run server (params : ExecuteCommandParams.t) =
  let uri, range =
    match params.arguments with
    | Some [ json_uri; json_range ] ->
      let uri = DocumentUri.t_of_yojson json_uri in
      let range = Range.t_of_yojson json_range in
      uri, range
    | None | Some _ -> error "takes a URI and a range as input"
  in
  let+ { ShowDocumentResult.success } =
    let req = ShowDocumentParams.create ~uri ~selection:range ~takeFocus:true () in
    Server.request server (Server_request.ShowDocumentRequest req)
  in
  if not success
  then (
    let uri = Uri.to_string uri in
    Format.eprintf "failed to open %s@." uri);
  `Null
;;

(* Dispatch the jump request to Merlin and get the result *)
let process_jump_request ~merlin ~position ~target =
  let+ results =
    Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
      let pposition = Position.logical position in
      let query = Query_protocol.Jump (target, pposition) in
      Query_commands.dispatch pipeline query)
  in
  match results with
  | `Error _ -> None
  | `Found pos -> Some pos
;;

type jump =
  { configuration : Merlin_config.configuration
  ; target : string
  ; uri : Uri.t
  ; range : Range.t
  }

type jump_group =
  { configurations : Merlin_config.configuration list
  ; uri : Uri.t
  ; range : Range.t
  }

let reraise_cancellation errors =
  List.find errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
    match exn with
    | Jsonrpc.Response.Error.E { code = RequestCancelled; _ } -> true
    | _ -> false)
  |> Option.iter ~f:Exn_with_backtrace.reraise
;;

let log_failures configuration errors =
  List.iter errors ~f:(fun error ->
    Log.log ~section:"code-actions" (fun () ->
      Log.msg
        "Configured Merlin jump computation failed"
        [ "mode", `String (Merlin_config.configuration_label configuration)
        ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
        ]))
;;

let group_jumps jumps =
  List.fold_left jumps ~init:[] ~f:(fun groups jump ->
    let rec add = function
      | [] ->
        [ { configurations = [ jump.configuration ]; uri = jump.uri; range = jump.range }
        ]
      | group :: rest ->
        if Uri.equal group.uri jump.uri && Poly.equal group.range jump.range
        then
          { group with configurations = group.configurations @ [ jump.configuration ] }
          :: rest
        else group :: add rest
    in
    add groups)
;;

let action_of_group ~label_modes target group =
  let base_title = sprintf "%s jump" (String.capitalize (rename_target target)) in
  let title =
    if label_modes
    then
      sprintf
        "%s (%s)"
        base_title
        (List.map group.configurations ~f:Merlin_config.configuration_label
         |> String.concat ~sep:", ")
    else base_title
  in
  let arguments = [ DocumentUri.yojson_of_t group.uri; Range.yojson_of_t group.range ] in
  let command = Command.create ~title ~command:command_name ~arguments () in
  CodeAction.create ~title ~kind:(kind target) ~command ()
;;

let code_actions
      (doc : Document.t)
      ({ Document.Merlin.configurations; _ } : Document.Merlin.configuration_context)
      (params : CodeActionParams.t)
      (capabilities : ShowDocumentClientCapabilities.t option)
  =
  let targets =
    List.filter targets ~f:(fun target ->
      Lsp.Code_action.kind_is_requested params.context.only (kind target))
  in
  match targets with
  | _ :: _ when available capabilities ->
    let configurations = Merlin_config.configuration_list configurations in
    let uri = Document.uri doc |> Source_path.uri in
    let rec collect acc = function
      | [] -> Fiber.return (List.rev acc)
      | configuration :: rest ->
        let configured_doc = Document.with_merlin_configuration doc configuration in
        let configured_merlin = Document.merlin_exn configured_doc in
        let* result =
          Fiber.collect_errors (fun () ->
            Fiber.parallel_map targets ~f:(fun target ->
              let+ result =
                process_jump_request
                  ~merlin:configured_merlin
                  ~position:params.range.start
                  ~target
              in
              let open Option.O in
              let* lexing_position = result in
              let+ position = Position.of_lexical_position lexing_position in
              let range = { Range.start = position; end_ = position } in
              { configuration; target; uri; range }))
        in
        let jumps =
          match result with
          | Ok jumps -> List.filter_opt jumps
          | Error errors ->
            reraise_cancellation errors;
            log_failures configuration errors;
            []
        in
        collect (List.rev_append jumps acc) rest
    in
    let+ jumps = collect [] configurations in
    List.concat_map targets ~f:(fun target ->
      let groups =
        List.filter jumps ~f:(fun jump -> String.equal jump.target target) |> group_jumps
      in
      let configuration_count = List.length configurations in
      let divergent = List.length groups > 1 in
      List.map groups ~f:(fun group ->
        let label_modes =
          divergent || List.length group.configurations <> configuration_count
        in
        action_of_group ~label_modes target group))
  | _ -> Fiber.return []
;;
