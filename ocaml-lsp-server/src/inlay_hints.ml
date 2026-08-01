open Import
open Fiber.O

let outline_type typ =
  typ
  |> Format.asprintf "@[<h>: %s@]"
  |> String.split_on_chars ~on:[ ' '; '\t'; '\n' ]
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;

let compute (state : State.t) { InlayHintParams.range; textDocument = { uri }; _ } =
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin doc ->
    let* { Document.Merlin.configurations; kind } =
      Document.Merlin.configuration_context_exn doc
    in
    if kind = Intf
    then Fiber.return None
    else (
      let hint_let_bindings =
        Option.map state.configuration.data.inlay_hints ~f:(fun c -> c.hint_let_bindings)
        |> Option.value ~default:false
      in
      let hint_pattern_variables =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_pattern_variables)
        |> Option.value ~default:false
      in
      let hint_function_params =
        Option.map state.configuration.data.inlay_hints ~f:(fun c ->
          c.hint_function_params)
        |> Option.value ~default:false
      in
      let+ configured =
        Document.Merlin.with_configurations
          ~name:"inlay-hints"
          doc
          ~configurations
          (fun _ pipeline ->
             let start = range.start |> Position.logical
             and stop = range.end_ |> Position.logical in
             let command =
               Query_protocol.Inlay_hints
                 ( start
                 , stop
                 , hint_let_bindings
                 , hint_pattern_variables
                 , hint_function_params
                 , not inside_test )
             in
             let hints = Query_commands.dispatch pipeline command in
             List.filter_map
               ~f:(fun (pos, label) ->
                 let open Option.O in
                 let+ position = Position.of_lexical_position pos in
                 position, outline_type label)
               hints)
      in
      let hints, failures, successes =
        Merlin_dot_protocol.Nonempty_list.to_list configured
        |> List.fold_left
             ~init:([], [], 0)
             ~f:
               (fun
                 (hints, failures, successes)
                 ({ configuration; result } : _ Document.Merlin.configured_result)
               ->
               match result with
               | Error error -> hints, (configuration, error) :: failures, successes
               | Ok found ->
                 ( List.rev_append
                     (List.map found ~f:(fun hint -> configuration, hint))
                     hints
                 , failures
                 , successes + 1 ))
      in
      List.iter failures ~f:(fun (configuration, error) ->
        Log.log ~section:"merlin" (fun () ->
          Log.msg
            "Merlin inlay hints configuration failed"
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
      let rec add configuration hint = function
        | [] -> [ hint, [ configuration ] ]
        | (candidate, contributors) :: rest ->
          if Poly.equal candidate hint
          then (
            let contributors =
              if
                List.exists contributors ~f:(fun contributor ->
                  contributor == configuration)
              then contributors
              else configuration :: contributors
            in
            (candidate, contributors) :: rest)
          else (candidate, contributors) :: add configuration hint rest
      in
      let groups =
        List.fold_left (List.rev hints) ~init:[] ~f:(fun groups (configuration, hint) ->
          add configuration hint groups)
      in
      let all_configurations = Merlin_config.configuration_list configurations in
      let hints =
        List.map groups ~f:(fun ((position, text), contributors) ->
          let universal = List.length contributors = List.length all_configurations in
          let label =
            if universal
            then `String text
            else (
              let labels =
                List.filter all_configurations ~f:(fun configuration ->
                  List.exists contributors ~f:(fun contributor ->
                    contributor == configuration))
                |> List.map ~f:Merlin_config.configuration_label
                |> String.concat ~sep:", "
              in
              `List
                [ Lsp.Types.InlayHintLabelPart.create ~value:text ()
                ; Lsp.Types.InlayHintLabelPart.create ~value:(" (" ^ labels ^ ")") ()
                ])
          in
          InlayHint.create
            ~kind:Type
            ~position
            ~label
            ~paddingLeft:false
            ~paddingRight:false
            ())
        |> List.filter ~f:(fun (hint : InlayHint.t) ->
          Lsp.Range.contains_position range hint.position ~inclusive_end:true)
      in
      Some hints)
;;
