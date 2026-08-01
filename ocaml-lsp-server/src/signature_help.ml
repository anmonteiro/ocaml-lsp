open Import
module Lexer_raw = Ocaml_preprocess.Lexer_raw
module Misc_utils = Merlin_analysis.Misc_utils
module Parser_raw = Ocaml_preprocess.Parser_raw
module Type_utils = Merlin_analysis.Type_utils

open struct
  open Ocaml_typing
  module Predef = Predef
  module Btype = Btype
end

(* Merlin may retain the nearest application after parsing has moved on to
   another expression. Lexing this slice avoids treating separators in comments
   or strings as boundaries. *)
let contains_application_boundary source ~from ~to_ =
  let source_length = String.length source in
  let from = max 0 (min from source_length) in
  let lexbuf =
    let source =
      let to_ = max 0 (min to_ source_length) in
      String.prefix source to_
    in
    Lexing.from_string source
  in
  let state = Lexer_raw.make (Lexer_raw.keywords []) in
  let rec loop ~last_was_boundary = function
    | Lexer_raw.Fail _ -> false
    | Return Parser_raw.EOF -> last_was_boundary
    | Refill k -> loop ~last_was_boundary (k ())
    | Return token ->
      let is_boundary =
        match token with
        | Parser_raw.ELSE | MINUSGREATER | SEMI | SEMISEMI -> true
        | _ -> false
      in
      if is_boundary && Lexing.lexeme_start lexbuf >= from
      then true
      else
        loop
          ~last_was_boundary:is_boundary
          (Lexer_raw.token_without_comments state lexbuf)
  in
  loop ~last_was_boundary:false (Lexer_raw.token_without_comments state lexbuf)
;;

let format_doc ~markdown ~doc =
  `MarkupContent
    (if markdown
     then (
       let value =
         match Doc_to_md.translate doc with
         | Raw d -> sprintf "(** %s *)" d
         | Markdown d -> d
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else { MarkupContent.value = doc; kind = MarkupKind.PlainText })
;;

type raw_signature =
  { label : string
  ; parameters : ParameterInformation.t list
  ; active_parameter : int option
  ; documentation : string option
  }

type signature_group =
  { signature : raw_signature
  ; contributors : (Merlin_config.configuration * string option) list
  }

let same_signature left right =
  String.equal left.label right.label
  && Poly.equal left.parameters right.parameters
  && Poly.equal left.active_parameter right.active_parameter
;;

let add_signature groups configuration signature =
  let rec loop previous = function
    | [] ->
      List.rev_append
        previous
        [ { signature; contributors = [ configuration, signature.documentation ] } ]
    | group :: rest when same_signature group.signature signature ->
      let group =
        { group with
          contributors = group.contributors @ [ configuration, signature.documentation ]
        }
      in
      List.rev_append previous (group :: rest)
    | group :: rest -> loop (group :: previous) rest
  in
  loop [] groups
;;

let documentation_text ~markdown = function
  | None -> ""
  | Some doc ->
    if markdown
    then (
      match Doc_to_md.translate doc with
      | Raw doc -> sprintf "(** %s *)" doc
      | Markdown doc -> doc)
    else doc
;;

let group_documentation ~markdown ~configuration_count contributors =
  match contributors with
  | [] -> None
  | (_, first) :: rest
    when List.length contributors = configuration_count
         && List.for_all rest ~f:(fun (_, doc) -> Poly.equal doc first) ->
    Option.map first ~f:(fun doc -> format_doc ~markdown ~doc)
  | contributors ->
    let value =
      List.map contributors ~f:(fun (configuration, doc) ->
        let mode = Merlin_config.configuration_label configuration in
        let doc = documentation_text ~markdown doc in
        if markdown then sprintf "### %s\n\n%s" mode doc else sprintf "%s:\n%s" mode doc)
      |> String.concat ~sep:(if markdown then "\n\n---\n\n" else "\n\n")
    in
    let kind = if markdown then MarkupKind.Markdown else MarkupKind.PlainText in
    Some (`MarkupContent { MarkupContent.kind; value })
;;

let log_failure configuration error =
  Log.log ~section:"merlin" (fun () ->
    Log.msg
      "Merlin configuration failed while computing signature help"
      [ "mode", `String (Merlin_config.configuration_label configuration)
      ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
      ])
;;

let run (state : State.t) { SignatureHelpParams.textDocument = { uri }; position; _ } =
  let open Fiber.O in
  let doc =
    let store = state.store in
    Document_store.get store uri
  in
  let pos = Position.logical position in
  let prefix =
    (* The value of [short_path] doesn't make a difference to the final result
       because labels cannot include dots. However, a true value is slightly
       faster for getting the prefix. *)
    Compl.prefix_of_position (Document.source doc) pos ~short_path:true
  in
  (* TODO use merlin resources efficiently and do everything in 1 thread *)
  match Document.kind doc with
  | `Other ->
    let help = SignatureHelp.create ~signatures:[] () in
    Fiber.return help
  | `Merlin merlin ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let configuration_count =
      Merlin_config.configuration_list configurations |> List.length
    in
    let client_capabilities = State.client_capabilities state in
    let supports_parameter_label_offsets =
      Capabilities.signature_label_offset_support client_capabilities
    in
    let supports_active_parameter =
      let open Option.O in
      let support =
        let* text_document = (State.client_capabilities state).textDocument in
        let* signature_help = text_document.signatureHelp in
        let* signature_information = signature_help.signatureInformation in
        signature_information.activeParameterSupport
      in
      Option.value support ~default:false
    in
    let markdown =
      Capabilities.supports_markdown
        (Capabilities.signature_documentation_format client_capabilities)
    in
    let absolute_position =
      Document.text_document doc
      |> fun document -> Text_document.absolute_position document position
    in
    let* results =
      Document.Merlin.with_configurations
        ~name:"signature-help"
        merlin
        ~configurations
        (fun _ pipeline ->
           let inside_comment =
             Mpipeline.reader_comments pipeline
             |> List.exists ~f:(fun (_, (loc : Loc.t)) ->
               loc.loc_start.pos_cnum <= absolute_position
               && absolute_position <= loc.loc_end.pos_cnum)
           in
           if inside_comment
           then None
           else (
             let application_signature =
               let typer = Mpipeline.typer_result pipeline in
               let pos = Mpipeline.get_lexing_pos pipeline pos in
               let node = Mtyper.node_at typer pos in
               match
                 Merlin_analysis.Signature_help.application_signature
                   node
                   ~prefix
                   ~cursor:pos
               with
               | None -> None
               | Some signature ->
                 let function_position =
                   Mpipeline.get_lexing_pos pipeline signature.function_position
                 in
                 let application_end, has_unassigned_parameter =
                   List.fold_left
                     signature.parameters
                     ~init:(function_position.pos_cnum, false)
                     ~f:(fun (application_end, has_unassigned_parameter) parameter ->
                       match parameter.argument with
                       | Omitted _ -> application_end, true
                       | Arg argument ->
                         ( max application_end argument.exp_loc.loc_end.pos_cnum
                         , has_unassigned_parameter || argument.exp_loc.loc_ghost ))
                 in
                 let source = Msource.text (Mpipeline.input_source pipeline) in
                 (* Error recovery can make Merlin select an application after
                    the cursor. A completed application can also remain selected
                    after inserting whitespace, despite having no parameter left
                    to edit. *)
                 if
                   pos.pos_cnum < function_position.pos_cnum
                   || (Option.is_none signature.active_param
                       && not has_unassigned_parameter)
                   || contains_application_boundary
                        source
                        ~from:application_end
                        ~to_:pos.pos_cnum
                 then None
                 else Some signature
             in
             match application_signature with
             | None -> None
             | Some application_signature ->
               let label_prefix =
                 let fun_name =
                   Option.value ~default:"_" application_signature.function_name
                 in
                 sprintf "%s : " fun_name
               in
               let offset = String.length label_prefix in
               let parameters =
                 List.map
                   application_signature.parameters
                   ~f:(fun (parameter : Merlin_analysis.Signature_help.parameter_info) ->
                     let label =
                       if supports_parameter_label_offsets
                       then
                         `Offset
                           (offset + parameter.param_start, offset + parameter.param_end)
                       else
                         `String
                           (String.sub
                              application_signature.signature
                              ~pos:parameter.param_start
                              ~len:(parameter.param_end - parameter.param_start))
                     in
                     ParameterInformation.create ~label ())
               in
               let documentation =
                 match
                   Query_commands.dispatch
                     pipeline
                     (Query_protocol.Document
                        (None, application_signature.function_position))
                 with
                 | `Found doc | `Builtin doc -> Some doc
                 | _ -> None
               in
               Some
                 { label = label_prefix ^ application_signature.signature
                 ; parameters
                 ; active_parameter = application_signature.active_param
                 ; documentation
                 }))
    in
    let results = Merlin_dot_protocol.Nonempty_list.to_list results in
    let successful, groups =
      List.fold_left results ~init:(0, []) ~f:(fun (successful, groups) result ->
        let { Document.Merlin.configuration; result } = result in
        match result with
        | Error error ->
          log_failure configuration error;
          successful, groups
        | Ok None -> successful + 1, groups
        | Ok (Some signature) ->
          successful + 1, add_signature groups configuration signature)
    in
    if successful = 0
    then (
      match
        List.find_map results ~f:(fun { Document.Merlin.result; _ } ->
          Result.error result)
      with
      | Some error -> Exn_with_backtrace.reraise error
      | None -> assert false)
    else (
      match groups with
      | [] -> Fiber.return (SignatureHelp.create ~signatures:[] ())
      | groups ->
        let primary = Merlin_config.primary configurations in
        let active_signature =
          List.findi groups ~f:(fun _ group ->
            List.exists group.contributors ~f:(fun (configuration, _) ->
              configuration == primary))
          |> Option.value_map ~default:0 ~f:fst
        in
        let active_parameter =
          (List.nth_exn groups active_signature).signature.active_parameter
        in
        let signatures =
          List.map groups ~f:(fun { signature; contributors } ->
            let documentation =
              group_documentation ~markdown ~configuration_count contributors
            in
            let activeParameter =
              Option.some_if supports_active_parameter signature.active_parameter
            in
            SignatureInformation.create
              ~label:signature.label
              ?documentation
              ?activeParameter
              ~parameters:signature.parameters
              ())
        in
        Fiber.return
          (SignatureHelp.create
             ~signatures
             ~activeSignature:active_signature
             ?activeParameter:(Some active_parameter)
             ()))
;;
