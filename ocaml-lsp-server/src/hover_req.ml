open Import
open Fiber.O

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

(* possibly overwrite the default mode using an environment variable *)
let environment_mode =
  match Env_vars._IS_HOVER_EXTENDED () with
  | Some true -> Extended_variable
  | Some false | None -> Default
;;

let hover_at_cursor parsetree (`Logical (cursor_line, cursor_col)) =
  let result = ref None in
  let is_at_cursor ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_or_after_start =
      loc_start.pos_lnum < cursor_line
      || (loc_start.pos_lnum = cursor_line && start_col <= cursor_col)
    in
    let before_or_at_end =
      loc_end.pos_lnum > cursor_line
      || (loc_end.pos_lnum = cursor_line && end_col >= cursor_col)
    in
    at_or_after_start && before_or_at_end
  in
  let is_on_first_or_last_char ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_start = loc_start.pos_lnum = cursor_line && start_col = cursor_col in
    let at_end = loc_end.pos_lnum = cursor_line && end_col = cursor_col + 1 in
    at_start || at_end
  in
  (* Hover location matches a variable binding *)
  let pat (self : Ast_iterator.iterator) (pattern : Parsetree.pattern) =
    if is_at_cursor pattern.ppat_loc
    then (
      match pattern.ppat_desc with
      | Ppat_any | Ppat_constant _ | Ppat_variant _ | Ppat_unpack _ ->
        result := Some `Type_enclosing
      | Ppat_record (fields, Open) ->
        let end_of_last_field =
          match List.last fields with
          | Some (_, field) -> field.ppat_loc.loc_end
          | None -> pattern.ppat_loc.loc_start
        in
        let is_on_bracket = is_on_first_or_last_char pattern.ppat_loc in
        if
          is_on_bracket
          || is_at_cursor { pattern.ppat_loc with loc_start = end_of_last_field }
        then result := Some `Type_enclosing
      | Ppat_construct ({ loc; _ }, _)
      | Ppat_var { loc; _ }
      | Ppat_alias (_, { loc; _ })
      | Ppat_type { loc; _ }
      | Ppat_open ({ loc; _ }, _) ->
        if is_at_cursor loc then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.pat self pattern
  in
  (* Hover an identifier in an expression *)
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    if is_at_cursor expr.pexp_loc
    then (
      match expr.pexp_desc with
      | Pexp_constant _ | Pexp_variant _ | Pexp_pack _ -> result := Some `Type_enclosing
      | Pexp_ident { loc; _ }
      | Pexp_construct ({ loc; _ }, _)
      | Pexp_field (_, { loc; _ })
      | Pexp_send (_, { loc; _ })
      | Pexp_new { loc; _ } ->
        if is_at_cursor loc
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_record (fields, _) ->
        (* On a record, each field may be hovered, along with the opening or
        closing brackets. *)
        let is_on_field =
          List.exists fields ~f:(fun (({ loc; _ } : _ Asttypes.loc), _) ->
            is_at_cursor loc)
        in
        let is_on_bracket = is_on_first_or_last_char expr.pexp_loc in
        if is_on_field || is_on_bracket
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_function _ | Pexp_lazy _ ->
        (* Anonymous function expressions can be hovered on the keyword [fun] or
           [function]. Lazy expressions can also be hovered on the [lazy]
           keyword. *)
        let is_at_keyword =
          let keyword_len =
            match expr.pexp_desc with
            | Pexp_function _ -> 8
            | Pexp_lazy _ -> 4
            | _ -> 0
          in
          let pos_cnum = expr.pexp_loc.loc_start.pos_cnum + keyword_len in
          let end_of_keyword = { expr.pexp_loc.loc_start with pos_cnum } in
          is_at_cursor
            { loc_start = expr.pexp_loc.loc_start
            ; loc_end = end_of_keyword
            ; loc_ghost = false
            }
        in
        if is_at_keyword
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_extension (ppx, _) when is_at_cursor ppx.loc -> result := Some (`Ppx ppx.txt)
      | _ -> Ast_iterator.default_iterator.expr self expr)
  in
  (* Hover a value declaration in a signature *)
  let value_description
        (self : Ast_iterator.iterator)
        (desc : Parsetree.value_description)
    =
    if is_at_cursor desc.pval_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.value_description self desc
  in
  (* Hover a type *)
  let typ (_ : Ast_iterator.iterator) (typ : Parsetree.core_type) =
    if is_at_cursor typ.ptyp_loc then result := Some `Type_enclosing
  in
  (* Hover a variant constructor where it is declared *)
  let constructor_declaration
        (self : Ast_iterator.iterator)
        (decl : Parsetree.constructor_declaration)
    =
    if is_at_cursor decl.pcd_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.constructor_declaration self decl
  in
  (* Hover an exception, or a type extension constructor, where it is declared *)
  let extension_constructor
        (self : Ast_iterator.iterator)
        (ext : Parsetree.extension_constructor)
    =
    if is_at_cursor ext.pext_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.extension_constructor self ext
  in
  (* Hover a type declaration *)
  let type_declaration (self : Ast_iterator.iterator) (decl : Parsetree.type_declaration) =
    if is_at_cursor decl.ptype_name.loc
    then result := Some `Type_enclosing
    else if is_at_cursor decl.ptype_loc
    then (
      let attribute_at_cursor =
        List.find decl.ptype_attributes ~f:(fun attr -> is_at_cursor attr.attr_loc)
      in
      match attribute_at_cursor with
      | Some attr -> result := Some (`Ppx attr.attr_name.txt)
      | None -> Ast_iterator.default_iterator.type_declaration self decl)
  in
  (* Classes and objects *)
  let class_declaration
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_declaration)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_declaration self decl
  in
  let class_type_declaration
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_type_declaration)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_type_declaration self decl
  in
  let class_description
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_description)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_description self decl
  in
  let class_field (self : Ast_iterator.iterator) (field : Parsetree.class_field) =
    (match field.pcf_desc with
     | (Pcf_val ({ loc; _ }, _, _) | Pcf_method ({ loc; _ }, _, _)) when is_at_cursor loc
       -> result := Some `Type_enclosing
     | Pcf_inherit (_, _, Some { loc; _ }) when is_at_cursor loc ->
       result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_field self field
  in
  let class_type_field (self : Ast_iterator.iterator) (field : Parsetree.class_type_field)
    =
    (match field.pctf_desc with
     | (Pctf_val ({ loc; _ }, _, _, _) | Pctf_method ({ loc; _ }, _, _, _))
       when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_type_field self field
  in
  let class_expr (self : Ast_iterator.iterator) (expr : Parsetree.class_expr) =
    (match expr.pcl_desc with
     | Pcl_constr ({ loc; _ }, _) when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_expr self expr
  in
  let class_type (self : Ast_iterator.iterator) (ct : Parsetree.class_type) =
    (match ct.pcty_desc with
     | Pcty_constr ({ loc; _ }, _) when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_type self ct
  in
  (* Hover a module identifier *)
  let module_expr (self : Ast_iterator.iterator) (expr : Parsetree.module_expr) =
    if is_at_cursor expr.pmod_loc
    then (
      match expr.pmod_desc with
      | Pmod_ident { loc; _ } -> if is_at_cursor loc then result := Some `Type_enclosing
      | Pmod_structure _ ->
        let is_at_keyword =
          let keyword_len =
            6
            (* struct *)
          in
          let pos_cnum = expr.pmod_loc.loc_start.pos_cnum + keyword_len in
          is_at_cursor
            { loc_start = expr.pmod_loc.loc_start
            ; loc_end = { expr.pmod_loc.loc_start with pos_cnum }
            ; loc_ghost = false
            }
        in
        if is_at_keyword then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.module_expr self expr
  in
  (* Hover a module type *)
  let module_type (self : Ast_iterator.iterator) (mtyp : Parsetree.module_type) =
    if is_at_cursor mtyp.pmty_loc
    then (
      match mtyp.pmty_desc with
      | Pmty_ident { loc; _ } -> if is_at_cursor loc then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.module_type self mtyp
  in
  (* Hover structure items *)
  let structure_item (self : Ast_iterator.iterator) (item : Parsetree.structure_item) =
    match item.pstr_desc with
    | Pstr_module desc when is_at_cursor desc.pmb_name.loc ->
      result := Some `Type_enclosing
    | _ -> Ast_iterator.default_iterator.structure_item self item
  in
  (* Hover signature items *)
  let signature_item (self : Ast_iterator.iterator) (item : Parsetree.signature_item) =
    match item.psig_desc with
    | Psig_open desc when is_at_cursor desc.popen_expr.loc ->
      (* [open X] is not captured by [module_expr] since it uses a different
         type in the AST. *)
      result := Some `Type_enclosing
    | Psig_module desc when is_at_cursor desc.pmd_name.loc ->
      result := Some `Type_enclosing
    | _ -> Ast_iterator.default_iterator.signature_item self item
  in
  let iterator =
    { Ast_iterator.default_iterator with
      pat
    ; expr
    ; typ
    ; type_declaration
    ; constructor_declaration
    ; extension_constructor
    ; value_description
    ; module_expr
    ; module_type
    ; structure_item
    ; signature_item
    ; class_declaration
    ; class_type_declaration
    ; class_description
    ; class_field
    ; class_type_field
    ; class_type
    ; class_expr
    }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  !result
;;

let print_dividers sections = String.concat ~sep:"\n***\n" sections

let format_as_code_block ~highlighter strings =
  sprintf "```%s\n%s\n```" highlighter (String.concat ~sep:" " strings)
;;

let format_type_enclosing
      ~syntax
      ~markdown
      ~typ
      ~doc
      ~(syntax_doc : Query_protocol.syntax_doc_result option)
  =
  (* TODO for vscode, we should just use the language id. But that will not work
     for all editors *)
  let syntax_doc =
    Option.map syntax_doc ~f:(fun syntax_doc ->
      sprintf
        "`syntax` %s: %s. See [Manual](%s)"
        syntax_doc.name
        syntax_doc.description
        syntax_doc.documentation)
  in
  `MarkupContent
    (if markdown
     then (
       let value =
         let markdown_name = Document.Syntax.markdown_name syntax in
         let type_info = Some (format_as_code_block ~highlighter:markdown_name [ typ ]) in
         let doc =
           Option.map doc ~f:(fun doc ->
             match Doc_to_md.translate doc with
             | Raw d -> d
             | Markdown d -> d)
         in
         print_dividers (List.filter_opt [ type_info; syntax_doc; doc ])
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else (
       let value = print_dividers (List.filter_opt [ Some typ; syntax_doc; doc ]) in
       { MarkupContent.value; kind = MarkupKind.PlainText }))
;;

let format_ppx_expansion ~ppx ~expansion =
  let value = sprintf "(* ppx %s expansion *)\n%s" ppx expansion in
  `MarkedString { Lsp.Types.MarkedString.value; language = Some "ocaml" }
;;

let hover_verbosity (state : State.t) ~uri ~position ~version mode =
  let verbosity =
    let mode =
      match mode, environment_mode with
      | Default, Extended_variable -> Extended_variable
      | x, _ -> x
    in
    match mode with
    | Default -> 0
    | Extended_fixed v ->
      state.hover_extended.history <- None;
      v
    | Extended_variable ->
      let v =
        match state.hover_extended.history with
        | None -> 0
        | Some history ->
          if
            Uri.equal uri history.uri
            && version = history.version
            && Lsp.Position.compare position history.position = 0
          then succ history.verbosity
          else 0
      in
      state.hover_extended.history <- Some { uri; position; version; verbosity = v };
      v
  in
  verbosity
;;

let doc_comment pipeline pos =
  match Query_commands.dispatch pipeline (Query_protocol.Document (None, pos)) with
  | `Found text | `Builtin text -> Some text
  | _ -> None
;;

type raw_hover =
  | Type_enclosing of Document.Merlin.type_enclosing
  | Ppx of
      { name : string
      ; code : string
      ; range : Range.t
      }

let type_enclosing pipeline merlin position verbosity ~with_syntax_doc =
  let command = Query_protocol.Type_enclosing (None, position, Some 0) in
  let pipeline =
    match verbosity with
    | 0 -> pipeline
    | verbosity ->
      let source = Document.Merlin.source merlin in
      let config = Mpipeline.final_config pipeline in
      let config =
        { config with query = { config.query with verbosity = Lvl verbosity } }
      in
      Mpipeline.make config source
  in
  match Query_commands.dispatch pipeline command with
  | [] | (_, `Index _, _) :: _ -> None
  | (loc, `String typ, _) :: _ ->
    let doc = doc_comment pipeline position in
    let syntax_doc =
      if with_syntax_doc then Document.Merlin.syntax_doc pipeline position else None
    in
    Some (Type_enclosing { loc; typ; doc; syntax_doc })
;;

let format_raw_hover ~(server : State.t Server.t) ~(doc : Document.t) ~markdown = function
  | Ppx { name; code; range } ->
    let contents = format_ppx_expansion ~ppx:name ~expansion:code in
    Fiber.return (Hover.create ~contents ~range ())
  | Type_enclosing { Document.Merlin.loc; typ; doc = documentation; syntax_doc } ->
    let state = Server.state server in
    let syntax = Document.syntax doc in
    let* typ =
      (* We ask Ocamlformat to format this type *)
      let* result = Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ in
      match result with
      | Ok v ->
        (* OCamlformat adds an unnecessary newline at the end of the type *)
        Fiber.return (String.strip v)
      | Error `No_process -> Fiber.return typ
      | Error (`Msg message) ->
        (* We log OCamlformat errors and display the unformatted type *)
        let+ () =
          let message =
            sprintf
              "An error occurred while querying ocamlformat:\n\
               Input type: %s\n\n\
               Answer: %s"
              typ
              message
          in
          State.log_msg server ~type_:Warning ~message
        in
        typ
    in
    let contents =
      format_type_enclosing ~syntax ~markdown ~typ ~doc:documentation ~syntax_doc
    in
    let range = Range.of_loc loc in
    Fiber.return (Hover.create ~contents ~range ())
;;

let hover_contents ~markdown (hover : Hover.t) =
  match hover.contents with
  | `MarkupContent { MarkupContent.kind = Markdown; value } -> value
  | `MarkupContent { MarkupContent.kind = PlainText; value } ->
    if markdown then format_as_code_block ~highlighter:"" [ value ] else value
  | `MarkedString { Lsp.Types.MarkedString.language; value } ->
    if markdown
    then
      Option.value_map language ~default:value ~f:(fun highlighter ->
        format_as_code_block ~highlighter [ value ])
    else value
  | `List values ->
    List.map values ~f:(fun { Lsp.Types.MarkedString.language; value } ->
      if markdown
      then
        Option.value_map language ~default:value ~f:(fun highlighter ->
          format_as_code_block ~highlighter [ value ])
      else value)
    |> print_dividers
;;

let aggregate_hovers
      ~markdown
      ~configuration_count
      (hovers : (Merlin_config.configuration * Hover.t) list)
  =
  match hovers with
  | [] -> None
  | [ (_, hover) ] when configuration_count = 1 -> Some hover
  | (_, first) :: rest
    when List.length hovers = configuration_count
         && List.for_all rest ~f:(fun (_, hover) -> Poly.equal hover first) -> Some first
  | (first_configuration, first) :: rest ->
    let hovers = (first_configuration, first) :: rest in
    let range =
      if List.for_all rest ~f:(fun (_, hover) -> Poly.equal hover.range first.range)
      then first.range
      else None
    in
    let value =
      List.map hovers ~f:(fun (configuration, hover) ->
        let label = Merlin_config.configuration_label configuration in
        let contents = hover_contents ~markdown hover in
        if markdown
        then sprintf "### %s\n\n%s" label contents
        else sprintf "%s:\n%s" label contents)
      |> String.concat ~sep:(if markdown then "\n\n---\n\n" else "\n\n")
    in
    let kind = if markdown then MarkupKind.Markdown else MarkupKind.PlainText in
    Some (Hover.create ~contents:(`MarkupContent { MarkupContent.kind; value }) ?range ())
;;

let log_failure configuration error =
  Log.log ~section:"merlin" (fun () ->
    Log.msg
      "Merlin configuration failed while computing hover"
      [ "mode", `String (Merlin_config.configuration_label configuration)
      ; "error", `String (Exn_with_backtrace.to_dyn error |> Dyn.to_string)
      ])
;;

let handle_document server doc ~uri ~position mode =
  Fiber.of_thunk (fun () ->
    let state : State.t = Server.state server in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let* { Document.Merlin.configurations; _ } =
        Document.Merlin.configuration_context_exn merlin
      in
      let configuration_count =
        Merlin_config.configuration_list configurations |> List.length
      in
      let markdown =
        Capabilities.supports_markdown
          (Capabilities.hover_content_format (State.client_capabilities state))
      in
      let verbosity =
        hover_verbosity state ~uri ~position ~version:(Document.version doc) mode
      in
      let with_syntax_doc =
        match state.configuration.data.syntax_documentation with
        | Some { enable = true } -> true
        | Some _ | None -> false
      in
      let logical_position = Position.logical position in
      let* results =
        Document.Merlin.with_configurations
          ~name:"hover"
          merlin
          ~configurations
          (fun _ pipeline ->
             let parsetree = Mpipeline.reader_parsetree pipeline in
             match hover_at_cursor parsetree logical_position with
             | None -> None
             | Some `Type_enclosing ->
               type_enclosing pipeline merlin logical_position verbosity ~with_syntax_doc
             | Some (`Ppx name) ->
               (match
                  Query_commands.dispatch
                    pipeline
                    (Query_protocol.Expand_ppx logical_position)
                with
                | `No_ppx -> None
                | `Found { Query_protocol.code; attr_start; attr_end } ->
                  let range =
                    Range.of_loc
                      { loc_start = attr_start; loc_end = attr_end; loc_ghost = false }
                  in
                  Some (Ppx { name; code; range })))
      in
      let results = Merlin_dot_protocol.Nonempty_list.to_list results in
      let rec format acc = function
        | [] -> Fiber.return (List.rev acc)
        | ({ configuration; result } : _ Document.Merlin.configured_result) :: rest ->
          (match result with
           | Error error ->
             log_failure configuration error;
             format acc rest
           | Ok None -> format acc rest
           | Ok (Some raw_hover) ->
             let* hover = format_raw_hover ~server ~doc ~markdown raw_hover in
             format ((configuration, hover) :: acc) rest)
      in
      let* hovers = format [] results in
      (match hovers with
       | _ :: _ -> Fiber.return (aggregate_hovers ~markdown ~configuration_count hovers)
       | [] ->
         (match
            List.find_map results ~f:(fun { Document.Merlin.result; _ } ->
              match result with
              | Ok _ -> None
              | Error error -> Some error)
          with
          | Some error
            when List.for_all results ~f:(fun { Document.Merlin.result; _ } ->
                   Result.is_error result) -> Exn_with_backtrace.reraise error
          | Some _ | None -> Fiber.return None)))
;;

let handle server { HoverParams.textDocument = { uri }; position; _ } mode =
  let state : State.t = Server.state server in
  let doc = Document_store.get state.store uri in
  handle_document server doc ~uri ~position mode
;;

let handle_primary server { HoverParams.textDocument = { uri }; position; _ } mode =
  let state : State.t = Server.state server in
  let doc = Document_store.get state.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let doc =
      Document.with_merlin_configuration doc (Merlin_config.primary configurations)
    in
    handle_document server doc ~uri ~position mode
;;
