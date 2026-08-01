open Import
open Fiber.O

let symbol_kind_of_outline_kind = function
  | `Value -> Lsp.Types.SymbolKind.Variable
  | `Constructor | `Exn -> EnumMember
  | `Label -> EnumMember
  | `Module -> Module
  | `Type -> TypeParameter
  | `Class -> Class
  | `ClassType | `Modtype -> Interface
  | `Method -> Method
;;

(* An absent [symbolKind.valueSet] means the client only supports the kinds from
   the initial version of the protocol, so fall back to one of those. *)
let supported_symbol_kind supported kind =
  match supported with
  | None -> kind
  | Some supported ->
    if List.mem supported kind ~equal:Poly.equal
    then kind
    else (
      match kind with
      | SymbolKind.EnumMember -> SymbolKind.Constructor
      | TypeParameter -> Class
      | kind -> kind)
;;

let rec items_to_symbols ~supports_deprecated_tag ~supported_kinds items =
  List.rev_map
    ~f:
      (fun
        { Query_protocol.outline_name
        ; outline_kind
        ; location
        ; selection
        ; children
        ; deprecated
        ; _
        } ->
      let range = Range.of_loc location in
      (* The LSP spec requires [selectionRange] to be contained in [range].
         Preserve valid selections, clip non-empty overlaps, and fall back to
         [range] for ghost, invalid, touching, or disjoint selections. *)
      let selectionRange =
        match Range.of_loc_opt selection with
        | None -> range
        | Some selection -> Lsp.Range.normalize_selection_range range ~selection
      in
      let { Deprecation.deprecated; tags } =
        Deprecation.create
          ~deprecated
          ~tag:SymbolTag.Deprecated
          ~supports_tag:supports_deprecated_tag
          ~supports_deprecated_field:true
      in
      DocumentSymbol.create
        ~name:outline_name
        ~kind:
          (supported_symbol_kind
             supported_kinds
             (symbol_kind_of_outline_kind outline_kind))
        ~range
        ~selectionRange
        ?deprecated
        ?tags
        ~children:(items_to_symbols ~supports_deprecated_tag ~supported_kinds children)
        ())
    items
;;

let rec merge_document_symbols symbol_lists =
  let rec add (symbol : DocumentSymbol.t) = function
    | [] ->
      [ { symbol with children = None }, [ Option.value symbol.children ~default:[] ] ]
    | (candidate, children) :: rest ->
      let key = { symbol with children = None } in
      if Poly.equal candidate key
      then (candidate, Option.value symbol.children ~default:[] :: children) :: rest
      else (candidate, children) :: add symbol rest
  in
  List.concat symbol_lists
  |> List.fold_left ~init:[] ~f:(fun groups symbol -> add symbol groups)
  |> List.map ~f:(fun ((symbol : DocumentSymbol.t), children) ->
    { symbol with children = Some (merge_document_symbols (List.rev children)) })
;;

let run (client_capabilities : ClientCapabilities.t) doc uri =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let* { Document.Merlin.configurations; _ } =
      Document.Merlin.configuration_context_exn merlin
    in
    let+ configured =
      Document.Merlin.dispatch_all
        ~name:"document-symbols"
        merlin
        ~configurations
        Query_protocol.Outline
    in
    let supports_deprecated_tag =
      Option.value_map
        (Capabilities.document_symbol_tag_support client_capabilities)
        ~default:false
        ~f:(fun value_set ->
          Deprecation.tag_supported
            value_set
            ~tag:Deprecated
            ~equal:(fun SymbolTag.Deprecated Deprecated -> true))
    in
    let supported_kinds = Capabilities.document_symbol_kind_support client_capabilities in
    let symbols, failures, successes =
      Merlin_dot_protocol.Nonempty_list.to_list configured
      |> List.fold_left
           ~init:([], [], 0)
           ~f:
             (fun
               (symbols, failures, successes)
               ({ configuration; result } : _ Document.Merlin.configured_result)
             ->
             match result with
             | Error error -> symbols, (configuration, error) :: failures, successes
             | Ok outline ->
               ( items_to_symbols ~supports_deprecated_tag ~supported_kinds outline
                 :: symbols
               , failures
               , successes + 1 ))
    in
    List.iter failures ~f:(fun (configuration, error) ->
      Log.log ~section:"merlin" (fun () ->
        Log.msg
          "Merlin document symbols configuration failed"
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
    let symbols = merge_document_symbols (List.rev symbols) in
    (match Capabilities.document_symbol_hierarchical_support client_capabilities with
     | true -> Some (`DocumentSymbol symbols)
     | false ->
       let uri = Source_path.uri uri in
       let flattened = Lsp.Document_symbol.flatten ~uri symbols in
       Some (`SymbolInformation flattened))
;;
