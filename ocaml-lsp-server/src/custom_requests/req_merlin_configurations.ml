open Import
open Fiber.O

let capability = "handleMerlinConfigurations", `Bool true
let meth = "ocamllsp/merlinConfigurations"

module Request_params = struct
  type t = { text_document : TextDocumentIdentifier.t }

  let create ~text_document = { text_document }

  let yojson_of_t { text_document } =
    `Assoc [ "textDocument", TextDocumentIdentifier.yojson_of_t text_document ]
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    { text_document = json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    }
  ;;
end

let on_request ~params state =
  let Request_params.{ text_document = { uri } } =
    (Option.value ~default:(`Assoc []) params :> Json.t) |> Request_params.t_of_yojson
  in
  match Document_store.get_opt state.State.store uri with
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidParams
         ~message:"ocamllsp/merlinConfigurations received a URI for an unloaded file"
         ())
  | Some doc ->
    (match Document.kind doc with
     | `Other -> Fiber.return (`List [])
     | `Merlin merlin ->
       let+ { Document.Merlin.configurations; _ } =
         Document.Merlin.configuration_context_exn merlin
       in
       Merlin_config.configuration_list configurations
       |> List.map ~f:(fun configuration ->
         let mode =
           Merlin_config.configuration_mode configuration
           |> Option.map ~f:Merlin_config.mode_key
           |> Option.value_map ~default:`Null ~f:(fun mode -> `String mode)
         in
         `Assoc
           [ "mode", mode; "isDefault", `Bool configuration.Merlin_config.is_default ])
       |> fun configurations -> `List configurations)
;;
