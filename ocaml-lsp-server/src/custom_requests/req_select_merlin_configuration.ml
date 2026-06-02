open Import
open Fiber.O

let capability = "handleSelectMerlinConfiguration", `Bool true
let meth = "ocamllsp/selectMerlinConfiguration"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; id : string
    }

  let create ~text_document ~id = { text_document; id }

  let yojson_of_t { text_document; id } =
    `Assoc
      [ "textDocument", TextDocumentIdentifier.yojson_of_t text_document
      ; "id", `String id
      ]
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    { text_document =
        json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    ; id = json |> member "id" |> to_string
    }
  ;;
end

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri }; id } =
      (Option.value ~default:(`Assoc []) params :> Json.t) |> Request_params.t_of_yojson
    in
    match Document_store.get_opt state.State.store uri with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidParams
           ~message:
             "ocamllsp/selectMerlinConfiguration received a URI for an unloaded file. \
              Load the file first."
           ())
    | Some doc ->
      (match Document.kind doc with
       | `Other ->
         Jsonrpc.Response.Error.raise
           (Jsonrpc.Response.Error.make
              ~code:InvalidRequest
              ~message:
                "Document with this URI is not supported by \
                 ocamllsp/selectMerlinConfiguration"
              ())
       | `Merlin merlin ->
         let+ result = Document.Merlin.set_active_configuration merlin ~id in
         (match result with
          | Ok () -> `Null
          | Error message ->
            Jsonrpc.Response.Error.raise
              (Jsonrpc.Response.Error.make
                 ~code:InvalidParams
                 ~message
                 ~data:(`Assoc [ "id", `String id ])
                 ()))))
;;
