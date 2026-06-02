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
    { text_document =
        json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    }
  ;;
end

type t =
  { id : string
  ; mode : string option
  ; is_default : bool
  ; is_active : bool
  }

let t_of_yojson json =
  let open Yojson.Safe.Util in
  { id = json |> member "id" |> to_string
  ; mode = json |> member "mode" |> to_string_option
  ; is_default = json |> member "isDefault" |> to_bool
  ; is_active = json |> member "isActive" |> to_bool
  }
;;

let yojson_of_t { id; mode; is_default; is_active } =
  `Assoc
    [ "id", `String id
    ; "mode", (match mode with
                | None -> `Null
                | Some mode -> `String mode)
    ; "isDefault", `Bool is_default
    ; "isActive", `Bool is_active
    ]
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri } } =
      (Option.value ~default:(`Assoc []) params :> Json.t) |> Request_params.t_of_yojson
    in
    match Document_store.get_opt state.State.store uri with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InvalidParams
           ~message:
             "ocamllsp/merlinConfigurations received a URI for an unloaded file. Load \
              the file first."
           ())
    | Some doc ->
      (match Document.kind doc with
       | `Other -> Fiber.return (`List [])
       | `Merlin merlin ->
         let+ configurations = Document.Merlin.configurations_with_active merlin in
         configurations
         |> List.map ~f:(fun ((configuration : Merlin_config.configuration), is_active) ->
           { id = configuration.id
           ; mode = configuration.mode
           ; is_default = configuration.is_default
           ; is_active
           })
         |> Json.yojson_of_list yojson_of_t))
;;
