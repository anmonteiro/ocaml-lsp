open Import

val primary_merlin : Document.Merlin.t -> Document.Merlin.t Fiber.t
val singleton_merlin : Document.Merlin.t -> Document.Merlin.t Fiber.t

val with_primary_merlin
  :  State.t
  -> Uri.t
  -> default:'a
  -> (Document.Merlin.t -> 'a Fiber.t)
  -> 'a Fiber.t

val with_singleton_merlin
  :  State.t
  -> Uri.t
  -> default:'a
  -> (Document.Merlin.t -> 'a Fiber.t)
  -> 'a Fiber.t

val with_primary_pipeline
  :  State.t
  -> Uri.t
  -> default:'a
  -> (Mpipeline.t -> 'a)
  -> 'a Fiber.t

val with_singleton_impl_pipeline
  :  State.t
  -> Uri.t
  -> default:'a
  -> (Mpipeline.t -> 'a)
  -> 'a Fiber.t

val raise_invalid_params : ?data:Json.t -> message:string -> unit -> 'a
val markup_content : kind:MarkupKind.t -> value:string -> MarkupContent.t

type 't req_params_spec =
  { params_schema : Jsonrpc.Structured.t
    (** used to document the structure of the params; example:
      [`Assoc [ "uri" , `String "<Uri>" ]]; *)
  ; of_jsonrpc_params : Jsonrpc.Structured.t -> 't option
    (** parses given structured JSON if it's of the expected schema;
      otherwise, return [None] *)
  }

val of_jsonrpc_params_exn
  :  'req_params req_params_spec
  -> Jsonrpc.Structured.t option
  -> 'req_params
