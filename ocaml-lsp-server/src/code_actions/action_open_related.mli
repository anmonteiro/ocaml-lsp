open Import

val command_name : string
val kind : CodeActionKind.t
val available : ShowDocumentClientCapabilities.t option -> bool
val command_run : _ Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t

val for_uri
  :  can_create_file:bool
  -> ShowDocumentClientCapabilities.t option
  -> Document.t
  -> (Document.Merlin.t * Document.Merlin.configuration_context) option
  -> CodeAction.t list Fiber.t
