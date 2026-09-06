open Import

val command_name : string
val kinds : CodeActionKind.t list
val available : ShowDocumentClientCapabilities.t option -> bool
val command_run : 'a Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t

val code_actions
  :  Document.t
  -> Document.Merlin.configuration_context
  -> CodeActionParams.t
  -> ShowDocumentClientCapabilities.t option
  -> CodeAction.t list Fiber.t
