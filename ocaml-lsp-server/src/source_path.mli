open Import

val canonicalize : string -> string
val of_path : string -> Uri.t
val uri : Uri.t -> Uri.t
val location : Location.t -> Location.t
val deduplicate_locations : Location.t list -> Location.t list
