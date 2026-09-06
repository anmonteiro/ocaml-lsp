(** Fetch merlin configuration from a project build system (usually dune) *)

open Import

type t

type mode =
  | Ocaml
  | Melange
  | Other of string

val mode_key : mode -> string
val mode_label : mode -> string

type source_kind =
  | Implementation
  | Interface

type configuration_origin =
  | Plural of
      { mode : mode
      ; kind : source_kind
      ; counterpart : Uri.t option
      }
  | Legacy_file

type configuration =
  { origin : configuration_origin
  ; is_default : bool
  ; config : Mconfig.t
  }

type configuration_set
type error = string list

val configuration_mode : configuration -> mode option
val configuration_label : configuration -> string
val prefer_dot_merlin : bool ref
val configurations : t -> (configuration_set, error) result Fiber.t
val configuration_list : configuration_set -> configuration list
val singleton : configuration -> configuration_set
val primary : configuration_set -> configuration
val find_mode : configuration_set -> mode -> configuration option

(** Temporary singular projection for handlers that have not migrated yet. *)
val config : t -> Mconfig.t Fiber.t

val destroy : t -> unit Fiber.t

module DB : sig
  type config := t
  type t

  val create
    :  trace:(message:(unit -> string) -> verbose:(unit -> string) -> unit Fiber.t)
    -> t

  val stop : t -> unit Fiber.t
  val run : t -> unit Fiber.t
  val get : t -> Uri.t -> config
end
