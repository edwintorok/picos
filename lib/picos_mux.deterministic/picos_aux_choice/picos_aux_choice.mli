val init: unit -> unit
(** [init ()] initialises the generator *)

val bool: unit -> bool
(** [bool ()] returns a deterministically chosen boolean *)

val bits : unit -> int
(** [bits ()] returns 30 bits chosen deterministically *)
