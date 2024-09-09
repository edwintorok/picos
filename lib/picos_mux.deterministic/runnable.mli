(** a domain/thread-safe collection of runnable functions *)
type !'a t

(** [create ()] creates a new empty collection *)
val create : unit -> _ t

(** [push t e] pushes the element [e] into [t] *)
val push: 'a t -> 'a -> unit

(** [pop t] returns a random element of [t], if any. 
  Otherwise it waits until one is available. 
*)
val pop: 'a t -> 'a

val iter: 'a t -> ('a -> unit) -> unit
(** [iter t f] iterates over a deterministically chosen, potentially empty, subset of [t] using [f] on each element.
  [f ()] is allowed to insert new elements into [t] and these could be considered by the same invocation of [iter] too.
 *)

val cardinal : _ t -> int
(** [cardinal t] returns the size of [t] for debugging purposes. *)
