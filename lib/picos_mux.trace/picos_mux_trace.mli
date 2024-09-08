val run : ?mode:[`FIFO | `LIFO] -> ?forbid:bool -> (unit -> 'a) -> 'a

val run_fiber :
  ?fatal_exn_handler:(exn -> unit) -> Picos.Fiber.t -> (Picos.Fiber.t -> unit) -> unit
