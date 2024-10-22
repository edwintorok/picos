val run : ?mode:[`FIFO | `LIFO] -> ?forbid:bool -> (unit -> 'a) -> 'a

val run_fiber :
  ?parent:Trace.explicit_span ->
  ?fatal_exn_handler:(exn -> unit) -> Picos.Fiber.t -> (Picos.Fiber.t -> unit) -> unit
