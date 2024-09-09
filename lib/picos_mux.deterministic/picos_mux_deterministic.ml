open Picos

let ready = Runnable.create ()

let run f =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  try f ()
  with e ->
    Printexc.print_backtrace stderr;
    raise e

(** [maybe_run_ready ()] deterministically chooses to run:
  - no elements
  - up to N elements
  - all elements (including newly inserted ones) until [ready] becomes empty
 *)
let maybe_run_ready () = Runnable.iter ready run

(* run_fiber returning, we are allowed to run more ready elements here *)
let retc ret =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  maybe_run_ready ();
  ret

let exnc exn =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  let bt = Printexc.get_raw_backtrace () in
  maybe_run_ready ();
  Printexc.raise_with_backtrace exn bt

let push f = Runnable.push ready f

let run_next () =
  Runnable.pop ready |> run

let handle_and_run_next f =
  Some
    (fun cont ->
      Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
      f cont;
      run_next ())

let handle ?(can_discontinue = true) ?trigger current f =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  handle_and_run_next @@ fun cont ->
  match (can_discontinue, Fiber.canceled current) with
  | true, Some (exn, bt) when Picos_aux_choice.bool () ->
      Trace.message "Fiber is canceled, discontinuing early";
      (* we can discontinue early, but that is a decision, and all
         decisions are routed through the deterministic choice logic
      *)
      Option.iter Trigger.dispose trigger;
      push @@ fun () -> Effect.Deep.discontinue_with_backtrace cont exn bt
  | _ -> f cont

let resume trigger fiber cont =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  let (_ : bool) = Fiber.unsuspend fiber trigger in
  push @@ fun () -> Fiber.resume fiber cont

let return value cont =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  (* we may return immediately if this is at the front of the queue,
     otherwise we may run some other effects first *)
  push @@ fun () ->
  Effect.Deep.continue cont value

let await trigger current cont =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  if not (Fiber.try_suspend current trigger current cont resume) then
    push @@ fun () -> Fiber.resume current cont

let rec run_fiber ~fatal_exn_handler (current : Fiber.t) main () =
  Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
  let handle_current =
    (* not allowed to handle cancelation here, see Picos docs! *)
    handle ~can_discontinue:false current @@ return current
  and handle_spawn fiber main =
    handle current @@ fun cont ->
    Trace.with_span ~__FILE__ ~__LINE__ __FUNCTION__ @@ fun _sp ->
    push @@ run_fiber ~fatal_exn_handler fiber main;
    return () cont
  and handle_yield = handle current @@ return ()
  and handle_await trigger = handle ~trigger current @@ await trigger current in
  let effc :
      type a. a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option
      = function
    | Fiber.Current ->
      Trace.message "current";
      handle_current
    | Fiber.Spawn { fiber; main } ->
      Trace.message "spawn";
      handle_spawn fiber main
    | Fiber.Yield ->
      Trace.message "yield";
       handle_yield
    | Trigger.Await trigger ->
      Trace.message "await";
       handle_await trigger
    | _ -> None
  in
  Effect.Deep.match_with main current { retc; exnc = fatal_exn_handler; effc }

let run_fiber ?(fatal_exn_handler = exnc) current main =
  run_fiber ~fatal_exn_handler current main ()

let run ?(forbid = false) main =
  let mode = if Picos_aux_choice.bool () then `LIFO else `FIFO in
  let computation = Computation.create ~mode () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber fiber main;
  Computation.peek_exn computation
