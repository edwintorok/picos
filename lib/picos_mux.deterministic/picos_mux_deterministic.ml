open Picos

let ready = Runnable.create ()

(*let run f =
  try f ()
  with e ->
    Printexc.print_backtrace stderr;
    raise e*)

let run f = f ()

(** [maybe_run_ready ()] deterministically chooses to run:
  - no elements
  - up to N elements
  - all elements (including newly inserted ones) until [ready] becomes empty
 *)
let maybe_run_ready () = Runnable.iter ready run

(* run_fiber returning, we are allowed to run more ready elements here *)
let retc ret =
  maybe_run_ready ();
  ret

let exnc exn =
  let bt = Printexc.get_raw_backtrace () in
  maybe_run_ready ();
  Printexc.raise_with_backtrace exn bt

let trace_cardinal explicit_span =
  Trace.messagef ~span:explicit_span.Trace.span (fun m ->
      m "q(%d)" (Runnable.cardinal ready))

let push ~explicit_span f =
  Runnable.push ready f;
  trace_cardinal explicit_span

let run_next explicit_span =
  let f = Runnable.pop ready in
  trace_cardinal explicit_span;
  run f

let handle_and_run_next ~explicit_span f =
  Some
    (fun cont ->
      f cont;
      run_next explicit_span)

let handle ?(can_discontinue = true) ?trigger ~parent ~name current f =
  let explicit_span =
    Trace.enter_manual_sub_span ~parent ~flavor:`Async ~__FUNCTION__ ~__FILE__
      ~__LINE__ name
  in
  handle_and_run_next ~explicit_span @@ fun cont ->
  match (can_discontinue, Fiber.canceled current) with
  | true, Some (exn, bt) when Picos_aux_choice.bool () ->
      (* we can discontinue early, but that is a decision, and all
         decisions are routed through the deterministic choice logic
      *)
      push ~explicit_span @@ fun () ->
      Option.iter Trigger.dispose trigger;
      Trace.message ~span:explicit_span.span "discontinuing early";
      Trace.exit_manual_span explicit_span;
      Effect.Deep.discontinue_with_backtrace cont exn bt
  | _ -> f ~explicit_span cont

let resume ~explicit_span trigger fiber cont =
  let (_ : bool) = Fiber.unsuspend fiber trigger in
  push ~explicit_span @@ fun () ->
  Trace.exit_manual_span explicit_span;
  Fiber.resume fiber cont

let discontinue_ exn bt ~explicit_span cont =
  push ~explicit_span @@ fun () ->
  Trace.exit_manual_span explicit_span;
  Effect.Deep.discontinue_with_backtrace cont exn bt

let return value ~explicit_span cont =
  (* we may return immediately if this is at the front of the queue,
     otherwise we may run some other effects first *)
  push ~explicit_span @@ fun () ->
  Trace.exit_manual_span explicit_span;
  Effect.Deep.continue cont value

let await trigger current ~explicit_span cont =
  let explicit_span =
    Trace.enter_manual_sub_span ~parent:explicit_span ~__FILE__ ~__LINE__
      ~__FUNCTION__ "await"
  in
  if
    not (Fiber.try_suspend current trigger current cont (resume ~explicit_span))
  then begin
    Trace.message ~span:explicit_span.span "await: already triggerred";
    push ~explicit_span @@ fun () ->
    Trace.exit_manual_span explicit_span;
    Fiber.resume current cont
  end

module Timeout = struct
  type t = { abstime: float; unique: int  }

  let make =
    let counter = Atomic.make 0 in
    fun relative ->
    (* TODO: mtime *)
    let abstime = Unix.gettimeofday () +. relative in
    {abstime; unique = Atomic.fetch_and_add counter 1}

  let compare a b =
    match Float.compare a.abstime b.abstime with
    | 0 -> Int.compare a.unique b.unique
    | n -> n

end

module Timeouts = Map.Make(Timeout)

let timers = Atomic.make Timeouts.empty

let rec update_timers f =
  let old = Atomic.get timers in
  let next = f old in
  if Atomic.compare_and_set timers old next then ()
  else begin
    Domain.cpu_relax ();
    update_timers f
  end


let rec run_fiber ~fatal_exn_handler ~parent (current : Fiber.t) main () =
  let parent =
    Trace.enter_manual_sub_span ~parent ~flavor:`Async ~__FUNCTION__ ~__FILE__
      ~__LINE__ "run_fiber(rec)"
  in
  let handle_current =
    (* not allowed to handle cancelation here, see Picos docs! *)
    handle ~can_discontinue:false ~parent ~name:"current" current
    @@ return current
  and handle_spawn fiber main =
    handle ~parent ~name:"spawn" current @@ fun ~explicit_span cont ->
    push ~explicit_span
    @@ run_fiber ~fatal_exn_handler ~parent:explicit_span fiber main;
    return () ~explicit_span cont
  and handle_yield = handle ~parent ~name:"yield" current @@ fun ~explicit_span cont ->
    (* first pop another task, so that we ensure we don't always yield back to ourselves,
       which can lead to not making progress and failing tests.
     *)
    let other = Runnable.pop_opt ready in
    trace_cardinal explicit_span;
    if Option.is_some other then Trace.message ~span:explicit_span.span "yielding"
    else Trace.message ~span:explicit_span.span "not yielding";
    Option.iter run other;
    return () ~explicit_span cont
  and handle_await trigger =
    handle ~trigger ~parent ~name:"await" current @@ await trigger current
  and handle_cancel_after seconds exn bt computation =
    handle ~parent ~name:"cancel_after" current @@ fun ~explicit_span cont ->
    (* TODO: build a list of these actions in virtual time, and decide which ones to cancel based on virtual time.
       Also if there are no pending actions then cancel everything immediately?

       for now we dispatch to picos_select
     *)
    let key = Timeout.make seconds in
    let cancel _trigger () () =
      update_timers (fun old -> Timeouts.remove key old);
      Computation.cancel computation exn bt
    in

    let trigger = Trigger.(from_action[@alert "-handler"]) () () cancel  in

    update_timers (fun old -> Timeouts.add key trigger old);

    if not (Computation.try_attach computation trigger) then
      Trigger.signal trigger;
    let btexn =
      try
        Picos_io_select.cancel_after computation ~seconds exn bt;
        None
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Some (bt, exn)
    in
    match btexn with
    | None ->
      return () ~explicit_span cont
    | Some (bt, exn) ->
        discontinue_ exn bt ~explicit_span cont
  in
  let effc :
      type a. a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option
      = function
    | Fiber.Current -> handle_current
    | Fiber.Spawn { fiber; main } -> handle_spawn fiber main
    | Fiber.Yield -> handle_yield
    | Trigger.Await trigger -> handle_await trigger
    | Computation.Cancel_after {seconds; exn; bt; computation} -> handle_cancel_after seconds exn bt computation
    | _ -> None
  and finally () = Trace.exit_manual_span parent in
  Fun.protect ~finally @@ fun () ->
  Effect.Deep.match_with main current { retc; exnc = fatal_exn_handler; effc }

let run_fiber ?(fatal_exn_handler = exnc) current main =
  (* otherwise Random.self_init overrides our seed... *)
  Picos_aux_choice.init ();
  let parent =
    Trace.enter_manual_toplevel_span ~flavor:`Async ~__FUNCTION__ ~__LINE__
      ~__FILE__ "run_fiber"
  in
  let finally () = Trace.exit_manual_span parent in
  Fun.protect ~finally @@ fun () ->
  run_fiber ~fatal_exn_handler ~parent current main ()

let run ?(forbid = false) main =
  let mode = if Picos_aux_choice.bool () then `LIFO else `FIFO in
  let computation = Computation.create ~mode () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber fiber main;
  Computation.peek_exn computation
