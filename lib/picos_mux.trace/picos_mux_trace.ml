open Picos

let[@inline always][@specialise always] with_async_span ~data continue discontinue ~parent ~__LINE__ name fn cont =
  let _data () = data cont in
  let explicit_span = Trace.enter_manual_sub_span (*~data*) ~parent ~flavor:`Async ~__FILE__ ~__LINE__ name in
  match fn ~parent:explicit_span with
  | result ->
    Trace.exit_manual_span explicit_span;
    continue cont result
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    let data () =
      (* https://opentelemetry.io/docs/specs/semconv/exceptions/exceptions-spans/ *)
      [ "exception.message", `String (Printexc.to_string exn)
      ; "exception.type", `String (Printexc.exn_slot_name exn)
      ; "exception.escaped", `Bool true
      ; "exception.stacktrace", `String (Printexc.raw_backtrace_to_string bt)
      ]
    in
    Trace.message ~span:explicit_span.Trace.span ~data "exception";
    Trace.exit_manual_span parent;
    discontinue cont exn bt

let continue () x = x
let discontinue () exn bt = Printexc.raise_with_backtrace exn bt
let no_data () = []

let continuation_data cont =
  ["stack", `String (Effect.Deep.get_callstack cont 32 |> Printexc.raw_backtrace_to_string)]

let rec run_fiber ~name ~fatal_exn_handler ~parent (current : Fiber.t) main =
  with_async_span ~data:no_data continue discontinue ~parent ~__LINE__ name @@ fun ~parent ->
  let[@inline always][@specialise always] with_next_handler name ~__LINE__ fn =
    let[@inline always][@specialise always] fn ~parent:_ = fn () in
    Some (with_async_span ~data:continuation_data Effect.Deep.continue Effect.Deep.discontinue_with_backtrace ~parent ~__LINE__ name fn)
  in
  let effc :
      type a. a Effect.t -> ((a, unit) Effect.Deep.continuation -> unit) option
      = function
    | Fiber.Current ->
      with_next_handler "Fiber.Current" ~__LINE__ Fiber.current
    | Fiber.Spawn { fiber; main } ->
      let main fiber = 
        run_fiber ~name:"Fiber.Spawn(child)" ~fatal_exn_handler ~parent fiber main ()
      in
      with_next_handler "Fiber.Spawn(parent)" ~__LINE__ (fun () -> Fiber.spawn fiber main)
    | Fiber.Yield -> with_next_handler "Fiber.Yield" ~__LINE__ Fiber.yield
    | Trigger.Await trigger ->
       with_next_handler "Fiber.Await" ~__LINE__ (fun () -> Trigger.await trigger)
    | Computation.Cancel_after {seconds; exn; bt; computation} ->
      with_next_handler "Computation.Cancel_after" ~__LINE__ (fun () ->
        Computation.cancel_after computation ~seconds exn bt
      )
    | _ -> None
  in
  Effect.Deep.match_with main current { retc = Fun.id; exnc = fatal_exn_handler; effc }

let run_fiber ?parent ?(fatal_exn_handler = raise) current main =
  let parent =
    match parent with
    | None -> Trace.enter_manual_toplevel_span ~flavor:`Async ~__LINE__ ~__FILE__ "run_fiber"
    | Some p -> p
  in
  let finally () = Trace.exit_manual_span parent in
  Fun.protect ~finally @@ fun () ->
  run_fiber ~name:"run_fiber" ~fatal_exn_handler ~parent current main ()

let run ?(mode=`FIFO) ?(forbid = false) main =
  let computation = Computation.create ~mode () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber fiber main;
  Computation.peek_exn computation
