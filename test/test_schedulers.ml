open Picos
open Picos_std_finally
open Picos_std_structured
open Picos_std_sync

let empty_bt = Printexc.get_callstack 0

let test_returns () =
  let actual = Test_scheduler.run @@ fun () -> 42 in
  assert (actual = 42)

let test_completes () =
  let packed = ref (Computation.Packed (Computation.create ())) in
  let result =
    Test_scheduler.run (fun () ->
        packed := Fiber.get_computation (Fiber.current ());
        101)
  in
  assert (result = 101);
  let (Packed computation) = !packed in
  assert (not (Computation.is_running computation));
  assert (not (Computation.is_canceled computation));
  begin
    match
      Test_scheduler.run (fun () ->
          packed := Fiber.get_computation (Fiber.current ());
          (failwith "42" : unit))
    with
    | () -> assert false
    | exception Failure msg -> assert (msg = "42")
    | exception e ->
      Printexc.print_backtrace stderr;
      Alcotest.V1.failf "This exception is not expected here: %s" (Printexc.to_string e)
  end;
  let (Packed computation) = !packed in
  assert (not (Computation.is_running computation));
  assert (Computation.is_canceled computation)

let test_current () =
  Test_scheduler.run ~max_domains:2 @@ fun () ->
  let fiber_parent = Fiber.current () in
  let fiber_child = ref fiber_parent in
  Flock.join_after @@ fun () ->
  Flock.fork (fun () -> fiber_child := Fiber.current ());
  while fiber_parent == !fiber_child do
    Control.yield ()
  done

let test_cancel_after_basic () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  Computation.cancel_after computation ~seconds:0.1 Exit empty_bt;
  Computation.wait computation

let test_cancel_after_long_timeout () =
  Test_scheduler.run @@ fun () ->
  let computation = Computation.create () in
  match Computation.cancel_after computation ~seconds:10e10 Exit empty_bt with
  | () -> Computation.finish computation
  | exception Invalid_argument _ -> ()

let test_op_raises_when_canceled () =
  Test_scheduler.run @@ fun () ->
  Flock.join_after @@ fun () ->
  let ivar = Ivar.create () in
  let wait () = Control.protect (fun () -> Ivar.read ivar) in
  [
    begin
      fun () ->
        Flock.fork_as_promise @@ fun () ->
        wait ();
        match Trigger.await (Trigger.create ()) with
        | None -> assert false
        | Some _ -> ()
    end;
    begin
      fun () ->
        Flock.fork_as_promise @@ fun () ->
        wait ();
        match
          Computation.cancel_after (Computation.create ()) ~seconds:1.0 Exit
            empty_bt
        with
        | () -> assert false
        | exception Control.Terminate -> ()
    end;
    begin
      fun () ->
        Flock.fork_as_promise @@ fun () ->
        wait ();
        match Fiber.yield () with
        | () -> assert false
        | exception Control.Terminate -> ()
    end;
    begin
      fun () ->
        Flock.fork_as_promise @@ fun () ->
        wait ();
        match
          Fiber.spawn
            (Fiber.create ~forbid:false (Computation.create ()))
            ignore
        with
        | () -> assert false
        | exception Control.Terminate -> ()
    end;
  ]
  |> Test_util.shuffle
  |> List.map (fun op -> op ())
  |> List.iter Promise.terminate;
  Ivar.fill ivar ()

let test_fatal () =
  match
    let computation = Computation.create () in
    let fiber = Fiber.create ~forbid:false computation in
    let fatal_exn_handler exn =
      let bt = Printexc.get_raw_backtrace () in
      Computation.cancel computation exn bt;
      Printexc.raise_with_backtrace exn bt
    in
    Test_scheduler.run_fiber ~fatal_exn_handler ~max_domains:3 fiber @@ fun _ ->
    for _ = 1 to 100 do
      Fiber.spawn (Fiber.create ~forbid:false computation) @@ fun _ ->
      while true do
        Fiber.yield ()
      done
    done;
    Fiber.spawn (Fiber.create ~forbid:false computation) (fun _ ->
        failwith "fatal");
    while true do
      Fiber.yield ()
    done
  with
  | _ -> assert false
  | exception Failure msg -> assert (msg = "fatal")

let test_cross_scheduler_wakeup () =
  let deadline = Unix.gettimeofday () +. 60.0 in
  let n_wakeups = 10_000 in
  for _ = 1 to 100 do
    if Unix.gettimeofday () < deadline then begin
      let trigger = Atomic.make None in
      let barrier = Atomic.make 2 in
      let wait () =
        Atomic.decr barrier;
        while Atomic.get barrier <> 0 do
          Domain.cpu_relax ()
        done
      in
      let awaiter () =
        for _ = 1 to n_wakeups do
          let t = Trigger.create () in
          Atomic.set trigger (Some t);
          Trigger.await t |> ignore
        done
      in
      let signaler () =
        for _ = 1 to n_wakeups do
          while
            match Atomic.get trigger with
            | None -> true
            | Some t ->
                Atomic.set trigger None;
                Backoff.once Backoff.default |> ignore;
                Trigger.signal t;
                false
          do
            Domain.cpu_relax ()
          done
        done
      in
      let which = Random.bool () in
      let@ _other =
        finally Domain.join @@ fun () ->
        Domain.spawn @@ fun () ->
        Test_scheduler.run @@ fun () ->
        wait ();
        if which then signaler () else awaiter ()
      in
      Test_scheduler.run @@ fun () ->
      wait ();
      if which then awaiter () else signaler ()
    end
  done

let () =
  [
    ("Trivial main returns", [ Alcotest.test_case "" `Quick test_returns ]);
    ( "Scheduler completes main computation",
      [ Alcotest.test_case "" `Quick test_completes ] );
    ("Current", [ Alcotest.test_case "" `Quick test_current ]);
    ( "Cancel_after",
      [
        Alcotest.test_case "basic" `Quick test_cancel_after_basic;
        Alcotest.test_case "long timeout" `Quick test_cancel_after_long_timeout;
      ] );
    ( "Operation on canceled fiber raises",
      [ Alcotest.test_case "" `Quick test_op_raises_when_canceled ] );
    ( "Cross scheduler wakeup",
      [ Alcotest.test_case "" `Quick test_cross_scheduler_wakeup ] );
    (* The fatal exn test must be kept last, because it may leave some
       schedulers in a bad state. *)
    ( "Fatal exception terminates scheduler",
      [ Alcotest.test_case "" `Quick test_fatal ] );
  ]
  |> Alcotest.run "Picos schedulers"
