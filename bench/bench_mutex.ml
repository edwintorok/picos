open Multicore_bench
open Picos
open Picos_sync

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

(** This will keep a domain running. *)
let yielder computation =
  let main () =
    try
      while true do
        Fiber.yield ()
      done
    with Exit -> ()
  in
  Fiber.spawn ~forbid:false computation [ main ]

let run_one ~budgetf ~n_fibers ~use_domains () =
  let n_domains = if use_domains then n_fibers else 1 in
  let n_ops = (if use_domains then 10 else 100) * Util.iter_factor in

  let v = ref 0 in
  let n_ops_todo = Countdown.create ~n_domains () in
  let mutex = Mutex.create ~padded:true () in

  let batch = if use_domains || n_fibers < 16 then 1000 else 100 in

  let init _ =
    assert (!v = 0);
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let wrap _ () = Scheduler.run in
  let work domain_index () =
    let n_live = Atomic.make (if use_domains then 1 else n_fibers) in
    let computation = Computation.create () in
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch in
      if n <> 0 then
        let rec loop n =
          if 0 < n then begin
            Mutex.lock mutex;
            let x = !v in
            v := x + 1;
            Fiber.yield ();
            assert (!v = x + 1);
            v := x;
            Mutex.unlock mutex;
            loop (n - 1)
          end
          else work ()
        in
        loop n
      else if 1 = Atomic.fetch_and_add n_live (-1) then
        Computation.cancel computation (Exn_bt.get_callstack 0 Exit)
    in
    if use_domains then begin
      yielder computation;
      work ()
    end
    else begin
      List.init n_fibers (fun _ -> work)
      |> Fiber.spawn ~forbid:false computation;
      Computation.wait computation
    end
  in

  let config =
    Printf.sprintf "%d %s%s" n_fibers
      (if use_domains then "domain" else "fiber")
      (if n_fibers = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~wrap ~work
    ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"locked yield" ~config

let run_suite ~budgetf =
  Util.cross [ false; true ] [ 1; 2; 4; 8; 256; 512; 1024 ]
  |> List.concat_map @@ fun (use_domains, n_fibers) ->
     if
       use_domains
       && (n_fibers = 1 || Picos_domain.recommended_domain_count () < n_fibers)
       || (is_ocaml4 && 256 < n_fibers)
     then []
     else run_one ~budgetf ~n_fibers ~use_domains ()
