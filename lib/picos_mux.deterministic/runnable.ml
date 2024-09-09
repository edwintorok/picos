module ChoiceMap = Map.Make (Int)

(* we only use a single thread to work better with afl,
   but be domain-safe in case this is used from another testing framework
*)

type !'a t = {
  map : 'a ChoiceMap.t Atomic.t;
  wait_mutex : Mutex.t;
  wait_cond : Condition.t;
}

let create () =
  {
    map = Atomic.make ChoiceMap.empty;
    wait_mutex = Mutex.create ();
    wait_cond = Condition.create ();
  }

let choose = Picos_aux_choice.bool
let cardinal t = Atomic.get t.map |> ChoiceMap.cardinal

(* We choose a position both when inserting and removing.
   We do not want a fully uniform distribution, because the chance of emulating FIFO or LIFO is quite low.
   But we want to test these common modes too, in addition to the uniformly random one.
   So give the special ones higher chance:
   25% FIFO
   25% LIFO
   50% other uniformly random
*)
let choose_deterministic_insert_position map =
  if choose () then
    (* there is a very low chance this would overflow, but accept that as inserting at random *)
    let special, delta =
      if choose () then (ChoiceMap.min_binding_opt map, -1)
      else (ChoiceMap.max_binding_opt map, 1)
    in
    match special with None -> 0 | Some (pos, _) -> pos + delta
  else Picos_aux_choice.bits ()

let rec choose_insert_position t pos =
  if ChoiceMap.mem pos t then choose_insert_position t (pos + 1) else pos

let choose_insert_position map =
  choose_insert_position map (choose_deterministic_insert_position map)

let choose_existing_item map =
  if choose () then
    if choose () then ChoiceMap.min_binding_opt map
    else ChoiceMap.max_binding_opt map
  else
    let position = Picos_aux_choice.bits () in
    match ChoiceMap.find_first_opt (( >= ) position) map with
    | Some item -> Some item
    | None ->
        (* TODO: get min, max and restrict position to that *)
        ChoiceMap.choose_opt map

let rec push t e =
  let prev = Atomic.get t.map in
  let pos = choose_insert_position prev in
  let next = ChoiceMap.add pos e prev in
  if Atomic.compare_and_set t.map prev next then begin
    Mutex.lock t.wait_mutex;
    Condition.broadcast t.wait_cond;
    Mutex.unlock t.wait_mutex
  end
  else begin
    Domain.cpu_relax ();
    (push [@tailcall]) t e
  end

let rec pop_opt t =
  let prev = Atomic.get t.map in
  match choose_existing_item prev with
  | None ->
    assert (ChoiceMap.is_empty prev);
    None
  | Some (pos, item) ->
      let next = ChoiceMap.remove pos prev in
      if Atomic.compare_and_set t.map prev next then Some item
      else begin
        Domain.cpu_relax ();
        (pop_opt [@tailcall]) t
      end

let rec pop t =
  match pop_opt t with
  | Some element -> element
  | None ->
      Mutex.lock t.wait_mutex;
      while ChoiceMap.is_empty (Atomic.get t.map) do
        Condition.wait t.wait_cond t.wait_mutex
      done;
      Mutex.unlock t.wait_mutex;
      (pop [@tailcall]) t

let rec iter_up_to t f n =
  if n > 0 then
    match pop_opt t with
    | None -> ()
    | Some item ->
        f item;
        (iter_up_to [@tailcall]) t f (n - 1)

let iter t f =
  let n =
    if choose () then if choose () then 0 else Int.max_int
    else
      let n = Atomic.get t.map |> ChoiceMap.cardinal in
      if n = 0 then 0
      else
        (* can AFL see this to guide fuzzing? *)
        Picos_aux_choice.bits () mod n
  in
  iter_up_to t f n
