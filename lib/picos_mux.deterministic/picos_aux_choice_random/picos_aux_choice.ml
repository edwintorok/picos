let make_seed () =
	match Sys.getenv_opt "QCHECK_SEED" with
	| Some seed -> int_of_string seed
	| None ->
		Random.self_init ();
		Random.bits ()

let () =
	let seed = make_seed () in
	Printf.eprintf "QCHECK_SEED=%d\n" seed

let bits = Random.bits
let bool = Random.bool
