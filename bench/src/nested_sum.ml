open! Core
open Import

(* In this section, we're doing a summation over nested maps containing floats in the
   inner maps. *)

let sum_map = Sum.sum_map
let sum_map' m = sum_map (Incr_map.mapi' m ~f:(fun ~key:_ ~data -> sum_map data))

(* Sets an element in the inner map, given both outer and inner index *)
let set_el m o i v =
  Map.update m o ~f:(function
    | None -> Map.singleton (module Int) i v
    | Some m' -> Map.set m' ~key:i ~data:v)
;;

(* Initializes a map full of zeros for all combinations of inner and outer indices *)
let initialize ~outer ~inner =
  Sequence.fold
    (Sequence.range 0 outer)
    ~init:(Map.empty (module Int))
    ~f:(fun acc o ->
      Sequence.fold (Sequence.range 0 inner) ~init:acc ~f:(fun acc i -> set_el acc o i 0.))
;;

(* Computes the nested sum incrementally, using [sum_map']. *)
let nested_sum_raw ~outer ~inner =
  let open Infix in
  let env =
    lazy
      (let input = Var.create (initialize ~outer ~inner) in
       let sum = Incr.observe (sum_map' (Var.watch input)) in
       input, sum)
  in
  let run () =
    let input, sum = force env in
    let o = Random.int outer in
    let i = Random.int inner in
    input := set_el !input o i (Random.float 1.0);
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float)
  in
  let name = sprintf "(%d,%d)" outer inner in
  name, run
;;

let nested_sum ~outer ~inner =
  let name, run = nested_sum_raw ~outer ~inner in
  Bench.Test.create ~name run
;;

(* Does the nested sum in an all-at-once way, using ordinary map folds *)
let nested_sum_ord_raw ~outer ~inner =
  let input = lazy (ref (initialize ~outer ~inner)) in
  let sum () =
    let input = force input in
    Map.fold !input ~init:0. ~f:(fun ~key:_ ~data:m acc ->
      Map.fold m ~init:acc ~f:(fun ~key:_ ~data:x acc -> x +. acc))
  in
  let name = sprintf "(%d,%d) ord" outer inner in
  let run () =
    let input = force input in
    let o = Random.int outer in
    let i = Random.int inner in
    input := set_el !input o i (Random.float 1.0);
    ignore (sum ())
  in
  name, run
;;

let nested_sum_ord ~outer ~inner =
  let name, run = nested_sum_ord_raw ~outer ~inner in
  Bench.Test.create ~name run
;;

let command () =
  Bench.make_command
    [ nested_sum ~outer:10000 ~inner:10
    ; nested_sum ~outer:1000 ~inner:100
    ; nested_sum ~outer:100 ~inner:1000
    ; nested_sum ~outer:10 ~inner:10000
    ; nested_sum_ord ~outer:1000 ~inner:100
    ]
;;

(* Looks like it doesn't matter all that much how you choose to structure the nested maps,
   though more outer elements clearly adds expense.  Note also that despite the decently
   large cost of the incremental update, the all-at-once computation is 100x worse.

   {v
    ┌────────────────┬──────────┬──────────┬──────────┬──────────┬────────────┐
    │ Name           │ Time/Run │  mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
    ├────────────────┼──────────┼──────────┼──────────┼──────────┼────────────┤
    │ (10000,10)     │   9.54us │   2.12kw │   69.87w │   69.94w │      1.28% │
    │ (1000,100)     │   6.41us │   1.51kw │   50.26w │   50.26w │      0.86% │
    │ (100,1000)     │   5.34us │   1.24kw │   38.76w │   38.76w │      0.72% │
    │ (10,10000)     │   5.10us │   1.29kw │   37.58w │   37.58w │      0.69% │
    │ (1000,100) ord │ 743.63us │ 200.16kw │   78.88w │   78.88w │    100.00% │
    └────────────────┴──────────┴──────────┴──────────┴──────────┴────────────┘
   v}
*)

let%expect_test "stats" =
  let stats = unstage (Stats.reporter ()) in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    0)) |}];
  let _name, run = nested_sum_raw ~outer:1000 ~inner:100 in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    0)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 2008)
     (changed    2008)
     (created    2008)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 7)
     (changed    6)
     (created    0)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 7)
     (changed    6)
     (created    0)) |}]
;;
