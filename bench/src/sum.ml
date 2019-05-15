open! Core
open Import

(* Sums up a list of incrementals in a tree-like fashion *)
let incr_list_sum l =
  match List.reduce_balanced l ~f:(Incr.map2 ~f:( +. )) with
  | None -> return 0.
  | Some x -> x
;;

(* Each of the tests below creates a collection of inputs and some form of incremental sum
   of those inputs.  The test then modifies a single cell, and stabilizes the computation
   and gets the result *)

let tree_sum_test len =
  let open Infix in
  let inputs = Array.init len ~f:(fun _ -> Incr.Var.create 0.) in
  let sum =
    Incr.observe (incr_list_sum (Array.to_list inputs |> List.map ~f:Var.watch))
  in
  Bench.Test.create ~name:"tree" (fun () ->
    let i = Random.int len in
    inputs.(i) := !(inputs.(i)) +. Random.float 1.0;
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float))
;;

(* This test uses incrementals built-in array fold.  *)
let array_fold_test len =
  let open Infix in
  let inputs = Array.init len ~f:(fun _ -> Incr.Var.create 0.) in
  let sum =
    Incr.observe
      (Incr.unordered_array_fold
         (Array.map inputs ~f:Var.watch)
         ~init:0.
         ~f:( +. )
         ~update:(F_inverse ( -. )))
  in
  Bench.Test.create ~name:"array_fold" (fun () ->
    let i = Random.int len in
    inputs.(i) := !(inputs.(i)) +. Random.float 1.0;
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float))
;;

(* This sums over an incremental map, using incr_map *)
let sum_map m =
  Incr_map.unordered_fold
    m
    ~init:0.
    ~add:(fun ~key:_ ~data:x sum -> sum +. x)
    ~remove:(fun ~key:_ ~data:x sum -> sum -. x)
    ~update:(fun ~key:_ ~old_data ~new_data sum -> sum -. old_data +. new_data)
;;

let incr_map_test len =
  let open Infix in
  let inputs =
    Var.create (Map.of_alist_exn (module Int) (List.init len ~f:(fun i -> i, 0.)))
  in
  let sum = Incr.observe (sum_map (Var.watch inputs)) in
  Bench.Test.create ~name:"incr_map" (fun () ->
    let i = Random.int len in
    let change = Random.float 1.0 in
    inputs :=
      Map.update !inputs i ~f:(function
        | None -> change
        | Some x -> x +. change);
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float))
;;

(* Here we just do a simple fold over the entire array to get the sum. *)
let all_at_once_test len =
  let inputs = Array.init len ~f:(fun _ -> 0.) in
  let sum () =
    let acc = ref 0.0 in
    for i = 0 to Array.length inputs - 1 do
      acc := !acc +. inputs.(i)
    done;
    !acc
  in
  Bench.Test.create ~name:"ord" (fun () ->
    let i = Random.int len in
    inputs.(i) <- inputs.(i) +. Random.float 1.0;
    ignore (sum () : float))
;;

let command () =
  let len = 100_000 in
  Bench.make_command
    [ tree_sum_test len; array_fold_test len; incr_map_test len; all_at_once_test len ]
;;

(* A run of the above benchmark included below.  You can see that the incr_map version is
   almost as fast as the tree sum, but it does allocate more.  The ordinary all-at-once
   computation is 50-100x slower than the incremental ones, unsurprisingly.

   {v
┌────────────┬──────────────┬─────────────┬──────────┬──────────┬────────────┐
│ Name       │     Time/Run │     mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├────────────┼──────────────┼─────────────┼──────────┼──────────┼────────────┤
│ tree       │   3_557.77ns │    -256.03w │   -1.70w │   -1.56w │      1.07% │
│ array_fold │     979.54ns │      10.00w │    1.27w │    1.27w │      0.29% │
│ incr_map   │   4_614.71ns │   1_389.90w │   38.10w │   38.10w │      1.38% │
│ ord        │ 333_760.67ns │ 400_005.53w │    1.15w │    1.15w │    100.00% │
└────────────┴──────────────┴─────────────┴──────────┴──────────┴────────────┘
    v} *)
