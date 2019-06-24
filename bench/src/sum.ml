open! Core
open Import

(* Sums up a list of incrementals in a tree-like fashion *)
let incr_list_sum l =
  match List.reduce_balanced l ~f:(Incr.map2 ~f:( +. )) with
  | None -> return 0.
  | Some x -> x
;;

let len = 100_000

(* Each of the tests below creates a collection of inputs and some form of incremental sum
   of those inputs.  The test then modifies a single cell, and stabilizes the computation
   and gets the result *)
let%bench_fun "tree" =
  let open Infix in
  let inputs = Array.init len ~f:(fun _ -> Incr.Var.create 0.) in
  let sum =
    Incr.observe (incr_list_sum (Array.to_list inputs |> List.map ~f:Var.watch))
  in
  fun () ->
    let i = Random.int len in
    inputs.(i) := !(inputs.(i)) +. Random.float 1.0;
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float)
;;

(* This test uses incrementals built-in array fold.  *)
let%bench_fun "array_fold" =
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
  fun () ->
    let i = Random.int len in
    inputs.(i) := !(inputs.(i)) +. Random.float 1.0;
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float)
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

let%bench_fun "incr_map" =
  let open Infix in
  let inputs =
    Var.create (Map.of_alist_exn (module Int) (List.init len ~f:(fun i -> i, 0.)))
  in
  let sum = Incr.observe (sum_map (Var.watch inputs)) in
  fun () ->
    let i = Random.int len in
    let change = Random.float 1.0 in
    inputs
    := Map.update !inputs i ~f:(function
      | None -> change
      | Some x -> x +. change);
    Incr.stabilize ();
    ignore (Obs.value_exn sum : float)
;;

(* Here we just do a simple fold over the entire array to get the sum. *)
let%bench_fun "ord" =
  let inputs = Array.init len ~f:(fun _ -> 0.) in
  let sum () =
    let acc = ref 0.0 in
    for i = 0 to Array.length inputs - 1 do
      acc := !acc +. inputs.(i)
    done;
    !acc
  in
  fun () ->
    let i = Random.int len in
    inputs.(i) <- inputs.(i) +. Random.float 1.0;
    ignore (sum () : float)
;;

(* A run of the above benchmark included below.  You can see that the incr_map version is
   faster than the tree sum, but it does allocate more.  The ordinary all-at-once
   computation is 50-100x slower than the incremental ones, unsurprisingly.

   {v
┌─────────────────────┬──────────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                │     Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├─────────────────────┼──────────────┼─────────┼──────────┼──────────┼────────────┤
│ [sum.ml] tree       │   2_691.56ns │  37.72w │    6.27w │    6.27w │      2.46% │
│ [sum.ml] array_fold │     676.13ns │   7.96w │    1.25w │    1.25w │      0.62% │
│ [sum.ml] incr_map   │   1_577.01ns │ 182.66w │   21.89w │   21.89w │      1.44% │
│ [sum.ml] ord        │ 109_582.98ns │   4.00w │          │          │    100.00% │
└─────────────────────┴──────────────┴─────────┴──────────┴──────────┴────────────┘

    v} *)
