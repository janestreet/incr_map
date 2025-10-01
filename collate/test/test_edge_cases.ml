open! Core
module Incr = Incremental.Make ()
open Incr_map_collate

let%expect_test "From_end with empty map" =
  let map = Incr.Var.create String.Map.empty in
  let collate_params =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range = All_rows
      ; rank_range = Between (From_end 2, From_start 10)
      ; widen_range_by = 0, 0
      }
  in
  let collated =
    Incr_map_collate.collate
      ~filter_equal:Unit.equal
      ~order_equal:Unit.equal
      ~filter_to_predicate:(fun () -> None)
      ~order_to_compare:(fun () -> Compare.Unchanged)
      (Incr.Var.watch map)
      (Incr.Var.watch collate_params)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn collated in
  print_s
    [%message
      "Empty map with From_end rank range"
        (Collated.to_alist result : (string * int) list)
        (Collated.num_before_range result : int)
        (Collated.num_filtered_rows result : int)
        (Collated.num_unfiltered_rows result : int)];
  [%expect
    {|
    ("Empty map with From_end rank range" ("Collated.to_alist result" ())
     ("Collated.num_before_range result" 0)
     ("Collated.num_filtered_rows result" 0)
     ("Collated.num_unfiltered_rows result" 0))
    |}]
;;

let%expect_test "From_end with small map" =
  let map = Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2 ]) in
  let collate_params =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range = All_rows
      ; rank_range = Between (From_end 5, From_start 10)
      ; widen_range_by = 0, 0
      }
  in
  let collated =
    Incr_map_collate.collate
      ~filter_equal:Unit.equal
      ~order_equal:Unit.equal
      ~filter_to_predicate:(fun () -> None)
      ~order_to_compare:(fun () -> Compare.Unchanged)
      (Incr.Var.watch map)
      (Incr.Var.watch collate_params)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn collated in
  print_s
    [%message
      "Small map with From_end rank range"
        (Collated.to_alist result : (string * int) list)
        (Collated.num_before_range result : int)
        (Collated.num_filtered_rows result : int)
        (Collated.num_unfiltered_rows result : int)];
  [%expect
    {|
    ("Small map with From_end rank range"
     ("Collated.to_alist result" ((a 1) (b 2)))
     ("Collated.num_before_range result" 0)
     ("Collated.num_filtered_rows result" 2)
     ("Collated.num_unfiltered_rows result" 2))
    |}]
;;

let%expect_test "Bug 2 fix: Correct num_before_range for non-existent keys" =
  (* Create a map with keys: ["a", "b", "c", "d", "e"] *)
  let map =
    Incr.Var.create (String.Map.of_alist_exn [ "a", 1; "b", 2; "c", 3; "d", 4; "e", 5 ])
  in
  (* Test case 1: Key exists in map *)
  let collate_params1 =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range = From "b" (* Start from "b" *)
      ; rank_range = All_rows
      ; widen_range_by = 0, 0
      }
  in
  let collated1 =
    Incr_map_collate.collate
      ~filter_equal:Unit.equal
      ~order_equal:Unit.equal
      ~filter_to_predicate:(fun () -> None)
      ~order_to_compare:(fun () -> Compare.Unchanged)
      (Incr.Var.watch map)
      (Incr.Var.watch collate_params1)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.stabilize ();
  let result1 = Incr.Observer.value_exn collated1 in
  print_s
    [%message
      "Key 'b' exists in map"
        (Collated.to_alist result1 : (string * int) list)
        (Collated.num_before_range result1 : int)];
  (* Test case 2: Key doesn't exist in map *)
  let collate_params2 =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range = From "b_and_a_half" (* Non-existent key between "b" and "c" *)
      ; rank_range = All_rows
      ; widen_range_by = 0, 0
      }
  in
  let collated2 =
    Incr_map_collate.collate
      ~filter_equal:Unit.equal
      ~order_equal:Unit.equal
      ~filter_to_predicate:(fun () -> None)
      ~order_to_compare:(fun () -> Compare.Unchanged)
      (Incr.Var.watch map)
      (Incr.Var.watch collate_params2)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.stabilize ();
  let result2 = Incr.Observer.value_exn collated2 in
  print_s
    [%message
      "Key 'b_and_a_half' doesn't exist in map"
        (Collated.to_alist result2 : (string * int) list)
        (Collated.num_before_range result2 : int)];
  [%expect
    {|
    ("Key 'b' exists in map"
     ("Collated.to_alist result1" ((b 2) (c 3) (d 4) (e 5)))
     ("Collated.num_before_range result1" 1))
    ("Key 'b_and_a_half' doesn't exist in map"
     ("Collated.to_alist result2" ((c 3) (d 4) (e 5)))
     ("Collated.num_before_range result2" 2))
    |}]
;;
