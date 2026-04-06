open! Core
open Expect_test_helpers_core
module Collated = Collate_protocol.Collated

let diff_int ~from ~to_ = exclave_ Diffable.Optional_diff.return (to_ - from)
let apply_int v d = v + d
let diff_string ~from:_ ~to_ = exclave_ Diffable.Optional_diff.return to_
let apply_string _ d = d

let get_diff diff_k diff_v ~from ~to_ =
  let local_ opt = Collated.Diff.get diff_k diff_v ~from ~to_ in
  if Diffable.Optional_diff.is_none opt
  then None
  else Some (Diffable.Optional_diff.unsafe_value opt)
;;

let create_collated ~data ~num_filtered_rows ~num_before_range ~num_unfiltered_rows =
  Collated.For_testing.of_list
    ~num_filtered_rows
    ~key_range:All_rows
    ~rank_range:All_rows
    ~num_before_range
    ~range_widened_by:(0, 0)
    ~num_unfiltered_rows
    data
;;

let%expect_test "Diff.get returns None when from and to are equal" =
  let t =
    create_collated
      ~data:[ "a", 1; "b", 2 ]
      ~num_filtered_rows:2
      ~num_before_range:0
      ~num_unfiltered_rows:5
  in
  let diff = get_diff diff_string diff_int ~from:t ~to_:t in
  print_s ([%sexp_of: (string, int, string, int) Collated.Diff.t option] diff);
  [%expect {| () |}]
;;

let%expect_test "roundtrip: apply_exn (get ~from ~to_) from = to_" =
  let from =
    create_collated
      ~data:[ "a", 1; "b", 2 ]
      ~num_filtered_rows:2
      ~num_before_range:0
      ~num_unfiltered_rows:5
  in
  let to_ =
    create_collated
      ~data:[ "a", 10; "c", 3 ]
      ~num_filtered_rows:3
      ~num_before_range:1
      ~num_unfiltered_rows:10
  in
  print_s [%message (from : (string, int) Collated.t)];
  [%expect
    {|
    (from (
      (data (
        (0   (a 1))
        (100 (b 2))))
      (num_filtered_rows 2)
      (key_range         All_rows)
      (rank_range        All_rows)
      (num_before_range  0)
      (range_widened_by (0 0))
      (num_unfiltered_rows 5)))
    |}];
  print_s [%message (to_ : (string, int) Collated.t)];
  [%expect
    {|
    (to_ (
      (data (
        (0   (a 10))
        (100 (c 3))))
      (num_filtered_rows 3)
      (key_range         All_rows)
      (rank_range        All_rows)
      (num_before_range  1)
      (range_widened_by (0 0))
      (num_unfiltered_rows 10)))
    |}];
  let diff = get_diff diff_string diff_int ~from ~to_ in
  print_s [%message (diff : (string, int, string, int) Collated.Diff.t option)];
  [%expect
    {|
    (diff ((
      (Data (
        (Diff 100 ((T1 c) (T2 1)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10))))
    |}];
  let diff = Option.value_exn diff in
  let result = Collated.Diff.apply_exn apply_string apply_int from diff in
  print_s [%message (result : (string, int) Collated.t)];
  [%expect
    {|
    (result (
      (data (
        (0   (a 10))
        (100 (c 3))))
      (num_filtered_rows 3)
      (key_range         All_rows)
      (rank_range        All_rows)
      (num_before_range  1)
      (range_widened_by (0 0))
      (num_unfiltered_rows 10)))
    |}];
  assert ([%equal: (string, int) Collated.t] result to_)
;;

let%expect_test "roundtrip with key_range and rank_range changes" =
  let from =
    Collated.For_testing.of_list
      ~num_filtered_rows:10
      ~key_range:(Between ("a", "z"))
      ~rank_range:(Between (0, 100))
      ~num_before_range:5
      ~range_widened_by:(1, 2)
      ~num_unfiltered_rows:20
      [ "x", 100 ]
  in
  let to_ =
    Collated.For_testing.of_list
      ~num_filtered_rows:15
      ~key_range:(From "m")
      ~rank_range:(From 50)
      ~num_before_range:7
      ~range_widened_by:(3, 4)
      ~num_unfiltered_rows:25
      [ "y", 200 ]
  in
  let diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  let result = Collated.Diff.apply_exn apply_string apply_int from diff in
  assert ([%equal: (string, int) Collated.t] result to_)
;;

let%expect_test "empty to non-empty roundtrip" =
  let from = Collated.empty in
  let to_ =
    create_collated
      ~data:[ "a", 1; "b", 2; "c", 3 ]
      ~num_filtered_rows:10
      ~num_before_range:3
      ~num_unfiltered_rows:20
  in
  let diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  let result = Collated.Diff.apply_exn apply_string apply_int from diff in
  assert ([%equal: (string, int) Collated.t] result to_)
;;

let%expect_test "non-empty to empty roundtrip" =
  let from =
    create_collated
      ~data:[ "a", 1; "b", 2; "c", 3 ]
      ~num_filtered_rows:10
      ~num_before_range:3
      ~num_unfiltered_rows:20
  in
  let to_ = Collated.empty in
  let diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  let result = Collated.Diff.apply_exn apply_string apply_int from diff in
  assert ([%equal: (string, int) Collated.t] result to_)
;;

(* Versioning tests *)

let%expect_test "V2.Diff.of_v1 roundtrip" =
  let from =
    create_collated
      ~data:[ "a", 1; "b", 2 ]
      ~num_filtered_rows:2
      ~num_before_range:0
      ~num_unfiltered_rows:5
  in
  let to_ =
    create_collated
      ~data:[ "a", 10; "c", 3 ]
      ~num_filtered_rows:3
      ~num_before_range:1
      ~num_unfiltered_rows:10
  in
  (* Get V2 diff, convert to V1, convert back to V2, and apply *)
  let v2_diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  let v1_diff = Collated.Stable.V2.Diff.to_v1 v2_diff in
  let v2_diff_roundtripped = Collated.Stable.V2.Diff.of_v1 v1_diff in
  print_s [%message (v2_diff : (string, int, string, int) Collated.Diff.t)];
  [%expect
    {|
    (v2_diff (
      (Data (
        (Diff 100 ((T1 c) (T2 1)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10)))
    |}];
  print_s
    [%message
      (v2_diff_roundtripped : (string, int, string, int) Collated.Stable.V2.Diff.t)];
  [%expect
    {|
    (v2_diff_roundtripped (
      (Data (
        (Diff 100 ((T1 c) (T2 1)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10)))
    |}];
  (* Should be equal since range_widened_by didn't change *)
  let result = Collated.Diff.apply_exn apply_string apply_int from v2_diff_roundtripped in
  assert ([%equal: (string, int) Collated.t] result to_)
;;

let%expect_test "V2.Diff.to_v1 drops Range_widened_by diffs" =
  (* Create values where range_widened_by differs *)
  let from =
    Collated.For_testing.of_list
      ~num_filtered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      ~range_widened_by:(0, 0)
      ~num_unfiltered_rows:20
      [ "a", 1 ]
  in
  let to_ =
    Collated.For_testing.of_list
      ~num_filtered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      ~range_widened_by:(5, 10)
      ~num_unfiltered_rows:20
      [ "a", 1 ]
  in
  let v2_diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  print_s [%message "V2 diff" (v2_diff : (string, int, string, int) Collated.Diff.t)];
  [%expect
    {|
    ("V2 diff" (
      v2_diff ((
        Range_widened_by (
          (T1 5)
          (T2 10))))))
    |}];
  (* Converting to V1 drops Range_widened_by *)
  let v1_diff = Collated.Stable.V2.Diff.to_v1 v2_diff in
  print_s
    [%message "V1 diff" (v1_diff : (string, int, string, int) Collated.Stable.V1.Diff.t)];
  [%expect {| ("V1 diff" (v1_diff ())) |}];
  (* Applying the V1->V2 roundtripped diff loses the range_widened_by update *)
  let v2_diff_roundtripped = Collated.Stable.V2.Diff.of_v1 v1_diff in
  let result = Collated.Diff.apply_exn apply_string apply_int from v2_diff_roundtripped in
  (* Result will NOT equal to_ because range_widened_by was lost *)
  print_s
    [%message
      "range_widened_by after roundtrip"
        ~from_original:(Collated.range_widened_by to_ : int * int)
        ~after_roundtrip:(Collated.range_widened_by result : int * int)];
  [%expect
    {|
    ("range_widened_by after roundtrip"
      (from_original   (5 10))
      (after_roundtrip (0 0)))
    |}]
;;

(* Diff.map tests *)

let%expect_test "Diff.map roundtrip: int -> string -> int" =
  let from =
    create_collated
      ~data:[ "a", 1; "b", 2 ]
      ~num_filtered_rows:2
      ~num_before_range:0
      ~num_unfiltered_rows:5
  in
  let to_ =
    create_collated
      ~data:[ "a", 10; "c", 30 ]
      ~num_filtered_rows:3
      ~num_before_range:1
      ~num_unfiltered_rows:10
  in
  let int_diff = Option.value_exn (get_diff diff_string diff_int ~from ~to_) in
  print_s [%message (int_diff : (string, int, string, int) Collated.Diff.t)];
  [%expect
    {|
    (int_diff (
      (Data (
        (Diff 100 ((T1 c) (T2 28)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10)))
    |}];
  (* Convert to string *)
  let string_diff =
    Collated.Diff.map
      int_diff
      ~f_value:Int.to_string
      ~f_value_diff:Int.to_string
      ~f_key:Fn.id
      ~f_key_diff:Fn.id
  in
  print_s [%message (string_diff : (string, string, string, string) Collated.Diff.t)];
  [%expect
    {|
    (string_diff (
      (Data (
        (Diff 100 ((T1 c) (T2 28)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10)))
    |}];
  (* Convert back to int *)
  let back_to_int =
    Collated.Diff.map
      string_diff
      ~f_value:Int.of_string
      ~f_value_diff:Int.of_string
      ~f_key:Fn.id
      ~f_key_diff:Fn.id
  in
  print_s [%message (back_to_int : (string, int, string, int) Collated.Diff.t)];
  [%expect
    {|
    (back_to_int (
      (Data (
        (Diff 100 ((T1 c) (T2 28)))
        (Diff 0   ((T1 a) (T2 9)))))
      (Num_filtered_rows   3)
      (Num_before_range    1)
      (Num_unfiltered_rows 10)))
    |}];
  (* Apply and verify result *)
  let result = Collated.Diff.apply_exn apply_string apply_int from back_to_int in
  assert ([%equal: (string, int) Collated.t] result to_)
;;
