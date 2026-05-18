open! Core
module Rank_from_start = Collate_shared.Rank_from_start
module Range = Rank_from_start.Range

let%expect_test "count_before" =
  let test ~lo ~hi =
    let bounds = Range.create ~lo ~hi in
    print_s [%message (Range.count_before bounds : int)]
  in
  test ~lo:Unbounded ~hi:Unbounded;
  [%expect {| ("Range.count_before bounds" 0) |}];
  test ~lo:(Incl 3) ~hi:Unbounded;
  [%expect {| ("Range.count_before bounds" 3) |}];
  test ~lo:(Excl 3) ~hi:Unbounded;
  [%expect {| ("Range.count_before bounds" 4) |}];
  test ~lo:(Incl (-5)) ~hi:Unbounded;
  [%expect {| ("Range.count_before bounds" 0) |}]
;;

let%expect_test "length" =
  let test ~lo ~hi ~data_length =
    let bounds = Range.create ~lo ~hi in
    print_s [%message (Range.length ~data_length bounds : int)]
  in
  test ~lo:Unbounded ~hi:Unbounded ~data_length:10;
  [%expect {| ("Range.length ~data_length bounds" 10) |}];
  test ~lo:(Incl 2) ~hi:(Incl 5) ~data_length:10;
  [%expect {| ("Range.length ~data_length bounds" 4) |}];
  test ~lo:(Incl 0) ~hi:(Excl 5) ~data_length:10;
  [%expect {| ("Range.length ~data_length bounds" 5) |}];
  test ~lo:Unbounded ~hi:(Incl 4) ~data_length:10;
  [%expect {| ("Range.length ~data_length bounds" 5) |}]
;;

let%expect_test "to_index_range" =
  let test ~lo ~hi ~data_length =
    let bounds = Range.create ~lo ~hi in
    let start, end_ = Range.to_index_range ~data_length bounds in
    print_s [%message (start : int) (end_ : int)]
  in
  test ~lo:Unbounded ~hi:Unbounded ~data_length:10;
  [%expect {| ((start 0) (end_ 10)) |}];
  test ~lo:(Incl 3) ~hi:(Incl 7) ~data_length:10;
  [%expect {| ((start 3) (end_ 8)) |}];
  test ~lo:(Excl 3) ~hi:(Excl 7) ~data_length:10;
  [%expect {| ((start 4) (end_ 7)) |}];
  test ~lo:(Incl 0) ~hi:(Incl 20) ~data_length:10;
  [%expect {| ((start 0) (end_ 10)) |}];
  test ~lo:(Incl (-5)) ~hi:(Incl 5) ~data_length:10;
  [%expect {| ((start 0) (end_ 6)) |}]
;;

let%expect_test "remove_basis" =
  let test ~basis_lo ~basis_hi ~lo ~hi =
    let basis = Range.create ~lo:basis_lo ~hi:basis_hi in
    let relative = Range.create ~lo ~hi in
    let result = Range.remove_basis ~basis relative in
    print_s [%message (result : Range.t)]
  in
  test ~basis_lo:(Incl 3) ~basis_hi:(Incl 7) ~lo:(Incl 1) ~hi:(Incl 2);
  [%expect {| (result ((Incl 4) (Incl 5))) |}];
  test ~basis_lo:(Incl 3) ~basis_hi:(Incl 7) ~lo:Unbounded ~hi:Unbounded;
  [%expect {| (result ((Incl 3) (Incl 7))) |}];
  test ~basis_lo:(Incl 3) ~basis_hi:(Incl 7) ~lo:(Incl 0) ~hi:(Incl 10);
  [%expect {| (result ((Incl 3) (Incl 7))) |}];
  test ~basis_lo:Unbounded ~basis_hi:Unbounded ~lo:(Incl 2) ~hi:(Incl 5);
  [%expect {| (result ((Incl 2) (Incl 5))) |}]
;;

let%expect_test "widen" =
  let test ~lo ~hi ~by ~data_length =
    let bounds = Range.create ~lo ~hi in
    let widened, (before, after) = Range.widen ~by ~data_length bounds in
    print_s [%message (widened : Range.t) (before : int) (after : int)]
  in
  test ~lo:(Incl 5) ~hi:(Incl 8) ~by:(2, 3) ~data_length:20;
  [%expect {| ((widened ((Incl 3) (Incl 11))) (before 2) (after 3)) |}];
  test ~lo:(Incl 1) ~hi:(Incl 5) ~by:(5, 0) ~data_length:10;
  [%expect {| ((widened ((Incl 0) (Incl 5))) (before 1) (after 0)) |}];
  test ~lo:(Incl 5) ~hi:(Incl 8) ~by:(0, 5) ~data_length:10;
  [%expect {| ((widened ((Incl 5) (Incl 9))) (before 0) (after 1)) |}];
  test ~lo:Unbounded ~hi:Unbounded ~by:(3, 3) ~data_length:10;
  [%expect {| ((widened (Unbounded Unbounded)) (before 0) (after 0)) |}]
;;

let%expect_test "of_which_rank_range" =
  let test rank_range ~data_length =
    let result = Range.of_which_rank_range ~data_length rank_range in
    print_s [%message (result : Range.t)]
  in
  test All_rows ~data_length:10;
  [%expect {| (result (Unbounded Unbounded)) |}];
  test (From (From_start 3)) ~data_length:10;
  [%expect {| (result ((Incl 3) Unbounded)) |}];
  test (To (From_start 7)) ~data_length:10;
  [%expect {| (result (Unbounded (Incl 7))) |}];
  test (Between (From_start 3, From_start 7)) ~data_length:10;
  [%expect {| (result ((Incl 3) (Incl 7))) |}];
  test (From (From_end 2)) ~data_length:10;
  [%expect {| (result ((Incl 7) Unbounded)) |}];
  test (Between (From_start 2, From_end 2)) ~data_length:10;
  [%expect {| (result ((Incl 2) (Incl 7))) |}]
;;
