open! Core
module Incr = Incremental.Make ()
open Incr_map_collate

let test_rank_range ~data ~rank_range =
  let map = List.mapi data ~f:(fun i x -> sprintf "%d" i, x) |> String.Map.of_alist_exn in
  let map_var = Incr.Var.create map in
  let collate_params =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range = All_rows
      ; rank_range
      ; widen_range_by = 0, 0
      }
  in
  let collated =
    Incr_map_collate.collate
      ~filter_equal:Unit.equal
      ~order_equal:Unit.equal
      ~filter_to_predicate:(fun () -> None)
      ~order_to_compare:(fun () -> Compare.Unchanged)
      (Incr.Var.watch map_var)
      (Incr.Var.watch collate_params)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.stabilize ();
  let result = Incr.Observer.value_exn collated in
  print_s
    [%message
      ""
        ~data:(List.length data : int)
        ~rank_range:(rank_range : Collate_params.Rank.t Collate_params.Which_range.t)
        ~result:(Collated.to_alist result |> List.map ~f:snd : int list)
        ~num_before_range:(Collated.num_before_range result : int)]
;;

(* These tests demonstrate the behavior of rank ranges with From_end indices, including
   cases where the indices result in negative positions. *)

let%expect_test "Between with From_end indices" =
  (* 8 elements, Between (From_end 10) (From_end 4) = Between (-3) 3 *)
  test_rank_range
    ~data:[ 0; 1; 2; 3; 4; 5; 6; 7 ]
    ~rank_range:(Between (From_end 10, From_end 4));
  [%expect
    {|
    ((data 8) (rank_range (Between (From_end 10) (From_end 4)))
     (result (0 1 2 3)) (num_before_range 0))
    |}];
  (* 8 elements, Between (From_end 2) (From_end 0) = Between 5 7 *)
  test_rank_range
    ~data:[ 0; 1; 2; 3; 4; 5; 6; 7 ]
    ~rank_range:(Between (From_end 2, From_end 0));
  [%expect
    {|
    ((data 8) (rank_range (Between (From_end 2) (From_end 0))) (result (5 6 7))
     (num_before_range 5))
    |}];
  (* 4 elements, Between (From_start 1) (From_end 8) = Between 1 (-5) *)
  test_rank_range ~data:[ 0; 1; 2; 3 ] ~rank_range:(Between (From_start 1, From_end 8));
  [%expect
    {|
    ((data 4) (rank_range (Between (From_start 1) (From_end 8))) (result ())
     (num_before_range 1))
    |}]
;;

let%expect_test "To with From_end indices" =
  (* 1 element, To (From_end 9) = To (-9) *)
  test_rank_range ~data:[ 0 ] ~rank_range:(To (From_end 9));
  [%expect
    {| ((data 1) (rank_range (To (From_end 9))) (result ()) (num_before_range 0)) |}];
  (* 5 elements, To (From_end 2) = To 2 *)
  test_rank_range ~data:[ 0; 1; 2; 3; 4 ] ~rank_range:(To (From_end 2));
  [%expect
    {|
    ((data 5) (rank_range (To (From_end 2))) (result (0 1 2))
     (num_before_range 0))
    |}]
;;

let%expect_test "From with From_end indices" =
  (* 3 elements, From (From_end 5) = From (-3) *)
  test_rank_range ~data:[ 0; 1; 2 ] ~rank_range:(From (From_end 5));
  [%expect
    {|
    ((data 3) (rank_range (From (From_end 5))) (result (0 1 2))
     (num_before_range 0))
    |}];
  (* 5 elements, From (From_end 1) = From 3 *)
  test_rank_range ~data:[ 0; 1; 2; 3; 4 ] ~rank_range:(From (From_end 1));
  [%expect
    {|
    ((data 5) (rank_range (From (From_end 1))) (result (3 4))
     (num_before_range 3))
    |}]
;;
