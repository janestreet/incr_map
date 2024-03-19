open! Core
open Expect_test_helpers_core
module Incr = Incremental.Make ()

let%expect_test _ =
  let data_var =
    Incr.Var.create (Int.Map.of_alist_exn (List.init 10 ~f:(fun i -> i, i)))
  in
  let collate_var =
    Incr.Var.create
      { Incr_map_collate.Collate.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Incr_map_collate.Compare.Unchanged
      ; key_range = All_rows
      ; rank_range = All_rows
      }
  in
  let t =
    Incr_map_collate.collate
      ~filter_equal:phys_equal
      ~order_equal:phys_equal
      ~filter_to_predicate:(fun f -> Some f)
      ~order_to_compare:Fn.id
      (Incr.Var.watch data_var)
      (Incr.Var.watch collate_var)
  in
  let collated_observer = Incr.observe (Incr_map_collate.collated t) in
  let key_rank_observer = Incr.observe (Incr_map_collate.key_rank t) in
  let stabilize_and_show () =
    Incr.stabilize ();
    let collated = Incr.Observer.value_exn collated_observer in
    let key_rank = Incr.Observer.value_exn key_rank_observer in
    let data_with_key_rank =
      Map.mapi (Incr.Var.value data_var) ~f:(fun ~key ~data -> data, key_rank key)
    in
    print_s
      [%message
        (collated : (int, int) Incr_map_collate.Collated.t)
          (data_with_key_rank : (int * int option) Int.Map.t)]
  in
  stabilize_and_show ();
  [%expect
    {|
    ((collated (
       (data (
         (0   (0 0))
         (100 (1 1))
         (200 (2 2))
         (300 (3 3))
         (400 (4 4))
         (500 (5 5))
         (600 (6 6))
         (700 (7 7))
         (800 (8 8))
         (900 (9 9))))
       (num_filtered_rows   10)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 10)))
     (data_with_key_rank (
       (0 (0 (0)))
       (1 (1 (1)))
       (2 (2 (2)))
       (3 (3 (3)))
       (4 (4 (4)))
       (5 (5 (5)))
       (6 (6 (6)))
       (7 (7 (7)))
       (8 (8 (8)))
       (9 (9 (9))))))
    |}];
  Incr.Var.set
    collate_var
    { Incr_map_collate.Collate.filter = (fun ~key:_ ~data:_ -> true)
    ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
    ; key_range = All_rows
    ; rank_range = All_rows
    };
  stabilize_and_show ();
  [%expect
    {|
    ((collated (
       (data (
         (0   (9 9))
         (100 (8 8))
         (200 (7 7))
         (300 (6 6))
         (400 (5 5))
         (500 (4 4))
         (600 (3 3))
         (700 (2 2))
         (800 (1 1))
         (900 (0 0))))
       (num_filtered_rows   10)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 10)))
     (data_with_key_rank (
       (0 (0 (9)))
       (1 (1 (8)))
       (2 (2 (7)))
       (3 (3 (6)))
       (4 (4 (5)))
       (5 (5 (4)))
       (6 (6 (3)))
       (7 (7 (2)))
       (8 (8 (1)))
       (9 (9 (0))))))
    |}];
  Incr.Var.set
    collate_var
    { Incr_map_collate.Collate.filter = (fun ~key:_ ~data:_ -> true)
    ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
    ; key_range = Between (3, 6)
    ; rank_range = All_rows
    };
  stabilize_and_show ();
  [%expect
    {|
    ((collated (
       (data ())
       (num_filtered_rows 10)
       (key_range (Between 3 6))
       (rank_range          All_rows)
       (num_before_range    6)
       (num_unfiltered_rows 10)))
     (data_with_key_rank (
       (0 (0 (9)))
       (1 (1 (8)))
       (2 (2 (7)))
       (3 (3 (6)))
       (4 (4 (5)))
       (5 (5 (4)))
       (6 (6 (3)))
       (7 (7 (2)))
       (8 (8 (1)))
       (9 (9 (0))))))
    |}];
  Incr.Var.set
    collate_var
    { Incr_map_collate.Collate.filter = (fun ~key:_ ~data:_ -> true)
    ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
    ; key_range = All_rows
    ; rank_range = Between (3, 6)
    };
  stabilize_and_show ();
  [%expect
    {|
    ((collated (
       (data (
         (0   (6 6))
         (100 (5 5))
         (200 (4 4))
         (300 (3 3))))
       (num_filtered_rows 10)
       (key_range         All_rows)
       (rank_range (Between 3 6))
       (num_before_range    3)
       (num_unfiltered_rows 10)))
     (data_with_key_rank (
       (0 (0 (9)))
       (1 (1 (8)))
       (2 (2 (7)))
       (3 (3 (6)))
       (4 (4 (5)))
       (5 (5 (4)))
       (6 (6 (3)))
       (7 (7 (2)))
       (8 (8 (1)))
       (9 (9 (0))))))
    |}];
  Incr.Var.set
    collate_var
    { Incr_map_collate.Collate.filter = (fun ~key:_ ~data -> data % 2 = 0)
    ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
    ; key_range = All_rows
    ; rank_range = All_rows
    };
  stabilize_and_show ();
  [%expect
    {|
    ((collated (
       (data (
         (0   (8 8))
         (100 (6 6))
         (200 (4 4))
         (300 (2 2))
         (400 (0 0))))
       (num_filtered_rows   5)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 10)))
     (data_with_key_rank (
       (0 (0 (4)))
       (1 (1 ()))
       (2 (2 (3)))
       (3 (3 ()))
       (4 (4 (2)))
       (5 (5 ()))
       (6 (6 (1)))
       (7 (7 ()))
       (8 (8 (0)))
       (9 (9 ())))))
    |}]
;;
