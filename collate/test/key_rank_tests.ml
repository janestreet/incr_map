open! Core
open Expect_test_helpers_core
module Incr = Incremental.Make ()

module Key = struct
  module T = struct
    type t = int [@@deriving compare]

    let sexp_of_t key = [%message (key : int)]
  end

  include T
  include Comparable.Make_plain (T)
end

module Data = struct
  let sexp_of_t data = [%message (data : int)]
end

module Rank = struct
  let sexp_of_t rank = [%message (rank : int)]
end

let setup_test () =
  let data_var =
    Incr.Var.create (Key.Map.of_alist_exn (List.init 10 ~f:(fun i -> i, i)))
  in
  let collate_var =
    Incr.Var.create
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
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
        (collated : (Key.t, Data.t) Incr_map_collate.Collated.t)
          (data_with_key_rank : (Data.t * Rank.t option) Key.Map.t)]
  in
  collate_var, stabilize_and_show
;;

let%expect_test "all data" =
  let collate_var, stabilize_and_show = setup_test () in
  let ( (* no sorting *) ) =
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 0) (data 0)))
           (100 ((key 1) (data 1)))
           (200 ((key 2) (data 2)))
           (300 ((key 3) (data 3)))
           (400 ((key 4) (data 4)))
           (500 ((key 5) (data 5)))
           (600 ((key 6) (data 6)))
           (700 ((key 7) (data 7)))
           (800 ((key 8) (data 8)))
           (900 ((key 9) (data 9)))))
         (num_filtered_rows   10)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 0))))
         ((key 1) ((data 1) ((rank 1))))
         ((key 2) ((data 2) ((rank 2))))
         ((key 3) ((data 3) ((rank 3))))
         ((key 4) ((data 4) ((rank 4))))
         ((key 5) ((data 5) ((rank 5))))
         ((key 6) ((data 6) ((rank 6))))
         ((key 7) ((data 7) ((rank 7))))
         ((key 8) ((data 8) ((rank 8))))
         ((key 9) ((data 9) ((rank 9)))))))
      |}]
  in
  let ( (* reverse order *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
      ; key_range = All_rows
      ; rank_range = All_rows
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 9) (data 9)))
           (100 ((key 8) (data 8)))
           (200 ((key 7) (data 7)))
           (300 ((key 6) (data 6)))
           (400 ((key 5) (data 5)))
           (500 ((key 4) (data 4)))
           (600 ((key 3) (data 3)))
           (700 ((key 2) (data 2)))
           (800 ((key 1) (data 1)))
           (900 ((key 0) (data 0)))))
         (num_filtered_rows   10)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 9))))
         ((key 1) ((data 1) ((rank 8))))
         ((key 2) ((data 2) ((rank 7))))
         ((key 3) ((data 3) ((rank 6))))
         ((key 4) ((data 4) ((rank 5))))
         ((key 5) ((data 5) ((rank 4))))
         ((key 6) ((data 6) ((rank 3))))
         ((key 7) ((data 7) ((rank 2))))
         ((key 8) ((data 8) ((rank 1))))
         ((key 9) ((data 9) ((rank 0)))))))
      |}]
  in
  ()
;;

let%expect_test "key range" =
  let collate_var, stabilize_and_show = setup_test () in
  let ( (* (between key-3 key-6) *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Unchanged
      ; key_range = Between (3, 6)
      ; rank_range = All_rows
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 3) (data 3)))
           (100 ((key 4) (data 4)))
           (200 ((key 5) (data 5)))
           (300 ((key 6) (data 6)))))
         (num_filtered_rows 10)
         (key_range (
           Between
           (key 3)
           (key 6)))
         (rank_range          All_rows)
         (num_before_range    3)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 0))))
         ((key 1) ((data 1) ((rank 1))))
         ((key 2) ((data 2) ((rank 2))))
         ((key 3) ((data 3) ((rank 3))))
         ((key 4) ((data 4) ((rank 4))))
         ((key 5) ((data 5) ((rank 5))))
         ((key 6) ((data 6) ((rank 6))))
         ((key 7) ((data 7) ((rank 7))))
         ((key 8) ((data 8) ((rank 8))))
         ((key 9) ((data 9) ((rank 9)))))))
      |}]
  in
  let ( (* (between key-3 key-6), reverse order. (aka an empty range) *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
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
         (key_range (
           Between
           (key 3)
           (key 6)))
         (rank_range          All_rows)
         (num_before_range    6)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 9))))
         ((key 1) ((data 1) ((rank 8))))
         ((key 2) ((data 2) ((rank 7))))
         ((key 3) ((data 3) ((rank 6))))
         ((key 4) ((data 4) ((rank 5))))
         ((key 5) ((data 5) ((rank 4))))
         ((key 6) ((data 6) ((rank 3))))
         ((key 7) ((data 7) ((rank 2))))
         ((key 8) ((data 8) ((rank 1))))
         ((key 9) ((data 9) ((rank 0)))))))
      |}]
  in
  ()
;;

let%expect_test "rank range from start" =
  let collate_var, stabilize_and_show = setup_test () in
  let ( (* (between from_start 3 from_start 6) *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Unchanged
      ; key_range = All_rows
      ; rank_range = Between (From_start 3, From_start 6)
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 3) (data 3)))
           (100 ((key 4) (data 4)))
           (200 ((key 5) (data 5)))
           (300 ((key 6) (data 6)))))
         (num_filtered_rows 10)
         (key_range         All_rows)
         (rank_range (Between 3 6))
         (num_before_range    3)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 0))))
         ((key 1) ((data 1) ((rank 1))))
         ((key 2) ((data 2) ((rank 2))))
         ((key 3) ((data 3) ((rank 3))))
         ((key 4) ((data 4) ((rank 4))))
         ((key 5) ((data 5) ((rank 5))))
         ((key 6) ((data 6) ((rank 6))))
         ((key 7) ((data 7) ((rank 7))))
         ((key 8) ((data 8) ((rank 8))))
         ((key 9) ((data 9) ((rank 9)))))))
      |}]
  in
  let ( (* (between from_start 3 from_start 6), reverse order *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
      ; key_range = All_rows
      ; rank_range = Between (From_start 3, From_start 6)
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 6) (data 6)))
           (100 ((key 5) (data 5)))
           (200 ((key 4) (data 4)))
           (300 ((key 3) (data 3)))))
         (num_filtered_rows 10)
         (key_range         All_rows)
         (rank_range (Between 3 6))
         (num_before_range    3)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 9))))
         ((key 1) ((data 1) ((rank 8))))
         ((key 2) ((data 2) ((rank 7))))
         ((key 3) ((data 3) ((rank 6))))
         ((key 4) ((data 4) ((rank 5))))
         ((key 5) ((data 5) ((rank 4))))
         ((key 6) ((data 6) ((rank 3))))
         ((key 7) ((data 7) ((rank 2))))
         ((key 8) ((data 8) ((rank 1))))
         ((key 9) ((data 9) ((rank 0)))))))
      |}]
  in
  ()
;;

let%expect_test "rank range from end" =
  let collate_var, stabilize_and_show = setup_test () in
  let ( (* forward order *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Unchanged
      ; key_range = All_rows
      ; rank_range = Between (From_end 3, From_end 1)
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 6) (data 6)))
           (100 ((key 7) (data 7)))
           (200 ((key 8) (data 8)))))
         (num_filtered_rows 10)
         (key_range         All_rows)
         (rank_range (Between 6 8))
         (num_before_range    6)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 0))))
         ((key 1) ((data 1) ((rank 1))))
         ((key 2) ((data 2) ((rank 2))))
         ((key 3) ((data 3) ((rank 3))))
         ((key 4) ((data 4) ((rank 4))))
         ((key 5) ((data 5) ((rank 5))))
         ((key 6) ((data 6) ((rank 6))))
         ((key 7) ((data 7) ((rank 7))))
         ((key 8) ((data 8) ((rank 8))))
         ((key 9) ((data 9) ((rank 9)))))))
      |}]
  in
  let ( (* reverse order *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data:_ -> true)
      ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
      ; key_range = All_rows
      ; rank_range = Between (From_end 3, From_end 1)
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 3) (data 3)))
           (100 ((key 2) (data 2)))
           (200 ((key 1) (data 1)))))
         (num_filtered_rows 10)
         (key_range         All_rows)
         (rank_range (Between 6 8))
         (num_before_range    6)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 9))))
         ((key 1) ((data 1) ((rank 8))))
         ((key 2) ((data 2) ((rank 7))))
         ((key 3) ((data 3) ((rank 6))))
         ((key 4) ((data 4) ((rank 5))))
         ((key 5) ((data 5) ((rank 4))))
         ((key 6) ((data 6) ((rank 3))))
         ((key 7) ((data 7) ((rank 2))))
         ((key 8) ((data 8) ((rank 1))))
         ((key 9) ((data 9) ((rank 0)))))))
      |}]
  in
  ()
;;

let%expect_test "filters" =
  let collate_var, stabilize_and_show = setup_test () in
  let ( (* even data only *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data -> data % 2 = 0)
      ; order = Unchanged
      ; key_range = All_rows
      ; rank_range = All_rows
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 0) (data 0)))
           (100 ((key 2) (data 2)))
           (200 ((key 4) (data 4)))
           (300 ((key 6) (data 6)))
           (400 ((key 8) (data 8)))))
         (num_filtered_rows   5)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 0))))
         ((key 1) ((data 1) ()))
         ((key 2) ((data 2) ((rank 1))))
         ((key 3) ((data 3) ()))
         ((key 4) ((data 4) ((rank 2))))
         ((key 5) ((data 5) ()))
         ((key 6) ((data 6) ((rank 3))))
         ((key 7) ((data 7) ()))
         ((key 8) ((data 8) ((rank 4))))
         ((key 9) ((data 9) ())))))
      |}]
  in
  let ( (* reverse order, even data only *) ) =
    Incr.Var.set
      collate_var
      { Incr_map_collate.Collate_params.filter = (fun ~key:_ ~data -> data % 2 = 0)
      ; order = Custom_by_value { compare = Comparable.reverse Int.compare }
      ; key_range = All_rows
      ; rank_range = All_rows
      };
    stabilize_and_show ();
    [%expect
      {|
      ((collated (
         (data (
           (0   ((key 8) (data 8)))
           (100 ((key 6) (data 6)))
           (200 ((key 4) (data 4)))
           (300 ((key 2) (data 2)))
           (400 ((key 0) (data 0)))))
         (num_filtered_rows   5)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))
       (data_with_key_rank (
         ((key 0) ((data 0) ((rank 4))))
         ((key 1) ((data 1) ()))
         ((key 2) ((data 2) ((rank 3))))
         ((key 3) ((data 3) ()))
         ((key 4) ((data 4) ((rank 2))))
         ((key 5) ((data 5) ()))
         ((key 6) ((data 6) ((rank 1))))
         ((key 7) ((data 7) ()))
         ((key 8) ((data 8) ((rank 0))))
         ((key 9) ((data 9) ())))))
      |}]
  in
  ()
;;
