(** Tests to verify the behavior of the old Legacy_diffable-based API *)
open! Core

open Expect_test_helpers_core
module Incr = Incremental.Make ()
open Incr_map_collate
module Key = String (* "symbol" *)
module Collate_params = Collate_params
module Compare = Compare
module Collated = Collated

module Value = struct
  type t = int * float (* "size" and "price" *) [@@deriving sexp, bin_io, equal, compare]
end

module Concrete = Collated.Stable.V1.Make_concrete (Key) (Value)

module Order = struct
  module T = struct
    type t = unit [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_compare = function
    | () -> Compare.Unchanged
  ;;
end

module Filter = struct
  module T = struct
    type t = unit [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_predicate = function
    | () -> (None : _ option)
  ;;
end

type t =
  { map : Value.t Key.Map.t Incr.Var.t
  ; collate : (Key.t, Filter.t, Order.t) Collate_params.t Incr.Var.t
  ; observer : Concrete.t Incr.Observer.t
  }

let get_res t =
  Incr.stabilize ();
  Incr.Observer.value_exn t.observer
;;

let modify_map t ~f = Incr.Var.set t.map (f (Incr.Var.value t.map))

let do_collate_default ?operation_order input collate =
  Incr_map_collate.collate
    ?operation_order
    ~filter_to_predicate:Filter.to_predicate
    ~order_to_compare:Order.to_compare
    ~filter_equal:Filter.equal
    ~order_equal:Order.equal
    input
    collate
;;

let init_test
  ?(data = [ "AAPL", (10, 1.0); "GOOG", (10, 3.0); "VOD", (10, 2.0) ])
  ?operation_order
  ?(filter = ())
  ?(order = ())
  ?(key_range = Collate_params.Which_range.All_rows)
  ?(rank_range = Collate_params.Which_range.All_rows)
  ?(do_collate = do_collate_default)
  ()
  =
  let initial = Key.Map.of_alist_exn data in
  let map = Incr.Var.create initial in
  let collate =
    Incr.Var.create
      ({ filter; order; key_range; rank_range; widen_range_by = 0, 0 }
       : _ Collate_params.t)
  in
  let observer =
    let collated =
      do_collate ?operation_order (Incr.Var.watch map) (Incr.Var.watch collate)
      |> Incr_map_collate.collated
      |> Incremental.map ~f:Incr_map_collate.Collated.to_stable_v1
    in
    Incr.observe collated
  in
  (* Make sure that our math works out *)
  Incr.Observer.on_update_exn observer ~f:(function
    | Incr.Observer.Update.Invalidated -> ()
    | Incr.Observer.Update.Initialized a | Incr.Observer.Update.Changed (_, a) ->
      let a = Incr_map_collate.Collated.of_stable_v1 a in
      let total = Incr_map_collate.Collated.num_filtered_rows a in
      let before = Incr_map_collate.Collated.num_before_range a in
      let inside = Incr_map_collate.Collated.length a in
      let after = Incr_map_collate.Collated.num_after_range a in
      assert (before + after + inside = total));
  { map; collate; observer }
;;

let%expect_test "diffs" =
  let t = init_test ~data:[ "A", (0, 0.); "B", (0, 100.) ] () in
  let res1 = get_res t in
  modify_map t ~f:(Map.add_exn ~key:"AA" ~data:(0, 50.));
  modify_map t ~f:(Map.set ~key:"A" ~data:(0, 1.));
  modify_map t ~f:(Map.add_exn ~key:"BB" ~data:(0, 200.));
  let res2 = get_res t in
  let update = Concrete.diffs ~from:res1 ~to_:res2 in
  print_s [%sexp (update : Concrete.Update.t)];
  [%expect
    {|
    ((Num_unfiltered_rows 4)
     (Num_filtered_rows   4)
     (Data (Add 200 (BB (0 200))))
     (Data (Add 50 (AA (0 50))))
     (Data (Add 0 (A (0 1)))))
    |}];
  let patched = Concrete.update res1 update in
  require_equal (module Concrete) res2 patched;
  print_s [%message "" ~orig:(res2 : Concrete.t) ~patched:(patched : Concrete.t)];
  [%expect
    {|
    ((orig (
       (data (
         (0   (A  (0 1)))
         (50  (AA (0 50)))
         (100 (B  (0 100)))
         (200 (BB (0 200)))))
       (num_filtered_rows   4)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 4)))
     (patched (
       (data (
         (0   (A  (0 1)))
         (50  (AA (0 50)))
         (100 (B  (0 100)))
         (200 (BB (0 200)))))
       (num_filtered_rows   4)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 4))))
    |}]
;;

let%expect_test "duplicates in diff" =
  let t1 =
    Collated.For_testing.of_list
      ~num_filtered_rows:1
      ~num_unfiltered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      ~range_widened_by:(0, 0)
      [ "1", (1, 1.0) ]
    |> Incr_map_collate.Collated.to_stable_v1
  in
  let t2 =
    Collated.For_testing.of_list
      ~num_filtered_rows:2
      ~num_unfiltered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      ~range_widened_by:(0, 0)
      [ "2", (2, 2.0) ]
    |> Incr_map_collate.Collated.to_stable_v1
  in
  let d1 = Concrete.to_diffs t1 in
  let d2 = Concrete.to_diffs t2 in
  print_s [%message "diffs" (d1 : Concrete.Update.t) (d2 : Concrete.Update.t)];
  let t1' = Concrete.of_diffs d1 in
  let t1'' = Concrete.update t1 d1 in
  let t1''' = Concrete.update t1 (d1 @ d1) in
  let t1'''' = Concrete.update t1 (d2 @ d1) in
  print_s [%message "t1s" ([ t1'; t1''; t1'''; t1'''' ] : Concrete.t list)];
  [%expect
    {|
    (diffs
      (d1 (
        (Num_unfiltered_rows     10)
        (Elements_prior_to_range 0)
        (Rank_range              All_rows)
        (Key_range               All_rows)
        (Num_filtered_rows       1)
        (Data (Add 0 (1 (1 1))))))
      (d2 (
        (Num_unfiltered_rows     10)
        (Elements_prior_to_range 0)
        (Rank_range              All_rows)
        (Key_range               All_rows)
        (Num_filtered_rows       2)
        (Data (Add 0 (2 (2 2)))))))
    (t1s (
      "[t1'; t1''; t1'''; t1'''']" (
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))))
    |}];
  let t2' = Concrete.update t1 (d1 @ d2) in
  print_s [%message "t2s" ([ t2; t2' ] : Concrete.t list)];
  [%expect
    {|
    (t2s (
      "[t2; t2']" (
        ((data ((0 (2 (2 2)))))
         (num_filtered_rows   2)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (2 (2 2)))))
         (num_filtered_rows   2)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))))
    |}]
;;
