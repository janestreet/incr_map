open! Core
open Incr_map_collate

(***)
module Incr = Incremental.Make ()
module Key = String
module Range = Incr_map_collate.With_caching.Range_memoize_bucket
module Fold = Incr_map_collate.Fold

module Value = struct
  type t = int [@@deriving sexp, bin_io, equal, compare]
end

module Concrete = Collated.Make_concrete (Key) (Value)

module Order = struct
  module T = struct
    type t =
      | Ascending
      | Descending
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_compare = function
    | Ascending -> Compare.Unchanged
    | Descending -> Reversed
  ;;
end

module Filter = struct
  module T = struct
    type t =
      | None
      | True
      | Key_has_vowel
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_predicate = function
    | None -> (None : _ option)
    | True -> Some (fun ~key:_ ~data:_ -> true)
    | Key_has_vowel ->
      Some
        (fun ~key ~data:_ ->
          String.exists (String.lowercase key) ~f:(fun ch ->
            List.exists [ 'a'; 'e'; 'i'; 'o'; 'u' ] ~f:(Char.equal ch)))
  ;;
end

module Order_filter = struct
  type t = Order.t * Filter.t [@@deriving equal, compare, sexp_of]
end

module Order_filter_range = struct
  type t = Order.t * Filter.t * Range.t [@@deriving equal, compare, sexp_of]
end

type 'a t =
  { map : Value.t Key.Map.t Incr.Var.t
  ; collate : (Key.t, Filter.t, Order.t) Collate_params.t Incr.Var.t
  ; observer : (Concrete.t * 'a) Incr.Observer.t
  }

let do_collate_and_fold
  ~order_cache_size
  ~order_filter_cache_size
  ~order_filter_range_cache_size
  ~data_equal
  ~fold_result_equal
  ~fold
  ()
  input
  collate
  =
  let with_caching =
    let order_cache_params =
      Incr_memoize.Store_params.alist_based__lru
        ~equal:Order.equal
        ~max_size:order_cache_size
    in
    let order_filter_cache_params =
      Incr_memoize.Store_params.alist_based__lru
        ~equal:Order_filter.equal
        ~max_size:order_filter_cache_size
    in
    let order_filter_range_cache_params =
      Incr_memoize.Store_params.alist_based__lru
        ~equal:Order_filter_range.equal
        ~max_size:order_filter_range_cache_size
    in
    Incr_map_collate.With_caching.collate_and_fold__sort_first
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      ~range_memoize_bucket_size:1
      ~order_cache_params
      ~order_filter_cache_params
      ~order_filter_range_cache_params
      ~fold
      input
      collate
  in
  let without_caching =
    Incr_map_collate.collate_and_fold
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      ~fold
      input
      collate
  in
  let%map.Incr with_caching_data = Incr_map_collate.collated with_caching
  and with_caching_fold_result = Incr_map_collate.fold_result with_caching
  and without_caching_data = Incr_map_collate.collated without_caching
  and without_caching_fold_result = Incr_map_collate.fold_result without_caching in
  assert (Collated.equal String.equal data_equal with_caching_data without_caching_data);
  assert (fold_result_equal with_caching_fold_result without_caching_fold_result);
  with_caching_data, with_caching_fold_result
;;

let get_res t =
  Incr.stabilize ();
  Incr.Observer.value_exn t.observer
;;

let modify_map t ~f = Incr.Var.set t.map (f (Incr.Var.value t.map))

let print_res t =
  let items, fold_result = get_res t in
  print_s
    [%message
      ""
        ~_:(Collated.to_alist items : (Key.t * Value.t) list)
        ~fold_result:(fold_result : int)
        ~num_filtered_rows:(Collated.num_filtered_rows items : int)
        ~num_unfiltered_rows:(Collated.num_unfiltered_rows items : int)]
;;

let set_collate ?filter ?rank_range ?key_range ?order t =
  let collate = Incr.Var.value t.collate in
  let collate : _ Collate_params.t =
    { filter = Option.value filter ~default:collate.filter
    ; key_range = Option.value key_range ~default:collate.key_range
    ; rank_range = Option.value rank_range ~default:collate.rank_range
    ; order = Option.value order ~default:collate.order
    }
  in
  Incr.Var.set t.collate collate
;;

let init_test
  ?(data = [ "A", 1; "B", 2; "C", 3 ])
  ?(filter = Filter.None)
  ?(order = Order.Ascending)
  ?(key_range = Collate_params.Which_range.All_rows)
  ?(rank_range = Collate_params.Which_range.All_rows)
  ~fold
  ()
  =
  let initial = Key.Map.of_alist_exn data in
  let map = Incr.Var.create initial in
  let collate =
    Incr.Var.create ({ filter; order; key_range; rank_range } : _ Collate_params.t)
  in
  let observer =
    let collated =
      do_collate_and_fold
        ~order_cache_size:1
        ~order_filter_cache_size:2
        ~order_filter_range_cache_size:3
        ~data_equal:[%equal: int]
        ~fold_result_equal:[%equal: int]
        ~fold
        ()
        (Incr.Var.watch map)
        (Incr.Var.watch collate)
    in
    Incr.observe collated
  in
  { map; collate; observer }
;;

let%expect_test "count items via fold" =
  let t =
    let fold =
      Fold.create
        ~init:0
        ~add:(fun ~key:_ ~data:_ acc -> acc + 1)
        ~remove:(fun ~key:_ ~data:_ acc -> acc - 1)
        ()
    in
    init_test ~fold ()
  in
  print_res t;
  [%expect
    {|
    (((A 1) (B 2) (C 3)) (fold_result 3) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}];
  set_collate ~filter:True t;
  print_res t;
  [%expect
    {|
    (((A 1) (B 2) (C 3)) (fold_result 3) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}];
  set_collate ~filter:Key_has_vowel t;
  print_res t;
  [%expect {| (((A 1)) (fold_result 1) (num_filtered_rows 1) (num_unfiltered_rows 3)) |}];
  set_collate ~order:Descending t;
  print_res t;
  [%expect {| (((A 1)) (fold_result 1) (num_filtered_rows 1) (num_unfiltered_rows 3)) |}];
  modify_map t ~f:(fun map -> Map.remove map "A");
  print_res t;
  [%expect {| (() (fold_result 0) (num_filtered_rows 0) (num_unfiltered_rows 2)) |}]
;;

let%expect_test "sum data in filter" =
  let t =
    let fold =
      Fold.create
        ~init:0
        ~add:(fun ~key:_ ~data acc -> acc + data)
        ~remove:(fun ~key:_ ~data acc -> acc - data)
        ()
    in
    init_test ~fold ()
  in
  print_res t;
  [%expect
    {|
    (((A 1) (B 2) (C 3)) (fold_result 6) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}];
  set_collate ~filter:Key_has_vowel t;
  print_res t;
  [%expect {| (((A 1)) (fold_result 1) (num_filtered_rows 1) (num_unfiltered_rows 3)) |}];
  modify_map t ~f:(Map.set ~key:"E" ~data:6);
  print_res t;
  [%expect
    {| (((A 1) (E 6)) (fold_result 7) (num_filtered_rows 2) (num_unfiltered_rows 4)) |}];
  modify_map t ~f:(Map.set ~key:"H" ~data:12);
  print_res t;
  [%expect
    {| (((A 1) (E 6)) (fold_result 7) (num_filtered_rows 2) (num_unfiltered_rows 5)) |}];
  set_collate ~filter:None t;
  print_res t;
  [%expect
    {|
    (((A 1) (B 2) (C 3) (E 6) (H 12)) (fold_result 24) (num_filtered_rows 5)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "fold with update" =
  let t =
    let fold =
      Fold.create
        ~init:0
        ~add:(fun ~key ~data acc -> acc + String.length key + data)
        ~remove:(fun ~key ~data acc -> acc - String.length key - data)
        ~update:(fun ~key ~old_data ~new_data acc ->
          print_s [%message "Updating" (key : string) (old_data : int) (new_data : int)];
          acc + new_data - old_data)
        ()
    in
    init_test ~fold ()
  in
  print_res t;
  [%expect
    {|
    (((A 1) (B 2) (C 3)) (fold_result 9) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}];
  (* Setting a value calls update *)
  modify_map t ~f:(Map.set ~key:"A" ~data:7);
  print_res t;
  [%expect
    {|
    (Updating (key A) (old_data 1) (new_data 7))
    (Updating (key A) (old_data 1) (new_data 7))
    (((A 7) (B 2) (C 3)) (fold_result 15) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}];
  (* Adding or removing a value does not *)
  modify_map t ~f:(fun map -> Map.remove map "A");
  modify_map t ~f:(fun map -> Map.add_exn map ~key:"D" ~data:0);
  print_res t;
  [%expect
    {|
    (((B 2) (C 3) (D 0)) (fold_result 8) (num_filtered_rows 3)
     (num_unfiltered_rows 3))
    |}]
;;
