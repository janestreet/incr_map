open Core
module Incr = Incremental.Make ()
module Collate = Incr_map_collate.Collate
module Collated = Incr_map_collate.Collated
module Incr_map_collate = Incr_map_collate.Make (Incr)
module Key = String
module Incr_map_helpers = Incr_map_test.Subrange_quickcheck_helper

module Filter = struct
  module T = struct
    type t =
      | All
      | Almost_all
    [@@deriving sexp, equal, compare]
  end

  include T
  include Comparator.Make (T)

  let to_predicate t =
    match t with
    | All -> None
    | Almost_all -> Some (fun ~key ~data:_ -> key land 0xfff <> 0)
  ;;
end

module Order = struct
  module T = struct
    type t =
      | Unchanged
      | By_value
    [@@deriving sexp, equal, compare]
  end

  include T
  include Comparator.Make (T)

  let to_compare = function
    | Unchanged -> Incr_map_collate.Compare.Unchanged
    | By_value -> Custom_by_value { compare = Int.compare }
  ;;
end

let seed = `Deterministic "31415925..."

let make_map =
  (* Generate a random map of given size. Caches per-size, as map generation is quite
     slow - 1M entries takes ~3s. *)
  let f =
    Memo.general ~hashable:Int.hashable (fun map_len ->
      Incr_map_helpers.map_with_length_gen map_len |> Quickcheck.random_value ~seed)
  in
  fun ~map_len -> f map_len
;;

let fresh_test ~map_len ~collate ~operation_order =
  let map = make_map ~map_len in
  let map_var = Incr.Var.create map in
  let collate_var = Incr.Var.create collate in
  let count_var = Incr.Var.create 0 in
  let res =
    let%bind.Incr _count = Incr.Var.watch count_var in
    Incr_map_collate.collate
      ~operation_order
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      (Incr.Var.watch map_var)
      (Incr.Var.watch collate_var)
  in
  let ob = Incr.observe res in
  stage (fun () ->
    Incr.Var.set count_var (Incr.Var.value count_var + 1);
    Incr.stabilize ();
    ignore (Incremental.Observer.value_exn ob : _ Collated.t))
;;

let change_filter_test ~map_len ~(collate : _ Collate.t) ~operation_order =
  let map = make_map ~map_len in
  let map_var = Incr.Var.create map in
  let collate_var = Incr.Var.create collate in
  let orig_filter = collate.filter in
  let res =
    Incr_map_collate.collate
      ~operation_order
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      (Incr.Var.watch map_var)
      (Incr.Var.watch collate_var)
  in
  let ob = Incr.observe res in
  stage (fun () ->
    Incr.Var.set collate_var { (Incr.Var.value collate_var) with filter = orig_filter };
    Incr.stabilize ();
    ignore (Incremental.Observer.value_exn ob : _ Collated.t))
;;

let incr_test ~map_len ~diff_len ~collate =
  let map1 = make_map ~map_len in
  let ops =
    Quickcheck.Generator.list_with_length
      diff_len
      (Incr_map_helpers.map_op_gen ~none_ratio:0. ())
    |> Quickcheck.random_value ~seed
  in
  let map2 = List.fold ~init:map1 ops ~f:Incr_map_helpers.apply_map_op in
  let map_var = Incr.Var.create map1 in
  let collate_var = Incr.Var.create collate in
  let ob =
    Incr.observe
      (Incr_map_collate.collate
         ~order_to_compare:Order.to_compare
         ~filter_to_predicate:Filter.to_predicate
         ~filter_equal:Filter.equal
         ~order_equal:Order.equal
         (Incr.Var.watch map_var)
         (Incr.Var.watch collate_var))
  in
  Incr.stabilize ();
  let is_currently_map2 = ref false in
  stage (fun () ->
    Incr.Var.set map_var (if !is_currently_map2 then map1 else map2);
    is_currently_map2 := not !is_currently_map2;
    Incr.stabilize ();
    ignore (Incremental.Observer.value_exn ob : _ Collated.t))
;;

let filter_almost_all ~key ~data:_ = key land 0xfff <> 0

module Sorted = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make_plain (T)

  let[@inline always] compare (k1, v1) (k2, v2) =
    let res = compare_int v1 v2 in
    if not (Int.equal res 0) then res else compare_int k1 k2
  ;;

  let[@inline always] compare_ignore_second (a1, _) (a2, _) = compare a1 a2
end

let fresh_benchmarks ~verbose =
  let run = Bench.run ~verbose in
  let map_len = 500_000 in
  run
    ~name:(sprintf "fresh (%d) - no filter, no sort, no range" map_len)
    ~init_f:(fun () ->
      fresh_test
        ~operation_order:`Filter_first
        ~map_len
        ~collate:(Collate.default ~filter:Filter.All ~order:Order.Unchanged));
  run
    ~name:(sprintf "fresh (%d) - just Map.filter, for comparison" map_len)
    ~init_f:(fun () ->
      let map = make_map ~map_len in
      stage (fun () ->
        ignore (Map.filter map ~f:(fun key -> filter_almost_all ~key ~data:0) : _ Map.t)));
  run ~name:(sprintf "fresh (%d) - just array.sort" map_len) ~init_f:(fun () ->
    let arr =
      Array.init map_len ~f:(fun key ->
        let value = Random.int 1_000_000_000 in
        (key, value), value)
    in
    stage (fun () ->
      let arr2 = Array.copy arr in
      Array.sort arr2 ~compare:Sorted.compare_ignore_second));
  run
    ~name:(sprintf "fresh (%d) - map sorting via array, for comparison" map_len)
    ~init_f:(fun () ->
      let map = make_map ~map_len in
      stage (fun () ->
        ignore
          (let key, data = Map.min_elt_exn map in
           let a = Array.create ~len:(Map.length map) ((key, data), data) in
           let idx = ref 0 in
           Map.iteri map ~f:(fun ~key ~data ->
             a.(!idx) <- (key, data), data;
             incr idx);
           Array.sort a ~compare:Sorted.compare_ignore_second;
           Sorted.Map.of_sorted_array_unchecked a
           : _ Map.t)));
  run
    ~name:(sprintf "fresh (%d) - just Map.fold (sort), for comparison" map_len)
    ~init_f:(fun () ->
      let map = make_map ~map_len in
      stage (fun () ->
        ignore
          (Map.fold map ~init:Sorted.Map.empty ~f:(fun ~key ~data acc ->
             Map.add_exn acc ~key:(data, key) ~data)
           : _ Map.t)));
  run ~name:(sprintf "fresh (%d) - filter, no sort, no range" map_len) ~init_f:(fun () ->
    fresh_test
      ~map_len
      ~operation_order:`Filter_first
      ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.Unchanged));
  run ~name:(sprintf "fresh (%d) - filter, sort, no range" map_len) ~init_f:(fun () ->
    fresh_test
      ~map_len
      ~operation_order:`Filter_first
      ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.By_value));
  run ~name:(sprintf "fresh (%d) - filter, sort, range" map_len) ~init_f:(fun () ->
    fresh_test
      ~map_len
      ~operation_order:`Filter_first
      ~collate:
        { (Collate.default ~filter:Filter.Almost_all ~order:Order.By_value) with
          rank_range = Between (100, 200)
        });
  run ~name:(sprintf "fresh (%d) - sort, filter, no range" map_len) ~init_f:(fun () ->
    fresh_test
      ~map_len
      ~operation_order:`Sort_first
      ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.By_value));
  run ~name:(sprintf "fresh (%d) - sort, filter, range" map_len) ~init_f:(fun () ->
    fresh_test
      ~map_len
      ~operation_order:`Sort_first
      ~collate:
        { (Collate.default ~filter:Filter.Almost_all ~order:Order.By_value) with
          rank_range = Between (100, 200)
        });
  run
    ~name:(sprintf "change filter (%d) - filter, sort, no range" map_len)
    ~init_f:(fun () ->
      change_filter_test
        ~map_len
        ~operation_order:`Filter_first
        ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.By_value));
  run
    ~name:(sprintf "change filter (%d) - filter, sort, range" map_len)
    ~init_f:(fun () ->
      change_filter_test
        ~map_len
        ~operation_order:`Filter_first
        ~collate:
          { (Collate.default ~filter:Filter.Almost_all ~order:Order.By_value) with
            rank_range = Between (100, 200)
          });
  run
    ~name:(sprintf "change filter (%d) - sort, filter, no range" map_len)
    ~init_f:(fun () ->
      change_filter_test
        ~map_len
        ~operation_order:`Sort_first
        ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.By_value));
  run
    ~name:(sprintf "change filter (%d) - sort, filter, range" map_len)
    ~init_f:(fun () ->
      change_filter_test
        ~map_len
        ~operation_order:`Sort_first
        ~collate:
          { (Collate.default ~filter:Filter.Almost_all ~order:Order.By_value) with
            rank_range = Between (100, 200)
          })
;;

let incr_benchmarks ~verbose =
  let run = Bench.run ~verbose in
  let map_len = 1_000_000 in
  let diff_len = 100 in
  run
    ~name:(sprintf "incr (%d, %d) - no filter, no sort, no range" map_len diff_len)
    ~init_f:(fun () ->
      incr_test
        ~map_len
        ~diff_len
        ~collate:(Collate.default ~filter:Filter.All ~order:Order.Unchanged));
  run
    ~name:(sprintf "incr (%d, %d) - filter, no sort, no range" map_len diff_len)
    ~init_f:(fun () ->
      incr_test
        ~map_len
        ~diff_len
        ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.Unchanged));
  run
    ~name:(sprintf "incr (%d, %d) - no filter, sort, no range" map_len diff_len)
    ~init_f:(fun () ->
      incr_test
        ~map_len
        ~diff_len
        ~collate:(Collate.default ~filter:Filter.All ~order:Order.By_value));
  run
    ~name:(sprintf "incr (%d, %d) - filter, sort, no range" map_len diff_len)
    ~init_f:(fun () ->
      incr_test
        ~map_len
        ~diff_len
        ~collate:(Collate.default ~filter:Filter.Almost_all ~order:Order.By_value))
;;

let all_benchmarks ~verbose =
  fresh_benchmarks ~verbose;
  incr_benchmarks ~verbose;
  ()
;;

let () =
  Command.basic
    ~summary:"Run incr_map_collate benchmarks"
    (let%map_open.Command verbose = flag "-verbose" no_arg ~doc:"More verbose output" in
     fun () -> all_benchmarks ~verbose)
  |> Command.run
;;
