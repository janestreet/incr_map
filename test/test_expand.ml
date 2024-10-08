open! Core
open! Import

module%test _ = struct
  module Key = struct
    module T = struct
      type t = (int, int) Tuple2.t [@@deriving sexp_of]

      type comparator_witness =
        (Int.comparator_witness, Int.comparator_witness) Tuple2.comparator_witness

      let comparator = Tuple2.comparator Int.comparator Int.comparator
    end

    include T
    include Comparable.Make_plain_using_comparator (T)
  end

  let%expect_test "manual updates" =
    let var = [ (0, 1), "a"; (1, 2), "b" ] |> Key.Map.of_alist_exn |> Incr.Var.create in
    let observer =
      Incr.observe
        (Incr_map.expand
           ~outer_comparator:(module Int)
           ~inner_comparator:(module Int)
           (Incr.Var.watch var))
    in
    let update_and_test ~f =
      Incr.Var.replace var ~f;
      Incr.stabilize ();
      print_s [%sexp (Incr.Observer.value_exn observer : string Int.Map.t Int.Map.t)]
    in
    update_and_test ~f:Fn.id;
    [%expect
      {|
      ((0 ((1 a)))
       (1 ((2 b))))
      |}];
    update_and_test ~f:(fun m -> Map.add_exn m ~key:(2, 4) ~data:"c");
    [%expect
      {|
      ((0 ((1 a)))
       (1 ((2 b)))
       (2 ((4 c))))
      |}];
    update_and_test ~f:(fun m -> Map.remove m (1, 2));
    [%expect
      {|
      ((0 ((1 a)))
       (2 ((4 c))))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:(2, 0) ~data:"c");
    [%expect
      {|
      ((0 ((1 a)))
       (2 (
         (0 c)
         (4 c))))
      |}];
    update_and_test ~f:(fun m -> Map.set m ~key:(2, 1) ~data:"asdf");
    [%expect
      {|
      ((0 ((1 a)))
       (2 (
         (0 c)
         (1 asdf)
         (4 c))))
      |}]
  ;;

  let all_at_once t =
    Map.fold t ~init:Int.Map.empty ~f:(fun ~key:(outer_key, inner_key) ~data acc ->
      Map.update acc outer_key ~f:(function
        | None -> Int.Map.singleton inner_key data
        | Some map -> Map.add_exn map ~key:inner_key ~data))
  ;;

  let%test_unit "randomized map changes" =
    let var = Incr.Var.create Key.Map.empty in
    let observer =
      Incremental.observe
        (Incr_map.expand
           (Incr.Var.watch var)
           ~outer_comparator:(module Int)
           ~inner_comparator:(module Int))
    in
    Quickcheck.test
      (Map_operations.tuple_key_quickcheck_generator String.quickcheck_generator)
      ~f:(fun operations ->
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          [%test_result: string Int.Map.t Int.Map.t]
            ~expect:(all_at_once (Incr.Var.latest_value var))
            (Incremental.Observer.value_exn observer)))
  ;;

  let%test_unit "expand collapse compose" =
    let var = Incr.Var.create Key.Map.empty in
    let observer =
      Incremental.observe
        (Incr_map.collapse
           ~comparator:(module Int)
           (Incr_map.expand
              (Incr.Var.watch var)
              ~outer_comparator:(module Int)
              ~inner_comparator:(module Int)))
    in
    Quickcheck.test
      (Map_operations.tuple_key_quickcheck_generator String.quickcheck_generator)
      ~f:(fun operations ->
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          [%test_result: string Key.Map.t]
            ~expect:(Incr.Var.latest_value var)
            (Incremental.Observer.value_exn observer)))
  ;;
end
