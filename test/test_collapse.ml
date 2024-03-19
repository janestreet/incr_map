open! Core
open! Import

let%test_module _ =
  (module struct
    module Collapsed = struct
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
      let var =
        [ 0, [ 1, "a" ]; 1, [ 2, "b" ] ]
        |> List.map ~f:(fun (outer_key, inner_alist) ->
             outer_key, Int.Map.of_alist_exn inner_alist)
        |> Int.Map.of_alist_exn
        |> Incr.Var.create
      in
      let observer =
        Incr.observe (Incr_map.collapse ~comparator:(module Int) (Incr.Var.watch var))
      in
      let update_and_test ~f =
        Incr.Var.replace var ~f;
        Incr.stabilize ();
        print_s [%sexp (Incr.Observer.value_exn observer : string Collapsed.Map.t)]
      in
      update_and_test ~f:Fn.id;
      [%expect {|
        (((0 1) a)
         ((1 2) b))
        |}];
      update_and_test ~f:(fun m -> Map.add_exn m ~key:2 ~data:(Int.Map.singleton 4 "c"));
      [%expect {|
        (((0 1) a)
         ((1 2) b)
         ((2 4) c))
        |}];
      update_and_test ~f:(fun m -> Map.remove m 1);
      [%expect {|
        (((0 1) a)
         ((2 4) c))
        |}];
      update_and_test ~f:(fun m -> Map.set m ~key:2 ~data:(Int.Map.singleton 0 "c"));
      [%expect {|
        (((0 1) a)
         ((2 0) c))
        |}];
      update_and_test ~f:(fun m ->
        Map.update m 2 ~f:(function
          | None -> assert false
          | Some m -> Map.add_exn m ~key:1 ~data:"asdf"));
      [%expect {|
        (((0 1) a)
         ((2 0) c)
         ((2 1) asdf))
        |}]
    ;;

    let all_at_once t =
      Map.fold
        t
        ~init:
          (Map.Using_comparator.empty
             ~comparator:(Tuple2.comparator Int.comparator Int.comparator))
        ~f:(fun ~key:outer_key ~data acc ->
          Map.fold data ~init:acc ~f:(fun ~key:inner_key ~data acc ->
            Map.add_exn acc ~key:(outer_key, inner_key) ~data))
    ;;

    let%test_unit "randomized map changes" =
      let var = Incr.Var.create Int.Map.empty in
      let observer =
        Incremental.observe
          (Incr_map.collapse (Incr.Var.watch var) ~comparator:(module Int))
      in
      Quickcheck.test
        (Map_operations.nested_quickcheck_generator String.quickcheck_generator)
        ~f:(fun operations ->
        Map_operations.nested_run_operations
          operations
          ~inner_map_comparator:(module Int)
          ~into:var
          ~after_stabilize:(fun () ->
            [%test_result: string Collapsed.Map.t]
              ~expect:(all_at_once (Incr.Var.latest_value var))
              (Incremental.Observer.value_exn observer)))
    ;;

    let%test_unit "collapse expand compose" =
      let var = Incr.Var.create Int.Map.empty in
      let observer =
        Incremental.observe
          (Incr_map.expand
             (Incr_map.collapse ~comparator:(module Int) (Incr.Var.watch var))
             ~outer_comparator:(module Int)
             ~inner_comparator:(module Int))
      in
      Quickcheck.test
        (Map_operations.nested_quickcheck_generator String.quickcheck_generator)
        ~f:(fun operations ->
        Map_operations.nested_run_operations
          operations
          ~inner_map_comparator:(module Int)
          ~into:var
          ~after_stabilize:(fun () ->
            [%test_result: string Int.Map.t Int.Map.t]
            (* NB: outer keys that map to an empty inner map will be dropped by this
                   operation. *)
              ~expect:(Map.filter (Incr.Var.latest_value var) ~f:(Fn.non Map.is_empty))
              (Incremental.Observer.value_exn observer)))
    ;;
  end)
;;
