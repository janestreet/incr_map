open! Core
open! Import

let%test_module _ =
  (module struct
    type t = string Int.Map.t String.Map.t

    module Collapsed = struct
      module T = struct
        type t = (string, int) Tuple2.t [@@deriving sexp_of]

        type comparator_witness =
          (String.comparator_witness, Int.comparator_witness) Tuple2.comparator_witness

        let comparator = Tuple2.comparator String.comparator Int.comparator
      end

      include T
      include Comparable.Make_plain_using_comparator (T)
    end

    let%expect_test "manual updates" =
      let var =
        [ "a", [ 1, "a" ]; "b", [ 2, "b" ] ]
        |> List.map ~f:(fun (outer_key, inner_alist) ->
          outer_key, Int.Map.of_alist_exn inner_alist)
        |> String.Map.of_alist_exn
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
        (((a 1) a)
         ((b 2) b)) |}];
      update_and_test ~f:(fun m -> Map.add_exn m ~key:"c" ~data:(Int.Map.singleton 4 "c"));
      [%expect {|
        (((a 1) a)
         ((b 2) b)
         ((c 4) c)) |}];
      update_and_test ~f:(fun m -> Map.remove m "b");
      [%expect {|
        (((a 1) a)
         ((c 4) c)) |}];
      update_and_test ~f:(fun m -> Map.set m ~key:"c" ~data:(Int.Map.singleton 0 "c"));
      [%expect {|
        (((a 1) a)
         ((c 0) c)) |}];
      update_and_test ~f:(fun m ->
        Map.update m "c" ~f:(function
          | None -> assert false
          | Some m -> Map.add_exn m ~key:1 ~data:"asdf"));
      [%expect {|
        (((a 1) a)
         ((c 0) c)
         ((c 1) asdf)) |}]
    ;;

    let quickcheck_generator : t Quickcheck.Generator.t =
      Map.quickcheck_generator
        (module String)
        [%quickcheck.generator: string]
        (Map.quickcheck_generator
           (module Int)
           [%quickcheck.generator: int]
           [%quickcheck.generator: string])
    ;;

    let all_at_once (t : t) =
      Map.fold
        t
        ~init:
          (Map.Using_comparator.empty
             ~comparator:(Tuple2.comparator String.comparator Int.comparator))
        ~f:(fun ~key:outer_key ~data acc ->
          Map.fold data ~init:acc ~f:(fun ~key:inner_key ~data acc ->
            Map.add_exn acc ~key:(outer_key, inner_key) ~data))
    ;;

    let%test_unit "randomized map changes" =
      let var = Incr.Var.create String.Map.empty in
      let observer =
        Incremental.observe
          (Incr_map.collapse (Incr.Var.watch var) ~comparator:(module Int))
      in
      Quickcheck.test quickcheck_generator ~f:(fun map ->
        Incr.Var.set var map;
        Incr.stabilize ();
        [%test_result: string Collapsed.Map.t]
          ~expect:(all_at_once map)
          (Incremental.Observer.value_exn observer))
    ;;
  end)
;;
