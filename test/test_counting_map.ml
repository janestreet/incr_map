open! Core
open! Import

type ('a, 'here, 'message, 'equal) test =
  ?here:'here -> ?message:'message -> ?equal:'equal -> expect:'a -> 'a -> unit

let run_test here ~incrementally ~all_at_once ~(test : _ test) =
  let int_map_operations_generator =
    Map_operations.quickcheck_generator (Base_quickcheck.Generator.int_inclusive 0 255)
  in
  Quickcheck.test int_map_operations_generator ~f:(fun operations ->
    let var = Incr.Var.create Int.Map.empty in
    let observer = Incremental.observe (incrementally (Incr.Var.watch var)) in
    Map_operations.run_operations ~into:var operations ~after_stabilize:(fun () ->
      let expected = all_at_once (Incr.Var.latest_value var) in
      let actual = Incremental.Observer.value_exn observer in
      test ~here:[ here ] ~expect:expected actual))
;;

let%test_unit "map_counti" =
  let incrementally input =
    Incr_map.mapi_count input ~comparator:(module Int) ~f:(fun ~key ~data -> key + data)
  in
  let all_at_once input =
    input
    |> Map.to_alist
    |> List.map ~f:(fun (k, v) -> k + v, ())
    |> Map.of_alist_multi (module Int)
    |> Map.map ~f:(List.length :> _ -> _)
  in
  run_test [%here] ~incrementally ~all_at_once ~test:[%test_result: int Int.Map.t]
;;

let%test_unit "map_count" =
  let incrementally input = Incr_map.map_count input ~comparator:(module Int) ~f:Fn.id in
  let all_at_once input =
    input
    |> Map.data
    |> List.map ~f:(fun d -> d, ())
    |> Map.of_alist_multi (module Int)
    |> Map.map ~f:(List.length :> _ -> _)
  in
  run_test [%here] ~incrementally ~all_at_once ~test:[%test_result: int Int.Map.t]
;;

let%test_unit "min_value" =
  let incrementally input = Incr_map.min_value input ~comparator:(module Int) in
  let all_at_once input = input |> Map.data |> List.min_elt ~compare:[%compare: int] in
  run_test [%here] ~incrementally ~all_at_once ~test:[%test_result: int option]
;;

let%test_unit "max_value" =
  let incrementally input = Incr_map.max_value input ~comparator:(module Int) in
  let all_at_once input = input |> Map.data |> List.max_elt ~compare:[%compare: int] in
  run_test [%here] ~incrementally ~all_at_once ~test:[%test_result: int option]
;;

let%test_unit "value_bounds" =
  let incrementally input = Incr_map.value_bounds input ~comparator:(module Int) in
  let all_at_once input =
    Option.both
      (input |> Map.data |> List.min_elt ~compare:[%compare: int])
      (input |> Map.data |> List.max_elt ~compare:[%compare: int])
  in
  run_test [%here] ~incrementally ~all_at_once ~test:[%test_result: (int * int) option]
;;
