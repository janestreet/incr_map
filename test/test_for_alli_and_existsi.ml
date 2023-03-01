open! Core
open! Import

let quickcheck_test ~incremental ~non_incremental =
  let f ~key ~data = String.contains data 'a' && key % 2 = 0 in
  let var = Incr.Var.create Int.Map.empty in
  let observer =
    Incremental.observe (incremental ?data_equal:None (Incr.Var.watch var) ~f)
  in
  Quickcheck.test
    (Map_operations.quickcheck_generator [%quickcheck.generator: string])
    ~f:(fun operations ->
      Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
        [%test_result: bool]
          ~expect:(non_incremental (Incr.Var.latest_value var) ~f)
          (Incremental.Observer.value_exn observer)))
;;

let%test_unit "Incr_map.for_alli" =
  quickcheck_test
    ~incremental:(Incr_map.for_alli ?instrumentation:None)
    ~non_incremental:(Map.for_alli :> _ -> f:(key:_ -> data:_ -> _) -> _)
;;

let%test_unit "Incr_map.existsi" =
  quickcheck_test
    ~incremental:(Incr_map.existsi ?instrumentation:None)
    ~non_incremental:(Map.existsi :> _ -> f:(key:_ -> data:_ -> _) -> _)
;;
