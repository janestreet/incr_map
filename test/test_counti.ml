open! Core
open! Import

let%test_unit "correctness" =
  let f ~key ~data = String.contains data 'a' && key % 2 = 0 in
  let var = Incr.Var.create Int.Map.empty in
  let observer = Incremental.observe (Incr_map.counti (Incr.Var.watch var) ~f) in
  Quickcheck.test
    (Map_operations.quickcheck_generator [%quickcheck.generator: string])
    ~f:(fun operations ->
      Map_operations.run_operations ~into:var operations ~after_stabilize:(fun () ->
        [%test_result: int]
          ~expect:(Map.counti (Incr.Var.latest_value var) ~f)
          (Incremental.Observer.value_exn observer)))
;;
