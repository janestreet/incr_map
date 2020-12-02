open! Core
open! Import

let%test_unit "correctness" =
  let f x = x in
  let var = Incr.Var.create Int.Map.empty in
  let observer =
    Incremental.observe (Incr_map.sum (Incr.Var.watch var) (module Int) ~f)
  in
  Quickcheck.test
    (Map_operations.quickcheck_generator [%quickcheck.generator: int])
    ~f:(fun operations ->
      Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
        [%test_result: int]
          ~expect:(Incr.Var.latest_value var |> Map.data |> List.sum (module Int) ~f)
          (Incremental.Observer.value_exn observer)))
;;
