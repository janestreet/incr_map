open! Core
open! Import

let%test_unit "correctness" =
  let f x = x in
  let var = Incr.Var.create String.Map.empty in
  let observer =
    Incremental.observe (Incr_map.sum (Incr.Var.watch var) (module Int) ~f)
  in
  Quickcheck.test
    (Map.quickcheck_generator
       (module String)
       [%quickcheck.generator: string]
       [%quickcheck.generator: int])
    ~f:(fun map ->
      Incr.Var.set var map;
      Incr.stabilize ();
      [%test_result: int]
        ~expect:(map |> Map.data |> List.sum (module Int) ~f)
        (Incremental.Observer.value_exn observer))
;;
