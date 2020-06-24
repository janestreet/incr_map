open! Core
open! Import

let%test_unit "correctness" =
  let f ~key ~data = String.contains key 'a' && Int.is_positive data in
  let var = Incr.Var.create String.Map.empty in
  let observer = Incremental.observe (Incr_map.counti (Incr.Var.watch var) ~f) in
  Quickcheck.test
    (Map.quickcheck_generator
       (module String)
       [%quickcheck.generator: string]
       [%quickcheck.generator: int])
    ~f:(fun map ->
      Incr.Var.set var map;
      Incr.stabilize ();
      [%test_result: int]
        ~expect:(Map.counti map ~f)
        (Incremental.Observer.value_exn observer))
;;
