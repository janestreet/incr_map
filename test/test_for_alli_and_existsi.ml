open! Core
open! Import

let quickcheck_test ~incremental ~non_incremental =
  let f ~key ~data = String.contains key 'a' && Int.is_positive data in
  let var = Incr.Var.create String.Map.empty in
  let observer =
    Incremental.observe (incremental ?data_equal:None (Incr.Var.watch var) ~f)
  in
  Quickcheck.test
    (Map.quickcheck_generator
       (module String)
       [%quickcheck.generator: string]
       [%quickcheck.generator: int])
    ~f:(fun map ->
      Incr.Var.set var map;
      Incr.stabilize ();
      [%test_result: bool]
        ~expect:(non_incremental map ~f)
        (Incremental.Observer.value_exn observer))
;;

let%test_unit "Incr_map.for_alli" =
  quickcheck_test ~incremental:Incr_map.for_alli ~non_incremental:Map.for_alli
;;

let%test_unit "Incr_map.existsi" =
  quickcheck_test ~incremental:Incr_map.existsi ~non_incremental:Map.existsi
;;
