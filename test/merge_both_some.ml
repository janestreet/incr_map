open! Core
open! Import

let%test_unit "merge_both_some" =
  Quickcheck.test
    ~sexp_of:
      [%sexp_of: (int, int) Map_operations.t list * (int, int) Map_operations.t list]
    (Quickcheck.Generator.both
       (Map_operations.quickcheck_generator Int.quickcheck_generator)
       (Map_operations.quickcheck_generator Int.quickcheck_generator))
    ~f:(fun (a_ops, b_ops) ->
      let m1 = Incr.Var.create Int.Map.empty in
      let watch_m1 = Incr.Var.watch m1 in
      let m2 = Incr.Var.create Int.Map.empty in
      let watch_m2 = Incr.Var.watch m2 in
      let fast =
        Incr_map.merge_both_some watch_m1 watch_m2 ~f:(fun ~key:_ v1 v2 -> v1 + v2)
      in
      let slow =
        let%map watch_m1 and watch_m2 in
        Map.merge watch_m1 watch_m2 ~f:(fun ~key:_ -> function
          | `Left _ | `Right _ -> None
          | `Both (a, b) -> Some (a + b))
      in
      let fast_obs = Incr.observe fast in
      let slow_obs = Incr.observe slow in
      Map_operations.run_operations a_ops ~into:m1 ~after_stabilize:(fun () ->
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_obs)
          (Incr.Observer.value_exn fast_obs));
      Map_operations.run_operations b_ops ~into:m2 ~after_stabilize:(fun () ->
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_obs)
          (Incr.Observer.value_exn fast_obs)))
;;
