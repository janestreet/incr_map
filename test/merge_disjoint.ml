open! Core
open! Import

let%test_unit "merge_disjoint" =
  Quickcheck.test
    ~sexp_of:
      [%sexp_of:
        (int, int) Map_operations.t list
        * (int, int) Map_operations.t list
        * int Int.Map.t
        * int Int.Map.t]
    (let%map.Quickcheck.Generator a_ops =
       Map_operations.quickcheck_generator Int.quickcheck_generator
     and b_ops = Map_operations.quickcheck_generator Int.quickcheck_generator
     and a_initial =
       Map.quickcheck_generator
         (module Int)
         Int.quickcheck_generator
         Int.quickcheck_generator
     and b_initial =
       Map.quickcheck_generator
         (module Int)
         Int.quickcheck_generator
         Int.quickcheck_generator
     in
     a_ops, b_ops, a_initial, b_initial)
    ~f:(fun (a_ops, b_ops, a_initial, b_initial) ->
      let m1 = Incr.Var.create a_initial in
      let watch_m1 = Incr.Var.watch m1 in
      let m2 = Incr.Var.create b_initial in
      let watch_m2 =
        let%map m1 = watch_m1
        and m2 = Incr.Var.watch m2 in
        (* make sure they're disjoint *)
        Map.fold m1 ~init:m2 ~f:(fun ~key ~data:_ acc -> Map.remove acc key)
      in
      let fast = Incr_map.merge_disjoint watch_m1 watch_m2 in
      let slow =
        let%map watch_m1 = watch_m1
        and watch_m2 = watch_m2 in
        Map.merge watch_m1 watch_m2 ~f:(fun ~key:_ -> function
          | `Left x | `Right x -> Some x
          | `Both _ -> assert false)
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
