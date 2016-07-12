open Core_kernel.Std

module Make (Incr : Incremental_kernel.Incremental_intf.S) = struct

  let diff_map i ~f =
    let open Incr.Let_syntax in
    let old = ref None in
    let%map a = i in
    let b = f ~old:!old a in
    old := Some (a, b);
    b

  let unordered_fold ?(data_equal=phys_equal) map ~init ~f ~f_inverse =
    diff_map map ~f:(fun ~old new_in ->
      let old_in, old_out =
        match old with
        | None -> (Map.empty ~comparator:(Map.comparator new_in), init)
        | Some x -> x
      in
      Sequence.fold ~init:old_out (Map.symmetric_diff old_in new_in ~data_equal)
        ~f:(fun acc (key, change) ->
          match change with
          | `Left old -> f_inverse ~key ~data:old acc
          | `Right new_ -> f ~key ~data:new_ acc
          | `Unequal (old, new_) ->
            f ~key ~data:new_ (f_inverse ~key ~data:old acc)))

  (** Captures the comparator (which can't change anyway, since the type determines the
      comparator) by freezing the corresponding map.  Note that by first using Incr.map to
      get the comparator out of the map, we allow the initial map itself to be garbage
      collected *)
  let with_comparator map f =
    Incr.bind (Incr.freeze (Incr.map map ~f:Map.comparator)) f

  let filter_mapi ?(data_equal=phys_equal) map ~f =
    diff_map map ~f:(fun ~old input -> match old with
      | None -> Map.filter_mapi input ~f
      | Some (old_input, old_output) ->
        Map.symmetric_diff old_input input ~data_equal
        |> Sequence.fold ~init:old_output ~f:(fun output (key, change) ->
          match change with
          | `Left _ -> Map.remove output key
          | `Right new_data | `Unequal (_, new_data) ->
            match f ~key ~data:new_data with
            | None -> Map.remove output key
            | Some output_data -> Map.add output ~key ~data:output_data))
  ;;

  let diff_map2 i1 i2 ~f =
    let old = ref None in
    Incr.map2 i1 i2 ~f:(fun a1 a2 ->
      let b = f ~old:!old a1 a2 in
      old := Some (a1, a2, b);
      b)

  let merge m1 m2 ~f =
    diff_map2 m1 m2 ~f:(fun ~old new_in1 new_in2 ->
      let (old_in1, old_in2, old_out) =
        match old with
        | None ->
          let empty = Map.empty ~comparator:(Map.comparator new_in1) in
          (empty, empty, empty)
        | Some x -> x
      in
      let touched_keys_with_dups =
        Sequence.append
          (Map.symmetric_diff old_in1 new_in1 ~data_equal:phys_equal
           |> Sequence.map ~f:fst)
          (Map.symmetric_diff old_in2 new_in2 ~data_equal:phys_equal
           |> Sequence.map ~f:fst)
      in
      Sequence.fold ~init:old_out touched_keys_with_dups
        ~f:(fun acc key ->
          let opt_update x =
            match f ~key x with
            | None -> Map.remove acc key
            | Some data -> Map.add acc ~key ~data
          in
          match Map.find new_in1 key, Map.find new_in2 key with
          | None, None -> Map.remove acc key
          | Some v1, None -> opt_update (`Left v1)
          | None, Some v2 -> opt_update (`Right v2)
          | Some v1, Some v2 -> opt_update (`Both (v1,v2))
        )
    )

  let filter_mapi_with_comparator' (type k) (type v1) (type v2) (type cmp)
        ?cutoff
        ?(data_equal=phys_equal)
        (lhs : (k,v1,cmp) Map.t Incr.t)
        ~(comparator : (k, cmp) Comparator.t)
        ~f
    : (k,v2,cmp) Map.t Incr.t
    =
    let module E = Incr.Expert in
    let empty_map = Map.empty ~comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let acc = ref empty_map in
    let result =
      E.Node.create (fun () -> !acc)
    in
    let rec lhs_change = lazy (Incr.map lhs ~f:(fun map ->
      let symmetric_diff =
        Map.symmetric_diff ~data_equal !prev_map map
      in
      let new_nodes =
        Sequence.fold symmetric_diff ~init:!prev_nodes
          ~f:(fun nodes (key, changed) ->
            match changed with
            | `Unequal _ ->
              let node, _dep = Map.find_exn nodes key in
              E.Node.make_stale node;
              nodes
            | `Left _ ->
              let (node, dep) = Map.find_exn nodes key in
              let nodes = Map.remove nodes key in
              E.Node.remove_dependency result dep;
              acc := Map.remove !acc key;
              E.Node.invalidate node;
              nodes
            | `Right _ ->
              let node = E.Node.create (fun () ->
                Map.find_exn !prev_map key
              ) in
              Option.iter cutoff ~f:(fun c -> Incr.set_cutoff (E.Node.watch node) c);
              E.Node.add_dependency node (E.Dependency.create (force lhs_change));
              let user_function_dep =
                E.Dependency.create
                  (f ~key ~data:(E.Node.watch node))
                  ~on_change:(fun opt ->
                    let old = !acc in
                    acc := (
                      match opt with
                      | None -> if Map.mem old key then Map.remove old key else old
                      | Some v -> Map.add old ~key ~data:v))
              in
              E.Node.add_dependency result user_function_dep;
              Map.add nodes ~key ~data:(node, user_function_dep))
      in
      prev_nodes := new_nodes;
      prev_map := map;
    ))
    in
    E.Node.add_dependency result (E.Dependency.create (force lhs_change));
    E.Node.watch result
  ;;

  let filter_mapi' ?cutoff ?data_equal map ~f =
    with_comparator map (fun comparator ->
      filter_mapi_with_comparator' ?cutoff ?data_equal map ~f ~comparator)
  ;;

  let flatten map =
    let module E = Incr.Expert in
    let result = ref (Map.empty ~comparator:(Map.comparator map)) in
    let node = E.Node.create (fun () -> !result) in
    Map.iteri map ~f:(fun ~key ~data:incr ->
      E.Node.add_dependency node
        (E.Dependency.create incr ~on_change:(fun a ->
          result := Map.add !result ~key ~data:a)));
    E.Node.watch node
  ;;

  let join_with_comparator map_incr ~comparator =
    let module E = Incr.Expert in
    let empty_map = Map.empty ~comparator in
    (* These map refs are initialised as part of outside_dep. *)
    let result_map = ref empty_map in
    let old_map_of_incrs = ref empty_map in
    let current_dependencies = ref empty_map in
    let result = E.Node.create (fun () -> !result_map) in
    let result_dependency_on incr ~key =
      E.Dependency.create incr ~on_change:(fun data ->
        result_map := Map.add !result_map ~key ~data)
    in
    let remove_result_map key =
      result_map := Map.remove !result_map key
    in
    let outside_dep =
      E.Dependency.create map_incr ~on_change:(fun map_of_incrs ->
        let sequence =
          Map.symmetric_diff ~data_equal:phys_equal
            !old_map_of_incrs map_of_incrs
        in
        let new_dependency_map =
          Sequence.fold sequence ~init:!current_dependencies
            ~f:(fun current_dependencies (key, diff) ->
              match diff with
              | `Left _old_incr  ->
                let dep = Map.find_exn current_dependencies key in
                E.Node.remove_dependency result dep;
                remove_result_map key;
                Map.remove current_dependencies key
              | `Right new_incr ->
                let new_dep = result_dependency_on new_incr ~key in
                E.Node.add_dependency result new_dep;
                Map.add current_dependencies ~key ~data:new_dep
              | `Unequal (_, new_incr) ->
                let dep = Map.find_exn current_dependencies key in
                E.Node.remove_dependency result dep;
                remove_result_map key;
                let new_dep = result_dependency_on new_incr ~key in
                E.Node.add_dependency result new_dep;
                Map.add current_dependencies ~key ~data:new_dep
            )
        in
        current_dependencies := new_dependency_map;
        old_map_of_incrs := map_of_incrs
      )
    in
    E.Node.add_dependency result outside_dep;
    E.Node.watch result
  ;;

  let join map =
    with_comparator map (fun comparator ->
      join_with_comparator map ~comparator)
  ;;
end
