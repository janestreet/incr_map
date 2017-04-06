open! Core_kernel
open! Int.Replace_polymorphic_compare

(** This type lets us capture the kind of map function being performed, so we can with
    one implementation perform map and filter-map operations.

    Here, ['input_data] is the type of data in the input map, ['output_data] is the type
    of data in the output map, and ['f_output] is the return type of the [~f] function
    passed to the mapping function. *)
module Map_type = struct
  type ('input_data, 'output_data, 'f_output) t =
    | Map : ('input_data, 'output_data, 'output_data) t
    | Filter_map : ('input_data, 'output_data, 'output_data option) t
    (* The extra type variable 'a is to allow in future:
        | Filter : ('output_data, 'output_data, bool) t *)
end

module Make (Incr: Incremental_kernel.Incremental.S_without_times) = struct

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
    Incr.bind (Incr.freeze (Incr.map map ~f:Map.comparator)) ~f

  let generic_mapi (type input_data) (type output_data) (type f_output)
        (witness : (input_data, output_data, f_output) Map_type.t)
        ?(data_equal=phys_equal)
        (map : ('key, input_data, 'cmp) Map.t Incr.t)
        ~(f : key:'key -> data:input_data -> f_output)
    =
    diff_map map ~f:(fun ~old input -> match old with
      | None ->
        begin match witness with
        | Map_type.Map -> (Map.mapi input ~f : ('key, output_data, 'cmp) Map.t)
        | Map_type.Filter_map -> Map.filter_mapi input ~f
        end
      | Some (old_input, old_output) ->
        Map.symmetric_diff old_input input ~data_equal
        |> Sequence.fold ~init:old_output ~f:(fun output (key, change) ->
          match change with
          | `Left _ -> Map.remove output key
          | `Right new_data | `Unequal (_, new_data) ->
            let res = f ~key ~data:new_data in
            match witness with
            | Map_type.Map -> Map.add output ~key ~data:res
            | Map_type.Filter_map ->
              match res with
              | None -> Map.remove output key
              | Some output_data -> Map.add output ~key ~data:output_data))
  ;;

  let mapi ?data_equal map ~f =
    generic_mapi Map ?data_equal map ~f

  let filter_mapi ?data_equal map ~f =
    generic_mapi Filter_map ?data_equal map ~f

  let diff_map2 i1 i2 ~f =
    let old = ref None in
    Incr.map2 i1 i2 ~f:(fun a1 a2 ->
      let b = f ~old:!old a1 a2 in
      old := Some (a1, a2, b);
      b)

  let merge
        ?(data_equal_left=phys_equal)
        ?(data_equal_right=phys_equal)
        left_map
        right_map
        ~f
    =
    diff_map2 left_map right_map ~f:(fun ~old new_left_map new_right_map ->
      let comparator = Map.comparator new_left_map in
      let (old_left_map, old_right_map, old_output) =
        match old with
        | None ->
          let empty = Map.empty ~comparator in
          (empty, empty, empty)
        | Some x -> x
      in
      let left_diff =
        Map.symmetric_diff old_left_map new_left_map ~data_equal:data_equal_left
      in
      let right_diff =
        Map.symmetric_diff old_right_map new_right_map ~data_equal:data_equal_right
      in
      (* We merge the two sides of the diffs together so we can make sure to handle each
         key exactly once. This relies on symmetric diff giving sorted output. *)
      Sequence.merge_with_duplicates left_diff right_diff
        ~cmp:(fun (left_key, _) (right_key, _) ->
          comparator.compare left_key right_key)
      |> Sequence.fold ~init:old_output ~f:(fun output diff_element ->
          let key =
            match diff_element with
            | Left (key, _) | Right (key, _) -> key
            | Both ((left_key, _), (right_key, _)) ->
              assert (comparator.compare left_key right_key = 0);
              left_key
          in
          (* These values represent whether there is data for the given key in the new
             input in the left and right map. *)
          let left_data_opt, right_data_opt =
            let new_data = function
              | `Left _ -> None
              | `Right x | `Unequal (_, x) -> Some x
            in
            match diff_element with
            | Both ((_, left_diff), (_, right_diff)) ->
              (new_data left_diff, new_data right_diff)
            | Left (_, left_diff) ->
              (new_data left_diff, Map.find new_right_map key)
            | Right (_, right_diff) ->
              (Map.find new_left_map key, new_data right_diff)
          in
          let output_data_opt =
            match left_data_opt, right_data_opt with
            | None   , None   -> None
            | Some x , None   -> f ~key (`Left x)
            | None   , Some y -> f ~key (`Right y)
            | Some x , Some y -> f ~key (`Both (x, y))
          in
          match output_data_opt with
          | None      -> Map.remove output key
          | Some data -> Map.add output ~key ~data))
  ;;

  let generic_mapi_with_comparator'
        (type input_data) (type output_data) (type f_output)
        (witness : (input_data, output_data, f_output) Map_type.t)
        ?cutoff
        ?(data_equal=phys_equal)
        (lhs : ('key,input_data,'cmp) Map.t Incr.t)
        ~(comparator : ('key,'cmp) Comparator.t)
        ~(f : key:'key -> data:input_data Incr.t -> f_output Incr.t)
    : ('key,output_data,'cmp) Map.t Incr.t
    =
    let module E = Incr.Expert in
    let empty_map = Map.empty ~comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let acc : ('key, output_data, 'cmp) Map.t ref = ref empty_map in
    let result =
      E.Node.create (fun () -> !acc)
    in
    let (on_inner_change : key:'key -> f_output -> unit) =
      match witness with
      | Map_type.Map ->
        (fun ~key data -> acc := Map.add !acc ~key ~data)
      | Map_type.Filter_map ->
        (fun ~key opt ->
           let old = !acc in
           acc := (
             match opt with
             | None -> if Map.mem old key then Map.remove old key else old
             | Some data -> Map.add old ~key ~data))
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
                  ~on_change:(on_inner_change ~key)
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
      generic_mapi_with_comparator' Map_type.Filter_map ?cutoff ?data_equal map ~f ~comparator)
  ;;

  let mapi' ?cutoff ?data_equal map ~f =
    with_comparator map (fun comparator ->
      generic_mapi_with_comparator' Map_type.Map ?cutoff ?data_equal map ~f ~comparator)
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
    let result_map = ref empty_map in
    let old_map_of_incrs = ref empty_map in
    let current_dependencies = ref empty_map in
    let result = E.Node.create (fun () -> !result_map) in
    let add_subnode current_dependencies ~key ~data_node =
      let new_dep =
        E.Dependency.create data_node ~on_change:(fun data ->
          result_map := Map.add !result_map ~key ~data)
      in
      E.Node.add_dependency result new_dep;
      Map.add current_dependencies ~key ~data:new_dep
    in
    let remove_subnode current_dependencies ~key =
      let dep = Map.find_exn current_dependencies key in
      E.Node.remove_dependency result dep;
      result_map := Map.remove !result_map key;
      Map.remove current_dependencies key
    in
    let lhs_change = Incr.map map_incr ~f:(fun map_of_incrs ->
      let sequence =
        Map.symmetric_diff ~data_equal:phys_equal
          !old_map_of_incrs map_of_incrs
      in
      let new_dependency_map =
        Sequence.fold sequence ~init:!current_dependencies
          ~f:(fun current_dependencies (key, diff) ->
            match diff with
            | `Left _ ->
              remove_subnode current_dependencies ~key
            | `Right data_node ->
              add_subnode current_dependencies ~key ~data_node
            | `Unequal (_, data_node) ->
              remove_subnode current_dependencies ~key
              |> add_subnode ~key ~data_node)
      in
      current_dependencies := new_dependency_map;
      old_map_of_incrs := map_of_incrs)
    in
    E.Node.add_dependency result (E.Dependency.create lhs_change);
    E.Node.watch result
  ;;

  let join map =
    with_comparator map (fun comparator ->
      join_with_comparator map ~comparator)
  ;;

  let subrange ?(data_equal=phys_equal) map_incr range =
    diff_map2 map_incr range ~f:(fun ~old map range ->
      let compare = (Map.comparator map).compare in
      let equal l r = compare l r = 0 in
      match range with
      | None ->
        (* Empty new range means empty map *)
        Map.empty ~comparator:(Map.comparator map)
      | Some ((min, max) as range) ->
        let from_scratch () =
          Map.subrange map ~lower_bound:(Incl min) ~upper_bound:(Incl max)
        in
        match old with
        | None | Some (_, None, _) ->
          (* no old range *)
          from_scratch ()
        | Some (_, Some (old_min, old_max), _)
          when (compare old_min old_max > 0
                || compare old_max min < 0 || compare old_min max > 0) ->
          (* empty old range or old range disjoint with new *)
          from_scratch ()
        | Some (old_map, Some ((old_min, old_max) as old_range), old_res) ->
          with_return (fun {return} ->
            (* Returns true iff the key is in both new and old ranges *)
            let in_range_intersection key =
              compare min key        <= 0 && compare key max     <= 0
              && compare old_min key <= 0 && compare key old_max <= 0
            in
            (* Apply changes to keys which are in the intersection of both ranges.

               [outside] is the number of updates outside the range intersection that we
               tolerate before giving up and reconstructing based on the new range. This
               is an optimisation in the case that the map changes in a very big way, at
               which point computing based on the new range is cheaper.  *)
            let apply_diff_in_intersection (outside, map) (key, data) =
              if in_range_intersection key then (
                match data with
                | `Left _ -> (outside, Map.remove map key)
                | `Right data | `Unequal (_, data) -> (outside, Map.add map ~key ~data)
              ) else (
                let outside = outside - 1 in
                if outside < 0 then return (from_scratch ())
                else (outside, Map.remove map key)
              )
            in
            (* First update the keys in /both/ the old and the new range. *)
            let with_updated_values_in_intersection =
              (* Cutoff the big diff computation if we reach O(|submap|) number of
                 changes that are outside the range *)
              let outside_cutoff = (Map.length old_res) / 4 in
              Map.symmetric_diff ~data_equal old_map map
              |> Sequence.fold ~init:(outside_cutoff, old_res)
                   ~f:apply_diff_in_intersection
              |> snd
            in
            if (Tuple2.equal ~eq1:equal ~eq2:equal old_range range) then
              (* There are no keys to remove and everything in range is updated. *)
              with_updated_values_in_intersection
            else begin
              (* Remove any keys which are not in the new range. *)
              let without_keys_out_of_range =
                Map.subrange with_updated_values_in_intersection
                  ~lower_bound:(Incl min) ~upper_bound:(Incl max)
              in
              (* Add in any keys which are in the new range but not the old range. *)
              let with_new_keys_now_in_range =
                let map_append_exn lower_part upper_part =
                  match Map.append ~lower_part ~upper_part with
                  | `Ok map -> map
                  | `Overlapping_key_ranges ->
                    failwith "impossible case: BUG in incr_map.ml subrange"
                in
                let lower_part =
                  Map.subrange map ~lower_bound:(Incl min) ~upper_bound:(Excl old_min)
                and upper_part =
                  Map.subrange map ~lower_bound:(Excl old_max) ~upper_bound:(Incl max)
                in
                map_append_exn lower_part
                  (map_append_exn without_keys_out_of_range upper_part)
              in
              with_new_keys_now_in_range
            end
          )
    )
  ;;

end
