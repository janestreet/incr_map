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

module Make (Incr : Incremental.S) = struct
  let with_old i ~f =
    let open Incr.Let_syntax in
    let old = ref None in
    let%map a = i in
    let b = f ~old:!old a in
    old := Some (a, b);
    b
  ;;

  let unordered_fold ?(data_equal = phys_equal) ?update map ~init ~add ~remove =
    let update =
      let default ~key ~old_data ~new_data acc =
        add ~key ~data:new_data (remove ~key ~data:old_data acc)
      in
      Option.value update ~default
    in
    with_old map ~f:(fun ~old new_in ->
      match old with
      | None -> Map.fold ~init ~f:add new_in
      | Some (old_in, old_out) ->
        Map.fold_symmetric_diff
          ~init:old_out
          old_in
          new_in
          ~data_equal
          ~f:(fun acc (key, change) ->
            match change with
            | `Left old -> remove ~key ~data:old acc
            | `Right new_ -> add ~key ~data:new_ acc
            | `Unequal (old, new_) -> update ~key ~old_data:old ~new_data:new_ acc))
  ;;

  let with_comparator' get_comparator x f =
    Incr.bind (Incr.freeze (Incr.map x ~f:get_comparator)) ~f
  ;;

  (** Captures the comparator (which can't change anyway, since the type determines the
      comparator) by freezing the corresponding map.  Note that by first using Incr.map to
      get the comparator out of the map, we allow the initial map itself to be garbage
      collected *)
  let with_comparator map f = with_comparator' Map.comparator map f

  let of_set set =
    with_comparator' Set.comparator set (fun comparator ->
      let old_input = ref (Set.Using_comparator.empty ~comparator) in
      let old_output = ref (Map.Using_comparator.empty ~comparator) in
      Incr.map set ~f:(fun new_input ->
        let new_output =
          Sequence.fold
            (Set.symmetric_diff !old_input new_input)
            ~init:!old_output
            ~f:(fun output ->
              function
              | First k -> Map.remove output k
              | Second k -> Map.add_exn output ~key:k ~data:())
        in
        old_input := new_input;
        old_output := new_output;
        new_output))
  ;;

  let generic_mapi
        (type input_data output_data f_output)
        (witness : (input_data, output_data, f_output) Map_type.t)
        ?(data_equal = phys_equal)
        (map : ('key, input_data, 'cmp) Map.t Incr.t)
        ~(f : key:'key -> data:input_data -> f_output)
    =
    with_old map ~f:(fun ~old input ->
      match old with
      | None ->
        (match witness with
         | Map_type.Map -> (Map.mapi input ~f : ('key, output_data, 'cmp) Map.t)
         | Map_type.Filter_map -> Map.filter_mapi input ~f)
      | Some (old_input, old_output) ->
        Map.fold_symmetric_diff
          old_input
          input
          ~data_equal
          ~init:old_output
          ~f:(fun output (key, change) ->
            match change with
            | `Left _ -> Map.remove output key
            | `Right new_data | `Unequal (_, new_data) ->
              let res = f ~key ~data:new_data in
              (match witness with
               | Map_type.Map -> Map.set output ~key ~data:res
               | Map_type.Filter_map ->
                 (match res with
                  | None -> Map.remove output key
                  | Some output_data -> Map.set output ~key ~data:output_data))))
  ;;

  let mapi ?data_equal map ~f = generic_mapi Map ?data_equal map ~f
  let filter_mapi ?data_equal map ~f = generic_mapi Filter_map ?data_equal map ~f

  let with_old2 i1 i2 ~f =
    let old = ref None in
    Incr.map2 i1 i2 ~f:(fun a1 a2 ->
      let b = f ~old:!old a1 a2 in
      old := Some (a1, a2, b);
      b)
  ;;

  let merge
        ?(data_equal_left = phys_equal)
        ?(data_equal_right = phys_equal)
        left_map
        right_map
        ~f
    =
    with_old2 left_map right_map ~f:(fun ~old new_left_map new_right_map ->
      let comparator = Map.comparator new_left_map in
      let old_left_map, old_right_map, old_output =
        match old with
        | None ->
          let empty = Map.Using_comparator.empty ~comparator in
          empty, empty, empty
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
      Sequence.merge_with_duplicates
        left_diff
        right_diff
        ~compare:(fun (left_key, _) (right_key, _) ->
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
            new_data left_diff, new_data right_diff
          | Left (_, left_diff) -> new_data left_diff, Map.find new_right_map key
          | Right (_, right_diff) ->
            Map.find new_left_map key, new_data right_diff
        in
        let output_data_opt =
          match left_data_opt, right_data_opt with
          | None, None -> None
          | Some x, None -> f ~key (`Left x)
          | None, Some y -> f ~key (`Right y)
          | Some x, Some y -> f ~key (`Both (x, y))
        in
        match output_data_opt with
        | None -> Map.remove output key
        | Some data -> Map.set output ~key ~data))
  ;;

  let generic_mapi_with_comparator'
        (type input_data output_data f_output)
        (witness : (input_data, output_data, f_output) Map_type.t)
        ?cutoff
        ?(data_equal = phys_equal)
        (lhs : ('key, input_data, 'cmp) Map.t Incr.t)
        ~(comparator : ('key, 'cmp) Comparator.t)
        ~(f : key:'key -> data:input_data Incr.t -> f_output Incr.t)
    : ('key, output_data, 'cmp) Map.t Incr.t
    =
    let module E = Incr.Expert in
    let empty_map = Map.Using_comparator.empty ~comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let acc : ('key, output_data, 'cmp) Map.t ref = ref empty_map in
    let result = E.Node.create (fun () -> !acc) in
    let (on_inner_change : key:'key -> f_output -> unit) =
      match witness with
      | Map_type.Map -> fun ~key data -> acc := Map.set !acc ~key ~data
      | Map_type.Filter_map ->
        fun ~key opt ->
          let old = !acc in
          acc
          := (match opt with
            | None -> if Map.mem old key then Map.remove old key else old
            | Some data -> Map.set old ~key ~data)
    in
    let rec lhs_change =
      lazy
        (Incr.map lhs ~f:(fun map ->
           let new_nodes =
             Map.fold_symmetric_diff
               ~data_equal
               !prev_map
               map
               ~init:!prev_nodes
               ~f:(fun nodes (key, changed) ->
                 match changed with
                 | `Unequal _ ->
                   let node, _dep = Map.find_exn nodes key in
                   E.Node.make_stale node;
                   nodes
                 | `Left _ ->
                   let node, dep = Map.find_exn nodes key in
                   let nodes = Map.remove nodes key in
                   E.Node.remove_dependency result dep;
                   acc := Map.remove !acc key;
                   E.Node.invalidate node;
                   nodes
                 | `Right _ ->
                   let node = E.Node.create (fun () -> Map.find_exn !prev_map key) in
                   Option.iter cutoff ~f:(fun c ->
                     Incr.set_cutoff (E.Node.watch node) c);
                   E.Node.add_dependency node (E.Dependency.create (force lhs_change));
                   let user_function_dep =
                     E.Dependency.create
                       (f ~key ~data:(E.Node.watch node))
                       ~on_change:(on_inner_change ~key)
                   in
                   E.Node.add_dependency result user_function_dep;
                   Map.set nodes ~key ~data:(node, user_function_dep))
           in
           prev_nodes := new_nodes;
           prev_map := map))
    in
    E.Node.add_dependency result (E.Dependency.create (force lhs_change));
    E.Node.watch result
  ;;

  let filter_mapi' ?cutoff ?data_equal map ~f =
    with_comparator map (fun comparator ->
      generic_mapi_with_comparator'
        Map_type.Filter_map
        ?cutoff
        ?data_equal
        map
        ~f
        ~comparator)
  ;;

  let mapi' ?cutoff ?data_equal map ~f =
    with_comparator map (fun comparator ->
      generic_mapi_with_comparator' Map_type.Map ?cutoff ?data_equal map ~f ~comparator)
  ;;

  let keys map =
    with_comparator map (fun comparator ->
      let add ~key ~data:_ acc = Set.add acc key in
      let remove ~key ~data:_ acc = Set.remove acc key in
      let data_equal _ _ = true in
      unordered_fold
        map
        ~init:(Set.Using_comparator.empty ~comparator)
        ~data_equal
        ~add
        ~remove)
  ;;

  let flatten map =
    let module E = Incr.Expert in
    let result = ref (Map.Using_comparator.empty ~comparator:(Map.comparator map)) in
    let node = E.Node.create (fun () -> !result) in
    Map.iteri map ~f:(fun ~key ~data:incr ->
      E.Node.add_dependency
        node
        (E.Dependency.create incr ~on_change:(fun a ->
           result := Map.set !result ~key ~data:a)));
    E.Node.watch node
  ;;

  let join_with_comparator map_incr ~comparator =
    let module E = Incr.Expert in
    let empty_map = Map.Using_comparator.empty ~comparator in
    let result_map = ref empty_map in
    let old_map_of_incrs = ref empty_map in
    let current_dependencies = ref empty_map in
    let result = E.Node.create (fun () -> !result_map) in
    let add_subnode current_dependencies ~key ~data_node =
      let new_dep =
        E.Dependency.create data_node ~on_change:(fun data ->
          result_map := Map.set !result_map ~key ~data)
      in
      E.Node.add_dependency result new_dep;
      Map.set current_dependencies ~key ~data:new_dep
    in
    let remove_subnode current_dependencies ~key =
      let dep = Map.find_exn current_dependencies key in
      E.Node.remove_dependency result dep;
      result_map := Map.remove !result_map key;
      Map.remove current_dependencies key
    in
    let lhs_change =
      Incr.map map_incr ~f:(fun map_of_incrs ->
        let new_dependency_map =
          Map.fold_symmetric_diff
            ~data_equal:phys_equal
            !old_map_of_incrs
            map_of_incrs
            ~init:!current_dependencies
            ~f:(fun current_dependencies (key, diff) ->
              match diff with
              | `Left _ -> remove_subnode current_dependencies ~key
              | `Right data_node -> add_subnode current_dependencies ~key ~data_node
              | `Unequal (_, data_node) ->
                remove_subnode current_dependencies ~key |> add_subnode ~key ~data_node)
        in
        current_dependencies := new_dependency_map;
        old_map_of_incrs := map_of_incrs)
    in
    E.Node.add_dependency result (E.Dependency.create lhs_change);
    E.Node.watch result
  ;;

  let join map =
    with_comparator map (fun comparator -> join_with_comparator map ~comparator)
  ;;

  module Separate_state = struct
    type ('k, 'v, 'cmp) t =
      { mutable input_map : ('k, 'v, 'cmp) Map.t
      ; mutable expert_nodes : ('k, 'v Incr.Expert.Node.t, 'cmp) Map.t
      ; mutable output_map : ('k, 'v Incr.t, 'cmp) Map.t
      }

    let create comparator =
      let empty = Map.Using_comparator.empty ~comparator in
      { input_map = empty; expert_nodes = empty; output_map = empty }
    ;;

    let create_lookup_node t key =
      Incr.Expert.Node.create (fun () -> Map.find_exn t.input_map key)
    ;;
  end

  let separate input_map ~data_equal =
    with_comparator input_map (fun comparator ->
      let state = Separate_state.create comparator in
      let output_map_node = Incr.Expert.Node.create (fun () -> state.output_map) in
      let make_node_depend_on_input_map_changed node ~input_map_changed =
        let dependency =
          Incr.Expert.Dependency.create (Lazy.force_val input_map_changed)
        in
        Incr.Expert.Node.add_dependency node dependency
      in
      (* We want to make nodes depend on [input_map_changed] so that [input_map_changed]
         is allowed to make them stale, but we do not want them to be recomputed for any
         other reason. So we make [input_map_changed] a unit incremental (that therefore
         never changes) and this way [output_map_node] and the lookup nodes will only be
         recomputed when they are explicitly made stale.
      *)
      let rec input_map_changed =
        lazy
          (Incr.map input_map ~f:(fun input_map ->
             let prev_input_map = state.input_map in
             let expert_nodes, output_map =
               Map.fold_symmetric_diff
                 prev_input_map
                 input_map
                 ~data_equal
                 ~init:(state.expert_nodes, state.output_map)
                 ~f:(fun (expert_nodes, output_map) (key, change) ->
                   match change with
                   | `Left _old_value ->
                     let old_node = Map.find_exn expert_nodes key in
                     Incr.Expert.Node.invalidate old_node;
                     Incr.Expert.Node.make_stale output_map_node;
                     Map.remove expert_nodes key, Map.remove output_map key
                   | `Right _new_value ->
                     let node = Separate_state.create_lookup_node state key in
                     make_node_depend_on_input_map_changed node ~input_map_changed;
                     Incr.Expert.Node.make_stale output_map_node;
                     ( Map.add_exn expert_nodes ~key ~data:node
                     , Map.add_exn
                         output_map
                         ~key
                         ~data:(Incr.Expert.Node.watch node) )
                   | `Unequal (_old_value, _new_value) ->
                     Incr.Expert.Node.make_stale (Map.find_exn expert_nodes key);
                     expert_nodes, output_map)
             in
             state.input_map <- input_map;
             state.expert_nodes <- expert_nodes;
             state.output_map <- output_map))
      in
      make_node_depend_on_input_map_changed output_map_node ~input_map_changed;
      Incr.Expert.Node.watch output_map_node)
  ;;

  let subrange
        (type k v cmp)
        ?(data_equal = phys_equal)
        (map_incr : (k, v, cmp) Map.t Incr.t)
        range
    =
    with_old2 map_incr range ~f:(fun ~old map range ->
      let compare = (Map.comparator map).compare in
      let equal l r = compare l r = 0 in
      let ( > ) a b = compare a b > 0
      and ( >= ) a b = compare a b >= 0
      and ( < ) a b = compare a b < 0
      and ( <= ) a b = compare a b <= 0 in
      let maybe_bound_equal a b : bool =
        match a, b with
        | Unbounded, Unbounded -> true
        | Unbounded, (Incl _ | Excl _) -> false
        | (Incl _ | Excl _), Unbounded -> false
        | Incl _, Excl _ -> false
        | Excl _, Incl _ -> false
        | Incl a, Incl b -> equal a b
        | Excl a, Excl b -> equal a b
      in
      let range_is_empty ~min ~max : bool =
        match min, max with
        | Unbounded, (Unbounded | Excl _ | Incl _) | (Excl _ | Incl _), Unbounded ->
          false
        | Incl min, Incl max -> min > max
        | Excl min, Excl max | Incl min, Excl max | Excl min, Incl max -> min >= max
      in
      let range_includes ~min ~max key : bool =
        (match min with
         | Unbounded -> true
         | Incl min -> min <= key
         | Excl min -> min < key)
        &&
        match max with
        | Unbounded -> true
        | Incl max -> max >= key
        | Excl max -> max > key
      in
      match range with
      | None ->
        (* Empty new range means empty map *)
        Map.Using_comparator.empty ~comparator:(Map.comparator map)
      | Some ((min, max) as range) ->
        let from_scratch () = Map.subrange map ~lower_bound:min ~upper_bound:max in
        (match old with
         | None | Some (_, None, _) ->
           (* no old range *)
           from_scratch ()
         | Some (_, Some (old_min, old_max), _)
           when range_is_empty ~min:old_min ~max:old_max
             || range_is_empty ~min ~max:old_max
             || range_is_empty ~min:old_min ~max ->
           (* empty old range or old range disjoint with new *)
           from_scratch ()
         | Some (old_map, Some ((old_min, old_max) as old_range), old_res) ->
           with_return (fun { return } ->
             (* Returns true iff the key is in both new and old ranges *)
             let in_range_intersection key =
               range_includes ~min ~max key
               && range_includes ~min:old_min ~max:old_max key
             in
             (* Apply changes to keys which are in the intersection of both ranges.

                [outside] is the number of updates outside the range intersection that we
                tolerate before giving up and reconstructing based on the new range. This
                is an optimisation in the case that the map changes in a very big way, at
                which point computing based on the new range is cheaper.  *)
             let apply_diff_in_intersection (outside, map) (key, data) =
               if in_range_intersection key
               then (
                 match data with
                 | `Left _ -> outside, Map.remove map key
                 | `Right data | `Unequal (_, data) -> outside, Map.set map ~key ~data)
               else (
                 let outside = outside - 1 in
                 if Int.O.(outside < 0)
                 then return (from_scratch ())
                 else outside, Map.remove map key)
             in
             (* First update the keys in /both/ the old and the new range. *)
             let with_updated_values_in_intersection =
               (* Cutoff the big diff computation if we reach O(|submap|) number of
                  changes that are outside the range *)
               let outside_cutoff = Map.length old_res / 4 in
               Map.fold_symmetric_diff
                 ~data_equal
                 old_map
                 map
                 ~init:(outside_cutoff, old_res)
                 ~f:apply_diff_in_intersection
               |> snd
             in
             if Tuple2.equal
                  ~eq1:maybe_bound_equal
                  ~eq2:maybe_bound_equal
                  old_range
                  range
             then
               (* There are no keys to remove and everything in range is updated. *)
               with_updated_values_in_intersection
             else (
               (* Remove any keys which are not in the new range. *)
               let without_keys_out_of_range =
                 Map.subrange
                   with_updated_values_in_intersection
                   ~lower_bound:min
                   ~upper_bound:max
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
                   match old_min with
                   | Unbounded ->
                     Map.Using_comparator.empty ~comparator:(Map.comparator map)
                   | Excl old_min ->
                     Map.subrange map ~lower_bound:min ~upper_bound:(Incl old_min)
                   | Incl old_min ->
                     Map.subrange map ~lower_bound:min ~upper_bound:(Excl old_min)
                 and upper_part =
                   match old_max with
                   | Unbounded ->
                     Map.Using_comparator.empty ~comparator:(Map.comparator map)
                   | Excl old_max ->
                     Map.subrange map ~lower_bound:(Incl old_max) ~upper_bound:max
                   | Incl old_max ->
                     Map.subrange map ~lower_bound:(Excl old_max) ~upper_bound:max
                 in
                 map_append_exn
                   lower_part
                   (map_append_exn without_keys_out_of_range upper_part)
               in
               with_new_keys_now_in_range))))
  ;;


  (** Find two keys in map by index, O(n). We use just one fold (two Map.nth would use two)
      and optimize for keys close to either beginning or end by using either fold or
      fold_right.
  *)
  module Key_status = struct
    type 'k t =
      | Known of 'k
      | Known_none
      | Unknown

    let is_known = function
      | Unknown -> false
      | _ -> true
    ;;

    let to_option = function
      | Unknown | Known_none -> None
      | Known k -> Some k
    ;;
  end

  let key_range_linear (type k) ~from ~to_ (map : (k, _, _) Map.t)
    : (k * k option) option
    =
    let open Key_status in
    let len = Map.length map in
    let begin_key = if Int.( >= ) from len then Known_none else Unknown in
    let end_key = if Int.( >= ) to_ len then Known_none else Unknown in
    let find_keys fold ~start_pos ~advance_pos =
      with_return (fun { return } ->
        fold
          map
          ~init:(begin_key, end_key, start_pos)
          ~f:(fun ~key ~data:_ (begin_key, end_key, pos) ->
            let begin_key = if Int.( = ) pos from then Known key else begin_key in
            let end_key = if Int.( = ) pos to_ then Known key else end_key in
            if is_known begin_key && is_known end_key
            then return (begin_key, end_key, pos)
            else begin_key, end_key, advance_pos pos))
    in
    let begin_key, end_key, _ =
      (* Searching from left takes O(to_), from right - O(len - from), so select the
         smaller one. *)
      if to_ < len - from
      then find_keys Map.fold ~start_pos:0 ~advance_pos:(fun pos -> pos + 1)
      else
        find_keys Map.fold_right ~start_pos:(len - 1) ~advance_pos:(fun pos -> pos - 1)
    in
    Option.map (Key_status.to_option begin_key) ~f:(fun begin_key ->
      begin_key, Key_status.to_option end_key)
  ;;

  let nth_from_either_side (type k) n (map : (k, _, _) Map.t) : k option =
    Option.map ~f:fst (key_range_linear ~from:n ~to_:n map)
  ;;

  (** Find key [by] positions earlier/later in a map. Returns none if out of bounds. *)
  let rec offset (key : 'k) (map : ('k, _, _) Map.t) ~by : 'k option =
    if Int.( = ) by 0
    then Some key
    else (
      let closest_dir, add =
        if Int.( < ) by 0 then `Less_than, 1 else `Greater_than, -1
      in
      match Map.closest_key map closest_dir key with
      | None -> None
      | Some (key, _) -> offset key map ~by:(by + add))
  ;;

  (** Find how we need to move [key] if [changed_key] changed in the given
      way *)
  let find_offset ~compare ~key ~changed_key change =
    if Int.( < ) (compare changed_key key) 0
    then (
      match change with
      | `Left _ -> 1
      | `Right _ -> -1
      | _ -> 0)
    else 0
  ;;

  (** Range map by indices *)
  let subrange_by_rank (type k) ?data_equal (map : (k, _, _) Map.t Incr.t) range =
    let key_range : (k * k option) option Incr.t =
      with_old2 map range ~f:(fun ~old map (from, to_) ->
        (* This function returns no keys, only begin key, or begin and end keys.
           These are the keys at [from] and [to_] positions in the map, or None if the
           indices are too big. As always [0 <= from && from <= to_], there is no
           possibility of only [to_] being a valid position.
        *)
        if Int.( < ) to_ from || Int.( < ) from 0
        then raise_s [%message "Invalid indices" (from : int) (to_ : int)];
        match old with
        | Some (old_map, (old_from, old_to), Some (begin_key, end_key_opt)) ->
          let find_offset = find_offset ~compare:(Map.comparator map).compare in
          let range_offset_begin = from - old_from in
          let range_offset_end = to_ - old_to in
          let adjust_and_offset ~by key =
            let by = by + if by >= 0 && not (Map.mem map key) then 1 else 0 in
            offset key map ~by
          in
          (* We only care about the keys changing and not the data, so [data_equal] here
             can be always true *)
          let diff ~init ~f =
            Map.fold_symmetric_diff ~data_equal:(fun _ _ -> true) old_map map ~init ~f
          in
          let begin_key_opt, end_key_opt =
            match end_key_opt with
            | Some end_key ->
              let map_offset_begin, map_offset_end =
                diff ~init:(0, 0) ~f:(fun (offset_begin, offset_end) (key, change) ->
                  ( offset_begin + find_offset ~key:begin_key ~changed_key:key change
                  , offset_end + find_offset ~key:end_key ~changed_key:key change ))
              in
              ( adjust_and_offset begin_key ~by:(map_offset_begin + range_offset_begin)
              , adjust_and_offset end_key ~by:(map_offset_end + range_offset_end) )
            | None ->
              let map_offset_begin =
                diff ~init:0 ~f:(fun offset_begin (key, change) ->
                  offset_begin + find_offset ~key:begin_key ~changed_key:key change)
              in
              ( adjust_and_offset begin_key ~by:(map_offset_begin + range_offset_begin)
              , nth_from_either_side to_ map )
          in
          assert (Option.for_all ~f:(Map.mem map) begin_key_opt);
          assert (Option.for_all ~f:(Map.mem map) end_key_opt);
          Option.map begin_key_opt ~f:(fun begin_key -> begin_key, end_key_opt)
        | None | Some (_, _, None) ->
          (* On first run (when we have to) or when both the keys are none, run O(n)
             scan. This is fine for keys-are-none case as it happens when the positions
             are past end of the map, so they shouldn't be too far from end after the
             map changes, and [key_range_linear] is fast in such case. *)
          key_range_linear map ~from ~to_)
    in
    let subrange_key_range =
      let open Incr.Let_syntax in
      let%map key_range = key_range
      and map = map in
      match key_range with
      | Some (begin_key, Some end_key) -> Some (Incl begin_key, Incl end_key)
      | Some (begin_key, None) ->
        let last_key = Map.max_elt map |> Option.value_exn |> fst in
        Some (Incl begin_key, Incl last_key)
      | None -> None
    in
    subrange ?data_equal map subrange_key_range
  ;;

  module For_testing = struct
    let key_range_linear = key_range_linear
  end

  module Lookup = struct
    type 'v entry =
      { mutable saved_value : 'v option
      ; node : 'v option Incr.Expert.Node.t
      }

    type ('k, 'v, 'cmp) t =
      { mutable saved_map : ('k, 'v, 'cmp) Map.t
      (* We may have multiple entries per key if nodes become necessary again after being
         removed. *)
      ; mutable lookup_entries : ('k, 'v entry list, 'cmp) Map.t
      ; updater_node : unit Incr.t
      ; scope : Incr.Scope.t
      }

    module M (K : sig
        type t
        type comparator_witness
      end) =
    struct
      type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
    end

    let create ?(data_equal = phys_equal) input_map ~comparator =
      let rec self =
        lazy
          (let updater_node =
             Incr.map input_map ~f:(fun input_map ->
               let (lazy self) = self in
               Map.fold_symmetric_diff
                 self.saved_map
                 input_map
                 ~data_equal
                 ~init:()
                 ~f:(fun () (key, changed_value) ->
                   let entries = Map.find_multi self.lookup_entries key in
                   List.iter entries ~f:(fun entry ->
                     entry.saved_value
                     <- (match changed_value with
                       | `Left _ -> None
                       | `Right new_value | `Unequal (_, new_value) ->
                         Some new_value);
                     Incr.Expert.Node.make_stale entry.node));
               self.saved_map <- input_map)
           in
           let empty_map = Map.Using_comparator.empty ~comparator in
           { saved_map = empty_map
           ; lookup_entries = empty_map
           ; updater_node
           ; scope = Incr.Scope.current ()
           })
      in
      Lazy.force self
    ;;

    let slow_path_link_entry t entry ~key ~is_now_observable =
      let (lazy entry) = entry in
      let current_entries = Map.find_multi t.lookup_entries key in
      let is_linked = List.exists current_entries ~f:(phys_equal entry) in
      if Bool.equal is_linked is_now_observable
      then ()
      else if is_now_observable
      then t.lookup_entries <- Map.add_multi t.lookup_entries ~key ~data:entry
      else (
        let new_entries =
          List.filter current_entries ~f:(fun x -> not (phys_equal entry x))
        in
        t.lookup_entries
        <- (if List.is_empty new_entries
            then Map.remove t.lookup_entries key
            else Map.set t.lookup_entries ~key ~data:new_entries))
    ;;

    let slow_path_create_node t key =
      Incr.Scope.within t.scope ~f:(fun () ->
        let rec entry =
          lazy
            { saved_value = Map.find t.saved_map key
            ; node =
                Incr.Expert.Node.create
                  (fun () -> (force entry).saved_value)
                  ~on_observability_change:(slow_path_link_entry t entry ~key)
            }
        in
        let (lazy entry) = entry in
        Incr.Expert.Node.add_dependency
          entry.node
          (Incr.Expert.Dependency.create t.updater_node);
        Incr.Expert.Node.watch entry.node)
    ;;

    let find t key =
      match Map.find_multi t.lookup_entries key with
      | entry :: _ -> Incr.Expert.Node.watch entry.node
      | [] -> slow_path_create_node t key
    ;;

    module For_debug = struct
      let sexp_of_entry sexp_of_value entry =
        let { saved_value; node } = entry in
        let node = Incr.Expert.Node.watch node in
        [%sexp
          { saved_value : value option
          ; node_info = (Incr.user_info node : (Info.t option[@sexp.option]))
          ; node_is_const =
              (Option.some_if (Incr.is_const node) () : (unit option[@sexp.option]))
          ; node_is_invalid =
              (Option.some_if (not (Incr.is_valid node)) () : (unit option[@sexp.option]))
          ; node_is_unnecessary =
              (Option.some_if (not (Incr.is_necessary node)) () : (unit option[@sexp.option
                                                                   ]))
          }]
      ;;

      let sexp_of_t sexp_of_key sexp_of_value t =
        let info_per_key =
          Map.merge t.saved_map t.lookup_entries ~f:(fun ~key data ->
            let actual_value, entries =
              match data with
              | `Left x -> Some x, []
              | `Right y -> None, y
              | `Both (x, y) -> Some x, y
            in
            Some
              [%sexp
                { key : key
                ; actual_value : (value option[@sexp.option])
                ; entries : value entry list
                }])
        in
        Sexp.List (Map.data info_per_key)
      ;;
    end
  end
end
