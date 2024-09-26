open! Core
include Incr_map_intf

let no_instrumentation = { Instrumentation.f = (fun f -> f ()) }

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

module Generic = struct
  let with_old ~instrumentation i ~f =
    let open Incremental.Let_syntax in
    let old = ref None in
    let%map a = i in
    instrumentation.Instrumentation.f (fun () ->
      let b = f ~old:!old a in
      old := Some (a, b);
      b)
  ;;

  let cutoff ?(instrumentation = no_instrumentation) map ~cutoff =
    let data_equal old_value new_value =
      Incremental.Cutoff.should_cutoff cutoff ~old_value ~new_value
    in
    with_old ~instrumentation map ~f:(fun ~old cur ->
      match old with
      | None -> cur
      | Some (_old_in, old) ->
        Map.fold_symmetric_diff ~data_equal ~init:old old cur ~f:(fun acc (key, change) ->
          match change with
          | `Left _old -> Map.remove acc key
          | `Right new_ -> Map.add_exn acc ~key ~data:new_
          | `Unequal (_old, new_value) -> Map.set acc ~key ~data:new_value))
  ;;

  let unordered_fold_with_comparator
    ~instrumentation
    ?(data_equal = phys_equal)
    ?update
    ?specialized_initial
    ?(finalize = Fn.id)
    ?(revert_to_init_when_empty = false)
    map
    ~init
    ~add
    ~remove
    =
    let update =
      let default ~cmp ~key ~old_data ~new_data acc =
        add ~cmp ~key ~data:new_data (remove ~cmp ~key ~data:old_data acc)
      in
      Option.value update ~default
    in
    with_old ~instrumentation map ~f:(fun ~old new_in ->
      let cmp = Map.comparator new_in in
      let acc =
        match old with
        | None ->
          (match specialized_initial with
           | None -> Map.fold ~init ~f:(add ~cmp) new_in
           | Some initial -> initial ~cmp ~init new_in)
        | Some (old_in, old_out) ->
          if revert_to_init_when_empty && Map.length new_in = 0
          then init
          else
            Map.fold_symmetric_diff
              ~init:old_out
              old_in
              new_in
              ~data_equal
              ~f:(fun acc (key, change) ->
                match change with
                | `Left old -> remove ~cmp ~key ~data:old acc
                | `Right new_ -> add ~cmp ~key ~data:new_ acc
                | `Unequal (old, new_) ->
                  update ~cmp ~key ~old_data:old ~new_data:new_ acc)
      in
      finalize acc)
  ;;

  let unordered_fold
    ~instrumentation
    ?data_equal
    ?update
    ?specialized_initial
    ?finalize
    ?revert_to_init_when_empty
    map
    ~init
    ~add
    ~remove
    =
    let add ~cmp:_ = add in
    let remove ~cmp:_ = remove in
    let update = Option.map update ~f:(fun update ~cmp:_ -> update) in
    let specialized_initial =
      Option.map specialized_initial ~f:(fun specialized_initial ~cmp:_ ->
        specialized_initial)
    in
    unordered_fold_with_comparator
      ~instrumentation
      ?data_equal
      ?update
      ?specialized_initial
      ?finalize
      ?revert_to_init_when_empty
      map
      ~init
      ~add
      ~remove
  ;;

  let unordered_fold_nested_maps_with_comparators
    ~instrumentation
    ?(data_equal = phys_equal)
    ?revert_to_init_when_empty
    ?update
    incr_map
    ~init
    ~add
    ~remove
    =
    let update =
      match update with
      | Some update -> update
      | None ->
        fun ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~old_data ~new_data acc ->
          add
            ~outer_cmp
            ~inner_cmp
            ~outer_key
            ~inner_key
            ~data:new_data
            (remove ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data:old_data acc)
    in
    unordered_fold_with_comparator
      incr_map
      ~instrumentation
      ?revert_to_init_when_empty
      ~init
      ~update:
        (fun
          ~cmp:outer_cmp
          ~key:outer_key
          ~old_data:old_inner_map
          ~new_data:new_inner_map
          acc
        ->
        let inner_cmp = Map.comparator new_inner_map in
        (Map.fold_symmetric_diff old_inner_map new_inner_map ~data_equal)
          ~init:acc
          ~f:(fun acc (inner_key, diff) ->
            match diff with
            | `Left data_removed ->
              remove ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data:data_removed acc
            | `Right data_added ->
              add ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data:data_added acc
            | `Unequal (old_data, new_data) ->
              update ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~old_data ~new_data acc) [@nontail
                                                                                          ])
      ~add:(fun ~cmp:outer_cmp ~key:outer_key ~data:inner_map acc ->
        let inner_cmp = Map.comparator inner_map in
        Map.fold inner_map ~init:acc ~f:(fun ~key:inner_key ~data acc ->
          add ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data acc))
      ~remove:(fun ~cmp:outer_cmp ~key:outer_key ~data:inner_map acc ->
        let inner_cmp = Map.comparator inner_map in
        Map.fold inner_map ~init:acc ~f:(fun ~key:inner_key ~data acc ->
          remove ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data acc))
  ;;

  let unordered_fold_nested_maps
    ~instrumentation
    ?data_equal
    ?revert_to_init_when_empty
    ?update
    incr_map
    ~init
    ~add
    ~remove
    =
    let add ~outer_cmp:_ ~inner_cmp:_ = add in
    let remove ~outer_cmp:_ ~inner_cmp:_ = remove in
    let update = Option.map update ~f:(fun update ~outer_cmp:_ ~inner_cmp:_ -> update) in
    unordered_fold_nested_maps_with_comparators
      ~instrumentation
      ?data_equal
      ?revert_to_init_when_empty
      ?update
      incr_map
      ~init
      ~add
      ~remove
  ;;

  let with_comparator' ~comparator get_comparator x f =
    match comparator with
    | None ->
      Incremental.bind (Incremental.freeze (Incremental.map x ~f:get_comparator)) ~f
    | Some comparator -> f comparator
  ;;

  (** Captures the comparator (which can't change anyway, since the type determines the
      comparator) by freezing the corresponding map.  Note that by first using Incremental.map to
      get the comparator out of the map, we allow the initial map itself to be garbage
      collected *)
  let with_comparator ~comparator map f =
    with_comparator' ~comparator Map.comparator_s map f
  ;;

  let of_set ?(instrumentation = no_instrumentation) set =
    with_old ~instrumentation set ~f:(fun ~old new_input ->
      match old with
      | None -> Map.of_key_set new_input ~f:(fun _key -> ())
      | Some (old_input, old_output) ->
        Sequence.fold
          (Set.symmetric_diff old_input new_input)
          ~init:old_output
          ~f:(fun output -> function
          | First k -> Map.remove output k
          | Second k -> Map.add_exn output ~key:k ~data:()))
  ;;

  let generic_mapi
    (type input_data output_data f_output state_witness)
    (witness : (input_data, output_data, f_output) Map_type.t)
    ~instrumentation
    ?(data_equal = phys_equal)
    (map : (('key, input_data, 'cmp) Map.t, state_witness) Incremental.t)
    ~(f : key:'key -> data:input_data -> f_output)
    =
    with_old ~instrumentation map ~f:(fun ~old input ->
      match old, Map.length input with
      | _, 0 | None, _ ->
        (match witness with
         | Map_type.Map -> (Map.mapi input ~f : ('key, output_data, 'cmp) Map.t)
         | Map_type.Filter_map -> Map.filter_mapi input ~f)
      | Some (old_input, old_output), _ ->
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

  let mapi ?(instrumentation = no_instrumentation) ?data_equal map ~f =
    generic_mapi Map ~instrumentation ?data_equal map ~f
  ;;

  let filter_mapi ?(instrumentation = no_instrumentation) ?data_equal map ~f =
    generic_mapi Filter_map ~instrumentation ?data_equal map ~f
  ;;

  let map ?instrumentation ?data_equal map ~f =
    mapi ?instrumentation ?data_equal map ~f:(fun ~key:_ ~data -> f data)
  ;;

  let filter_map ?instrumentation ?data_equal map ~f =
    filter_mapi ?instrumentation ?data_equal map ~f:(fun ~key:_ ~data -> f data)
  ;;

  let with_old2 ~instrumentation i1 i2 ~f =
    let old = ref None in
    Incremental.map2 i1 i2 ~f:(fun a1 a2 ->
      instrumentation.Instrumentation.f (fun () ->
        let b = f ~old:!old a1 a2 in
        old := Some (a1, a2, b);
        b))
  ;;

  let unordered_fold_with_extra
    ?(instrumentation = no_instrumentation)
    ?(data_equal = phys_equal)
    ?(extra_equal = phys_equal)
    ?update
    ?specialized_initial
    ?(finalize = Fn.id)
    ?(revert_to_init_when_empty = false)
    map
    extra
    ~init
    ~add
    ~remove
    ~extra_changed
    =
    let update =
      let default ~key ~old_data ~new_data acc extra =
        add ~key ~data:new_data (remove ~key ~data:old_data acc extra) extra
      in
      Option.value update ~default
    in
    with_old2 ~instrumentation map extra ~f:(fun ~old new_in new_extra ->
      let acc =
        match old with
        | None ->
          (match specialized_initial with
           | None ->
             Map.fold new_in ~init ~f:(fun ~key ~data acc -> add ~key ~data acc new_extra)
           | Some initial -> initial ~init new_in new_extra)
        | Some (old_in, old_extra, old_out) ->
          let acc =
            if extra_equal old_extra new_extra
            then old_out
            else extra_changed ~old_extra ~new_extra ~input:old_in old_out
          in
          if revert_to_init_when_empty && Map.length new_in = 0
          then init
          else
            Map.fold_symmetric_diff
              ~init:acc
              old_in
              new_in
              ~data_equal
              ~f:(fun acc (key, change) ->
                match change with
                | `Left old -> remove ~key ~data:old acc new_extra
                | `Right new_ -> add ~key ~data:new_ acc new_extra
                | `Unequal (old, new_) ->
                  update ~key ~old_data:old ~new_data:new_ acc new_extra)
      in
      finalize acc)
  ;;

  let mapi_count
    (type a cmp)
    ?(instrumentation = no_instrumentation)
    ?(data_equal = phys_equal)
    input
    ~(comparator : (module Comparator.S with type t = a and type comparator_witness = cmp))
    ~f
    =
    let module M = (val comparator) in
    let add new_key acc =
      Map.update acc new_key ~f:(function
        | None -> 1
        | Some n -> n + 1)
    in
    let remove new_key acc =
      Map.change acc new_key ~f:(function
        | None -> None
        | Some 1 -> None
        | Some n -> Some (n - 1))
    in
    unordered_fold
      ~instrumentation
      ~data_equal
      input
      ~init:(Map.empty (module M))
      ~add:(fun ~key ~data acc -> add (f ~key ~data) acc)
      ~remove:(fun ~key ~data acc -> remove (f ~key ~data) acc)
      ~update:(fun ~key ~old_data ~new_data acc ->
        let prev_key = f ~key ~data:old_data in
        let new_key = f ~key ~data:new_data in
        if M.comparator.compare prev_key new_key = 0
        then acc
        else acc |> remove prev_key |> add new_key)
  ;;

  let map_count ?instrumentation ?data_equal input ~comparator ~f =
    mapi_count ?instrumentation ?data_equal input ~comparator ~f:(fun ~key:_ ~data ->
      f data)
  ;;

  let min_helper map =
    match Map.min_elt map with
    | None -> None
    | Some (min, _) -> Some min
  ;;

  let max_helper map =
    match Map.max_elt map with
    | None -> None
    | Some (max, _) -> Some max
  ;;

  let bounds_helper map =
    match Map.min_elt map, Map.max_elt map with
    | None, None -> None
    | Some (min, _), Some (max, _) -> Some (min, max)
    | _ -> assert false
  ;;

  let mapi_min ?instrumentation ?data_equal input ~comparator ~f =
    Incremental.map
      ~f:min_helper
      (mapi_count ?instrumentation ?data_equal input ~comparator ~f)
  ;;

  let mapi_max ?instrumentation ?data_equal input ~comparator ~f =
    Incremental.map
      ~f:max_helper
      (mapi_count ?instrumentation ?data_equal input ~comparator ~f)
  ;;

  let mapi_bounds ?instrumentation ?data_equal input ~comparator ~f =
    Incremental.map
      ~f:bounds_helper
      (mapi_count ?instrumentation ?data_equal input ~comparator ~f)
  ;;

  let map_min ?instrumentation ?data_equal input ~comparator ~f =
    mapi_min ?instrumentation ?data_equal input ~comparator ~f:(fun ~key:_ ~data ->
      f data)
  ;;

  let map_max ?instrumentation ?data_equal input ~comparator ~f =
    mapi_max ?instrumentation ?data_equal input ~comparator ~f:(fun ~key:_ ~data ->
      f data)
  ;;

  let min_value ?instrumentation ?data_equal input ~comparator =
    map_min ?instrumentation ?data_equal input ~comparator ~f:Fn.id
  ;;

  let max_value ?instrumentation ?data_equal input ~comparator =
    map_max ?instrumentation ?data_equal input ~comparator ~f:Fn.id
  ;;

  let map_bounds ?instrumentation ?data_equal input ~comparator ~f =
    mapi_bounds ?instrumentation ?data_equal input ~comparator ~f:(fun ~key:_ ~data ->
      f data)
  ;;

  let value_bounds ?instrumentation ?data_equal input ~comparator =
    map_bounds ?instrumentation ?data_equal input ~comparator ~f:Fn.id
  ;;

  let merge_shared_impl
    ~old
    ~new_left_map
    ~new_right_map
    ~data_equal_left
    ~data_equal_right
    ~f
    =
    let comparator = Map.comparator new_left_map in
    let old_left_map, old_right_map, old_output =
      match old with
      | None ->
        let empty = Map.Using_comparator.empty ~comparator in
        empty, empty, empty
      | Some x -> x
    in
    let apply_right output (key, diff_element) =
      f ~old_output ~key ~output ~diff_element:(`Right diff_element)
    in
    if phys_equal old_left_map new_left_map
    then
      Map.fold_symmetric_diff
        ~init:old_output
        old_right_map
        new_right_map
        ~data_equal:data_equal_right
        ~f:apply_right [@nontail]
    else (
      let right_diffs =
        Map.symmetric_diff old_right_map new_right_map ~data_equal:data_equal_right
      in
      let output, right_diffs =
        Map.fold_symmetric_diff
          ~init:(old_output, Sequence.next right_diffs)
          old_left_map
          new_left_map
          ~data_equal:data_equal_left
          ~f:(fun (output, right_diffs) (left_key, left_diff_element) ->
            let rec loop
              compare
              ~old_output
              ~output
              right_diffs
              left_key
              left_diff_element
              ~f
              =
              let[@inline] apply_left output =
                f
                  ~old_output
                  ~key:left_key
                  ~output
                  ~diff_element:(`Left left_diff_element)
              in
              let[@inline] apply_right output (key, diff_element) =
                f ~old_output ~key ~output ~diff_element:(`Right diff_element)
              in
              match right_diffs with
              | None -> apply_left output, right_diffs
              | Some (((right_key, right_diff_element) as hd), tl) ->
                (match compare left_key right_key with
                 | 0 ->
                   ( f
                       ~old_output
                       ~key:left_key
                       ~output
                       ~diff_element:(`Both (left_diff_element, right_diff_element))
                   , Sequence.next tl )
                 | x when x > 0 ->
                   (loop [@tailcall])
                     compare
                     ~old_output
                     ~output:(apply_right output hd)
                     (Sequence.next tl)
                     left_key
                     left_diff_element
                     ~f
                 | _ -> apply_left output, right_diffs)
            in
            loop
              comparator.compare
              ~old_output
              ~output
              right_diffs
              left_key
              left_diff_element
              ~f [@nontail])
      in
      Option.value_map right_diffs ~default:output ~f:(fun (hd, tl) ->
        Sequence.fold ~init:(apply_right output hd) tl ~f:apply_right) [@nontail])
  ;;

  let new_data_from_diff_element = function
    | `Left _ -> None
    | `Right x | `Unequal (_, x) -> Some x
  ;;

  let merge
    ?(instrumentation = no_instrumentation)
    ?(data_equal_left = phys_equal)
    ?(data_equal_right = phys_equal)
    left_map
    right_map
    ~f
    =
    with_old2
      left_map
      right_map
      ~instrumentation
      ~f:(fun ~old new_left_map new_right_map ->
        merge_shared_impl
          ~old
          ~new_left_map
          ~new_right_map
          ~data_equal_left
          ~data_equal_right
          ~f:(fun ~old_output:_ ~key ~output ~diff_element ->
            (* These values represent whether there is data for the given key in the new
               input in the left and right map. *)
            let left_data_opt, right_data_opt =
              match diff_element with
              | `Both (left_diff, right_diff) ->
                ( new_data_from_diff_element left_diff
                , new_data_from_diff_element right_diff )
              | `Left left_diff ->
                new_data_from_diff_element left_diff, Map.find new_right_map key
              | `Right right_diff ->
                Map.find new_left_map key, new_data_from_diff_element right_diff
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

  let merge_both_some
    ?(instrumentation = no_instrumentation)
    ?(data_equal_left = phys_equal)
    ?(data_equal_right = phys_equal)
    ?(out_equal = phys_equal)
    left_map
    right_map
    ~f
    =
    with_old2
      left_map
      right_map
      ~instrumentation
      ~f:(fun ~old new_left_map new_right_map ->
        let comparator = Map.comparator new_left_map in
        let empty = Map.Using_comparator.empty ~comparator in
        match Map.length new_left_map, Map.length new_right_map with
        (* Because we only care about keys that are in both maps, if either map is
           empty, bail early. *)
        | 0, _ | _, 0 -> empty
        | _ ->
          merge_shared_impl
            ~old
            ~new_left_map
            ~new_right_map
            ~data_equal_left
            ~data_equal_right
            ~f:(fun ~old_output ~key ~output ~diff_element ->
              let left_and_right_data_opt =
                let open Option.Let_syntax in
                match diff_element with
                | `Both (left_diff, right_diff) ->
                  let%bind left_data = new_data_from_diff_element left_diff in
                  let%map right_data = new_data_from_diff_element right_diff in
                  left_data, right_data
                | `Left left_diff ->
                  let%bind left_data = new_data_from_diff_element left_diff in
                  let%map right_data = Map.find new_right_map key in
                  left_data, right_data
                | `Right right_diff ->
                  (* This match arm binds [right_data] first because the map lookup
                     is slower than calling [new_data_from_diff_element]. *)
                  let%bind right_data = new_data_from_diff_element right_diff in
                  let%map left_data = Map.find new_left_map key in
                  left_data, right_data
              in
              (* look for the previously computed value to see if we actually need to
                 add or remove the key. *)
              let prev_out = Map.find old_output key in
              match left_and_right_data_opt with
              | Some (x, y) ->
                let data = f ~key x y in
                (match prev_out with
                 (* if the new result is the same as the old, don't change the map *)
                 | Some prev_out when out_equal data prev_out -> output
                 | None | Some _ -> Map.set output ~key ~data)
              | None ->
                (match prev_out with
                 | None -> output
                 | Some _ -> Map.remove output key)))
  ;;

  let merge_disjoint
    ?(instrumentation = no_instrumentation)
    ?(data_equal = phys_equal)
    left_map
    right_map
    =
    with_old2
      left_map
      right_map
      ~instrumentation
      ~f:(fun ~old new_left_map new_right_map ->
        let comparator = Map.comparator new_left_map in
        let empty = Map.Using_comparator.empty ~comparator in
        let merge_from_scratch () =
          Map.merge_skewed new_left_map new_right_map ~combine:(fun ~key _ _ ->
            raise_s
              [%message
                [%here]
                  "Incr_map.merge_disjoint"
                  "caller has broken invariant, a key is present in both maps"
                  ~key:(comparator.sexp_of_t key : Sexp.t)])
        in
        match Map.length new_left_map, Map.length new_right_map with
        | 0, 0 -> empty
        | 0, _ ->
          (* if the left map is empty, the "merged" result is the right map *)
          new_right_map
        | _, 0 ->
          (* if the right map is empty, the "merged" result is the left map*)
          new_left_map
        | l, r when l < Int.floor_log2 r || r < Int.floor_log2 l ->
          (* If one of the maps is small enough in comparison to the other,
             [Map.merge_skewed] is likely cheaper than computing the symmetric diff of both maps *)
          merge_from_scratch ()
        | _, _ ->
          (match old with
           | None -> merge_from_scratch ()
           | Some (old_left_map, old_right_map, old_result) ->
             let with_left_changes =
               Map.fold_symmetric_diff
                 old_left_map
                 new_left_map
                 ~data_equal
                 ~init:old_result
                 ~f:(fun acc (key, elt) ->
                   match elt with
                   | `Right data -> Map.set acc ~key ~data
                   | `Left _ -> Map.remove acc key
                   | `Unequal (_prev, cur) -> Map.set acc ~key ~data:cur)
             in
             Map.fold_symmetric_diff
               old_right_map
               new_right_map
               ~data_equal
               ~init:with_left_changes
               ~f:(fun acc (key, elt) ->
                 match elt with
                 | `Right data -> Map.set acc ~key ~data
                 | `Left _ ->
                   (* the key may have been moved into the the left map, so check before removing *)
                   if Map.mem new_left_map key then acc else Map.remove acc key
                 | `Unequal (_prev, cur) -> Map.set acc ~key ~data:cur)))
  ;;

  let generic_mapi_with_comparator'
    (type input_data output_data f_output state_witness)
    (witness : (input_data, output_data, f_output) Map_type.t)
    ~instrumentation
    ?cutoff
    ?(data_equal = phys_equal)
    (lhs : (('key, input_data, 'cmp) Map.t, state_witness) Incremental.t)
    ~(comparator : ('key, 'cmp) Comparator.Module.t)
    ~(f :
        key:'key
        -> data:(input_data, state_witness) Incremental.t
        -> (f_output, state_witness) Incremental.t)
    : (('key, output_data, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state lhs in
    let empty_map = Map.empty comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let acc : ('key, output_data, 'cmp) Map.t ref = ref empty_map in
    let result = E.Node.create incremental_state (fun () -> !acc) in
    let (on_inner_change : key:'key -> f_output -> unit) =
      match witness with
      | Map_type.Map -> fun ~key data -> acc := Map.set !acc ~key ~data
      | Map_type.Filter_map ->
        fun ~key opt ->
          let old = !acc in
          acc
          := (match opt with
               | None -> Map.remove old key
               | Some data -> Map.set old ~key ~data)
    in
    let rec lhs_change =
      lazy
        (Incremental.map lhs ~f:(fun map ->
           instrumentation.Instrumentation.f (fun () ->
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
                     let node =
                       E.Node.create incremental_state (fun () ->
                         Map.find_exn !prev_map key)
                     in
                     Option.iter cutoff ~f:(fun c ->
                       Incremental.set_cutoff (E.Node.watch node) c);
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
             prev_map := map)))
    in
    E.Node.add_dependency result (E.Dependency.create (force lhs_change));
    E.Node.watch result
  ;;

  let filter_mapi'
    ?(instrumentation = no_instrumentation)
    ?comparator
    ?cutoff
    ?data_equal
    map
    ~f
    =
    with_comparator ~comparator map (fun comparator ->
      generic_mapi_with_comparator'
        Map_type.Filter_map
        ~instrumentation
        ?cutoff
        ?data_equal
        map
        ~f
        ~comparator)
  ;;

  let mapi' ?(instrumentation = no_instrumentation) ?comparator ?cutoff ?data_equal map ~f
    =
    with_comparator ~comparator map (fun comparator ->
      generic_mapi_with_comparator'
        Map_type.Map
        ~instrumentation
        ?cutoff
        ?data_equal
        map
        ~f
        ~comparator)
  ;;

  let map' ?instrumentation ?comparator ?cutoff ?data_equal map ~f =
    mapi' ?instrumentation ?comparator ?cutoff ?data_equal map ~f:(fun ~key:_ ~data ->
      f data)
  ;;

  let filter_map' ?instrumentation ?comparator ?cutoff ?data_equal map ~f =
    filter_mapi'
      ?instrumentation
      ?comparator
      ?cutoff
      ?data_equal
      map
      ~f:(fun ~key:_ ~data -> f data)
  ;;

  let merge'
    ?instrumentation
    ?comparator
    ?cutoff
    ?data_equal_left
    ?data_equal_right
    map1
    map2
    ~f
    =
    merge
      ?instrumentation
      ?data_equal_left
      ?data_equal_right
      map1
      map2
      ~f:(fun ~key:_ diff -> Some diff)
    |> filter_mapi' ?instrumentation ?comparator ?cutoff ~f:(fun ~key ~data:diff ->
      f ~key diff)
  ;;

  let unzip_mapi_with_comparator
    (type v v1 v2 state_witness)
    ~instrumentation
    ?(data_equal : v -> v -> bool = phys_equal)
    ?(left_result_equal : v1 -> v1 -> bool = phys_equal)
    ?(right_result_equal : v2 -> v2 -> bool = phys_equal)
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
    ~(comparator : ('key, 'cmp) Comparator.Module.t)
    ~(f : key:'key -> data:v -> v1 * v2)
    : (('key, v1, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v2, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state input in
    let empty_map = Map.empty comparator in
    let left_acc : ('key, v1, 'cmp) Map.t ref = ref empty_map in
    let left_result = E.Node.create incremental_state (fun () -> !left_acc) in
    let right_acc : ('key, v2, 'cmp) Map.t ref = ref empty_map in
    let right_result = E.Node.create incremental_state (fun () -> !right_acc) in
    let prev_map = ref empty_map in
    let input_change =
      Incremental.map input ~f:(fun map ->
        instrumentation.Instrumentation.f (fun () ->
          let left, right =
            match Map.is_empty !prev_map, Map.is_empty map with
            | true, true | false, true -> empty_map, empty_map
            | true, false ->
              (* Mapping on a map is way faster than symmetric diffing and then
                 building the maps up piece by piece, so we do this whenever we
                 transition from "empty" to "something", which will almost always
                 happen on the first stabilization. *)
              let left =
                Map.mapi map ~f:(fun ~key ~data ->
                  let l, _ = f ~key ~data in
                  l)
              in
              let right =
                Map.mapi map ~f:(fun ~key ~data ->
                  let _, r = f ~key ~data in
                  r)
              in
              left, right
            | false, false ->
              Map.fold_symmetric_diff
                ~data_equal
                !prev_map
                map
                ~init:(!left_acc, !right_acc)
                ~f:(fun (left, right) (key, changed) ->
                  match changed with
                  | `Unequal (prev, new_) ->
                    let prev_a, prev_b = f ~key ~data:prev in
                    let new_a, new_b = f ~key ~data:new_ in
                    let left =
                      if left_result_equal prev_a new_a
                      then left
                      else Map.set left ~key ~data:new_a
                    in
                    let right =
                      if right_result_equal prev_b new_b
                      then right
                      else Map.set right ~key ~data:new_b
                    in
                    left, right
                  | `Left _ -> Map.remove left key, Map.remove right key
                  | `Right element ->
                    let a, b = f ~key ~data:element in
                    Map.set left ~key ~data:a, Map.set right ~key ~data:b)
          in
          if not (phys_equal !left_acc left) then E.Node.make_stale left_result;
          if not (phys_equal !right_acc right) then E.Node.make_stale right_result;
          left_acc := left;
          right_acc := right;
          prev_map := map))
    in
    E.Node.add_dependency left_result (E.Dependency.create input_change);
    E.Node.add_dependency right_result (E.Dependency.create input_change);
    E.Node.watch left_result, E.Node.watch right_result
  ;;

  let unzip_mapi
    ?(instrumentation = no_instrumentation)
    ?comparator
    ?data_equal
    ?left_result_equal
    ?right_result_equal
    input
    ~f
    =
    let pair =
      with_comparator ~comparator input (fun comparator ->
        input
        |> unzip_mapi_with_comparator
             ~instrumentation
             ?data_equal
             ?left_result_equal
             ?right_result_equal
             ~comparator
             ~f
        |> Tuple2.uncurry Incremental.both)
    in
    Incremental.map ~f:fst pair, Incremental.map ~f:snd pair
  ;;

  let unzip ?instrumentation ?comparator ?left_result_equal ?right_result_equal input =
    let data_equal =
      Option.map2 left_result_equal right_result_equal ~f:(fun l r ->
        Tuple2.equal ~eq1:l ~eq2:r)
    in
    unzip_mapi
      ?instrumentation
      ?comparator
      ?data_equal
      ?left_result_equal
      ?right_result_equal
      input
      ~f:(fun ~key:_ ~data -> data)
  ;;

  let unzip_mapi_with_comparator'
    (type v v1 v2 state_witness)
    ~instrumentation
    ?cutoff
    ?(data_equal = phys_equal)
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
    ~(comparator : ('key, 'cmp) Comparator.Module.t)
    ~(f :
        key:'key
        -> data:(v, state_witness) Incremental.t
        -> (v1, state_witness) Incremental.t * (v2, state_witness) Incremental.t)
    : (('key, v1, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v2, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state input in
    let empty_map = Map.empty comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let left_acc : ('key, v1, 'cmp) Map.t ref = ref empty_map in
    let left_result = E.Node.create incremental_state (fun () -> !left_acc) in
    let right_acc : ('key, v2, 'cmp) Map.t ref = ref empty_map in
    let right_result = E.Node.create incremental_state (fun () -> !right_acc) in
    let left_on_inner_change ~key data = left_acc := Map.set !left_acc ~key ~data in
    let right_on_inner_change ~key data = right_acc := Map.set !right_acc ~key ~data in
    let rec input_change =
      lazy
        (Incremental.map input ~f:(fun map ->
           instrumentation.Instrumentation.f (fun () ->
             let new_nodes =
               Map.fold_symmetric_diff
                 ~data_equal
                 !prev_map
                 map
                 ~init:!prev_nodes
                 ~f:(fun nodes (key, changed) ->
                   match changed with
                   | `Unequal _ ->
                     let node, _left_dep, _right_dep = Map.find_exn nodes key in
                     E.Node.make_stale node;
                     nodes
                   | `Left _ ->
                     let node, left_dep, right_dep = Map.find_exn nodes key in
                     let nodes = Map.remove nodes key in
                     E.Node.remove_dependency left_result left_dep;
                     E.Node.remove_dependency right_result right_dep;
                     left_acc := Map.remove !left_acc key;
                     right_acc := Map.remove !right_acc key;
                     E.Node.invalidate node;
                     nodes
                   | `Right _ ->
                     let node =
                       E.Node.create incremental_state (fun () ->
                         Map.find_exn !prev_map key)
                     in
                     Option.iter cutoff ~f:(fun c ->
                       Incremental.set_cutoff (E.Node.watch node) c);
                     E.Node.add_dependency node (E.Dependency.create (force input_change));
                     let left_incr, right_incr = f ~key ~data:(E.Node.watch node) in
                     let left_user_function_dep =
                       E.Dependency.create
                         left_incr
                         ~on_change:(left_on_inner_change ~key)
                     in
                     let right_user_function_dep =
                       E.Dependency.create
                         right_incr
                         ~on_change:(right_on_inner_change ~key)
                     in
                     E.Node.add_dependency left_result left_user_function_dep;
                     E.Node.add_dependency right_result right_user_function_dep;
                     Map.set
                       nodes
                       ~key
                       ~data:(node, left_user_function_dep, right_user_function_dep))
             in
             prev_nodes := new_nodes;
             prev_map := map)))
    in
    E.Node.add_dependency left_result (E.Dependency.create (force input_change));
    E.Node.add_dependency right_result (E.Dependency.create (force input_change));
    E.Node.watch left_result, E.Node.watch right_result
  ;;

  let unzip_mapi'
    ?(instrumentation = no_instrumentation)
    ?comparator
    ?cutoff
    ?data_equal
    map
    ~f
    =
    let pair =
      with_comparator ~comparator map (fun comparator ->
        map
        |> unzip_mapi_with_comparator' ~instrumentation ?cutoff ?data_equal ~comparator ~f
        |> Incremental.return (Incremental.state map))
    in
    Incremental.bind ~f:fst pair, Incremental.bind ~f:snd pair
  ;;

  let unzip3_mapi_with_comparator'
    (type v v1 v2 v3 state_witness)
    ~instrumentation
    ?cutoff
    ?(data_equal = phys_equal)
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
    ~(comparator : ('key, 'cmp) Comparator.Module.t)
    ~(f :
        key:'key
        -> data:(v, state_witness) Incremental.t
        -> (v1, state_witness) Incremental.t
           * (v2, state_witness) Incremental.t
           * (v3, state_witness) Incremental.t)
    : (('key, v1, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v2, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v3, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state input in
    let empty_map = Map.empty comparator in
    let prev_map = ref empty_map in
    let prev_nodes = ref empty_map in
    let left_acc : ('key, v1, 'cmp) Map.t ref = ref empty_map in
    let left_result = E.Node.create incremental_state (fun () -> !left_acc) in
    let middle_acc : ('key, v2, 'cmp) Map.t ref = ref empty_map in
    let middle_result = E.Node.create incremental_state (fun () -> !middle_acc) in
    let right_acc : ('key, v3, 'cmp) Map.t ref = ref empty_map in
    let right_result = E.Node.create incremental_state (fun () -> !right_acc) in
    let left_on_inner_change ~key data = left_acc := Map.set !left_acc ~key ~data in
    let middle_on_inner_change ~key data = middle_acc := Map.set !middle_acc ~key ~data in
    let right_on_inner_change ~key data = right_acc := Map.set !right_acc ~key ~data in
    let rec input_change =
      lazy
        (Incremental.map input ~f:(fun map ->
           instrumentation.Instrumentation.f (fun () ->
             let new_nodes =
               Map.fold_symmetric_diff
                 ~data_equal
                 !prev_map
                 map
                 ~init:!prev_nodes
                 ~f:(fun nodes (key, changed) ->
                   match changed with
                   | `Unequal _ ->
                     let node, _left_dep, _middle_dep, _right_dep =
                       Map.find_exn nodes key
                     in
                     E.Node.make_stale node;
                     nodes
                   | `Left _ ->
                     let node, left_dep, middle_dep, right_dep = Map.find_exn nodes key in
                     let nodes = Map.remove nodes key in
                     E.Node.remove_dependency left_result left_dep;
                     E.Node.remove_dependency middle_result middle_dep;
                     E.Node.remove_dependency right_result right_dep;
                     left_acc := Map.remove !left_acc key;
                     middle_acc := Map.remove !middle_acc key;
                     right_acc := Map.remove !right_acc key;
                     E.Node.invalidate node;
                     nodes
                   | `Right _ ->
                     let node =
                       E.Node.create incremental_state (fun () ->
                         Map.find_exn !prev_map key)
                     in
                     Option.iter cutoff ~f:(fun c ->
                       Incremental.set_cutoff (E.Node.watch node) c);
                     E.Node.add_dependency node (E.Dependency.create (force input_change));
                     let left_incr, middle_incr, right_incr =
                       f ~key ~data:(E.Node.watch node)
                     in
                     let left_user_function_dep =
                       E.Dependency.create
                         left_incr
                         ~on_change:(left_on_inner_change ~key)
                     in
                     let middle_user_function_dep =
                       E.Dependency.create
                         middle_incr
                         ~on_change:(middle_on_inner_change ~key)
                     in
                     let right_user_function_dep =
                       E.Dependency.create
                         right_incr
                         ~on_change:(right_on_inner_change ~key)
                     in
                     E.Node.add_dependency left_result left_user_function_dep;
                     E.Node.add_dependency middle_result middle_user_function_dep;
                     E.Node.add_dependency right_result right_user_function_dep;
                     Map.set
                       nodes
                       ~key
                       ~data:
                         ( node
                         , left_user_function_dep
                         , middle_user_function_dep
                         , right_user_function_dep ))
             in
             prev_nodes := new_nodes;
             prev_map := map)))
    in
    E.Node.add_dependency left_result (E.Dependency.create (force input_change));
    E.Node.add_dependency middle_result (E.Dependency.create (force input_change));
    E.Node.add_dependency right_result (E.Dependency.create (force input_change));
    E.Node.watch left_result, E.Node.watch middle_result, E.Node.watch right_result
  ;;

  let unzip3_mapi'
    ?(instrumentation = no_instrumentation)
    ?comparator
    ?cutoff
    ?data_equal
    map
    ~f
    =
    let triple =
      with_comparator ~comparator map (fun comparator ->
        map
        |> unzip3_mapi_with_comparator'
             ~instrumentation
             ?cutoff
             ?data_equal
             ~comparator
             ~f
        |> Incremental.return (Incremental.state map))
    in
    ( Incremental.bind ~f:Tuple3.get1 triple
    , Incremental.bind ~f:Tuple3.get2 triple
    , Incremental.bind ~f:Tuple3.get3 triple )
  ;;

  let keys ?(instrumentation = no_instrumentation) ?comparator map =
    with_comparator ~comparator map (fun comparator ->
      let add ~key ~data:_ acc = Set.add acc key in
      let remove ~key ~data:_ acc = Set.remove acc key in
      let data_equal _ _ = true in
      unordered_fold
        map
        ~instrumentation
        ~init:(Set.empty comparator)
        ~revert_to_init_when_empty:true
        ~data_equal
        ~add
        ~remove)
  ;;

  let partition_mapi
    ?(instrumentation = no_instrumentation)
    ?comparator
    ?data_equal
    map
    ~f
    =
    with_comparator ~comparator map (fun comparator ->
      let empty = Map.empty comparator in
      unordered_fold
        ?data_equal
        map
        ~instrumentation
        ~init:(empty, empty)
        ~revert_to_init_when_empty:true
        ~update:(fun ~key ~old_data:_ ~new_data:data (first, second) ->
          match f ~key ~data with
          | First data -> Map.set first ~key ~data, Map.remove second key
          | Second data -> Map.remove first key, Map.set second ~key ~data)
        ~add:(fun ~key ~data (first, second) ->
          match f ~key ~data with
          | First data -> Map.add_exn first ~key ~data, second
          | Second data -> first, Map.add_exn second ~key ~data)
        ~remove:(fun ~key ~data:_ (first, second) ->
          Map.remove first key, Map.remove second key))
  ;;

  let partition_mapi' ?instrumentation ?comparator ?cutoff ?data_equal map ~f =
    with_comparator ~comparator map (fun comparator ->
      mapi' ?instrumentation ~comparator ?cutoff ?data_equal map ~f
      |> partition_mapi ?instrumentation ~comparator ~f:(fun ~key:_ ~data -> data))
  ;;

  let flatten state map =
    let module E = Incremental.Expert in
    let result = ref (Map.Using_comparator.empty ~comparator:(Map.comparator map)) in
    let node = E.Node.create state (fun () -> !result) in
    Map.iteri map ~f:(fun ~key ~data:incr ->
      E.Node.add_dependency
        node
        (E.Dependency.create incr ~on_change:(fun a ->
           result := Map.set !result ~key ~data:a)));
    E.Node.watch node
  ;;

  let join_with_comparator ~instrumentation map_incr ~comparator =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state map_incr in
    let empty_map = Map.empty comparator in
    let result_map = ref empty_map in
    let old_map_of_incrs = ref empty_map in
    let current_dependencies = ref empty_map in
    let result = E.Node.create incremental_state (fun () -> !result_map) in
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
      Incremental.map map_incr ~f:(fun map_of_incrs ->
        instrumentation.Instrumentation.f (fun () ->
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
          old_map_of_incrs := map_of_incrs))
    in
    E.Node.add_dependency result (E.Dependency.create lhs_change);
    E.Node.watch result
  ;;

  let join ?(instrumentation = no_instrumentation) ?comparator map =
    with_comparator ~comparator map (fun comparator ->
      join_with_comparator ~instrumentation map ~comparator)
  ;;

  module Separate_state = struct
    type ('k, 'v, 'cmp, 'w) t =
      { mutable input_map : ('k, 'v, 'cmp) Map.t
      ; mutable expert_nodes : ('k, ('v, 'w) Incremental.Expert.Node.t, 'cmp) Map.t
      ; mutable output_map : ('k, ('v, 'w) Incremental.t, 'cmp) Map.t
      }

    let create comparator =
      let empty = Map.empty comparator in
      { input_map = empty; expert_nodes = empty; output_map = empty }
    ;;

    let create_lookup_node state t key =
      Incremental.Expert.Node.create state (fun () -> Map.find_exn t.input_map key)
    ;;
  end

  let separate ?(instrumentation = no_instrumentation) ?comparator input_map ~data_equal =
    let incremental_state = Incremental.state input_map in
    with_comparator ~comparator input_map (fun comparator ->
      let state = Separate_state.create comparator in
      let output_map_node =
        Incremental.Expert.Node.create incremental_state (fun () -> state.output_map)
      in
      let make_node_depend_on_input_map_changed node ~input_map_changed =
        let dependency =
          Incremental.Expert.Dependency.create (Lazy.force_val input_map_changed)
        in
        Incremental.Expert.Node.add_dependency node dependency
      in
      (* We want to make nodes depend on [input_map_changed] so that [input_map_changed]
         is allowed to make them stale, but we do not want them to be recomputed for any
         other reason. So we make [input_map_changed] a unit incremental (that therefore
         never changes) and this way [output_map_node] and the lookup nodes will only be
         recomputed when they are explicitly made stale.
      *)
      let rec input_map_changed =
        lazy
          (Incremental.map input_map ~f:(fun input_map ->
             instrumentation.Instrumentation.f (fun () ->
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
                       Incremental.Expert.Node.invalidate old_node;
                       Incremental.Expert.Node.make_stale output_map_node;
                       Map.remove expert_nodes key, Map.remove output_map key
                     | `Right _new_value ->
                       let node =
                         Separate_state.create_lookup_node incremental_state state key
                       in
                       make_node_depend_on_input_map_changed node ~input_map_changed;
                       Incremental.Expert.Node.make_stale output_map_node;
                       ( Map.add_exn expert_nodes ~key ~data:node
                       , Map.add_exn
                           output_map
                           ~key
                           ~data:(Incremental.Expert.Node.watch node) )
                     | `Unequal (_old_value, _new_value) ->
                       Incremental.Expert.Node.make_stale (Map.find_exn expert_nodes key);
                       expert_nodes, output_map)
               in
               state.input_map <- input_map;
               state.expert_nodes <- expert_nodes;
               state.output_map <- output_map)))
      in
      make_node_depend_on_input_map_changed output_map_node ~input_map_changed;
      Incremental.Expert.Node.watch output_map_node)
  ;;

  (* Just for deriving structural equality. *)
  type 'a maybe_bound_structurally = 'a Maybe_bound.t =
    | Incl of 'a
    | Excl of 'a
    | Unbounded
  [@@deriving equal]

  let subrange
    (type k v cmp state_witness)
    ?(instrumentation = no_instrumentation)
    ?(data_equal = phys_equal)
    (map_incr : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    range
    =
    with_old2 ~instrumentation map_incr range ~f:(fun ~old map range ->
      let compare = (Map.comparator map).compare in
      let equal l r = compare l r = 0 in
      let ( > ) a b = compare a b > 0
      and ( >= ) a b = compare a b >= 0 in
      let maybe_bound_equal a b : bool = equal_maybe_bound_structurally equal a b in
      let range_is_empty ~min ~max : bool =
        match min, max with
        | Unbounded, (Unbounded | Excl _ | Incl _) | (Excl _ | Incl _), Unbounded -> false
        | Incl min, Incl max -> min > max
        | Excl min, Excl max | Incl min, Excl max | Excl min, Incl max -> min >= max
      in
      let range_includes ~min ~max key : bool =
        Maybe_bound.is_lower_bound min ~of_:key ~compare
        && Maybe_bound.is_upper_bound max ~of_:key ~compare
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
             if Tuple2.equal ~eq1:maybe_bound_equal ~eq2:maybe_bound_equal old_range range
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

  let rekey
    ?(instrumentation = no_instrumentation)
    ?data_equal
    map_incr
    ~comparator:outer_comparator
    ~f
    =
    unordered_fold
      map_incr
      ?data_equal
      ~instrumentation
      ~init:(Map.empty outer_comparator, [])
      ~revert_to_init_when_empty:true
      ~add:(fun ~key ~data (output, adds) ->
        let new_entry = f ~key ~data, data in
        output, new_entry :: adds)
      ~remove:(fun ~key ~data (output, adds) -> Map.remove output (f ~key ~data), adds)
      ~update:(fun ~key ~old_data ~new_data (output, adds) ->
        let prev_key = f ~key ~data:old_data in
        let new_key = f ~key ~data:new_data in
        if (Map.comparator output).compare prev_key new_key = 0
        then Map.set output ~key:new_key ~data:new_data, adds
        else (
          let output = Map.remove output prev_key in
          output, (new_key, new_data) :: adds))
      ~finalize:(fun (output, adds) ->
        let output =
          List.fold adds ~init:output ~f:(fun output (key, data) ->
            Map.add_exn output ~key ~data)
        in
        output, [])
    |> Incremental.map ~f:fst
  ;;

  let index_byi
    ?(instrumentation = no_instrumentation)
    ?inner_comparator
    ?data_equal
    map_incr
    ~comparator:outer_comparator
    ~index
    =
    with_comparator ~comparator:inner_comparator map_incr (fun inner_comparator ->
      unordered_fold
        ?data_equal
        ~instrumentation
        map_incr
        ~init:(Map.empty outer_comparator)
        ~revert_to_init_when_empty:true
        ~add:(fun ~key:inner_key ~data outer_map ->
          match index ~key:inner_key ~data with
          | None -> outer_map
          | Some outer_key ->
            Map.update outer_map outer_key ~f:(function
              | None -> Map.singleton inner_comparator inner_key data
              | Some inner_map -> Map.add_exn inner_map ~key:inner_key ~data))
        ~remove:(fun ~key:inner_key ~data outer_map ->
          match index ~key:inner_key ~data with
          | None -> outer_map
          | Some outer_key ->
            Map.change outer_map outer_key ~f:(function
              | None ->
                failwith "BUG: Hit supposedly impossible case in Incr_map.index_by"
              | Some inner_map ->
                let inner_map = Map.remove inner_map inner_key in
                if Map.is_empty inner_map then None else Some inner_map)))
  ;;

  let index_by ?instrumentation ?inner_comparator ?data_equal map_incr ~comparator ~index =
    index_byi
      ?instrumentation
      ?inner_comparator
      ?data_equal
      map_incr
      ~comparator
      ~index:(fun ~key:_ ~data -> index data)
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

  let find_key_range_linear (type k) ~from ~to_ (map : (k, _, _) Map.t)
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
      else find_keys Map.fold_right ~start_pos:(len - 1) ~advance_pos:(fun pos -> pos - 1)
    in
    Option.map (Key_status.to_option begin_key) ~f:(fun begin_key ->
      begin_key, Key_status.to_option end_key)
  ;;

  let nth_from_either_side (type k) n (map : (k, _, _) Map.t) : k option =
    Option.map ~f:fst (find_key_range_linear ~from:n ~to_:n map)
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

  let rank
    (type k v cmp state_witness)
    ?(instrumentation = no_instrumentation)
    ?comparator
    (map : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    (key : (k, state_witness) Incremental.t)
    =
    with_comparator ~comparator map (fun comparator ->
      let module Cmp = (val comparator) in
      let compare_key = Cmp.comparator.compare in
      let same_key a b = compare_key a b = 0 in
      let when_key_changed ~map ~old_key ~new_key ~old_rank =
        if compare_key new_key old_key < 0
        then (
          (* If the new key is smaller than the old key, find the size of the map subrange
             between them and subtract it from the previous rank *)
          let lower_bound, upper_bound = Excl new_key, Excl old_key in
          let subrange = Map.subrange map ~lower_bound ~upper_bound in
          old_rank - Map.length subrange - 1)
        else (
          (* Otherwise, the new key is larger than the old key, so find the size of the
             map subrange between them and add it to the previous rank *)
          let lower_bound, upper_bound = Excl old_key, Excl new_key in
          let subrange = Map.subrange map ~lower_bound ~upper_bound in
          old_rank + Map.length subrange + 1)
      in
      let when_map_changed ~old_map ~new_map ~key ~old_rank =
        Map.fold_symmetric_diff
        (* We don't care about the data, so optimize these checks *)
          ~data_equal:(fun _ _ -> true)
          old_map
          new_map
          ~init:old_rank
          ~f:(fun acc (diff_key, diff) ->
            match diff with
            | `Left _ when compare_key diff_key key < 0 -> acc - 1
            | `Right _ when compare_key diff_key key < 0 -> acc + 1
            | _ -> acc)
      in
      let rec process ~(old : ((k, v, _) Map.t * _ * _) option) new_map (new_key : k) =
        if not (Map.mem new_map new_key)
        then None
        else (
          match old with
          (* If the map and key are the same, just reuse the old rank *)
          | Some (old_map, old_key, old_rank)
            when phys_equal new_map old_map && same_key old_key new_key -> old_rank
          (* If the map is the same but the key changed *)
          | Some (old_map, old_key, Some old_rank) when phys_equal new_map old_map ->
            Some (when_key_changed ~map:new_map ~old_key ~new_key ~old_rank)
          (* If the key is the same but the map changed *)
          | Some (old_map, old_key, Some old_rank) when same_key new_key old_key ->
            Some (when_map_changed ~old_map ~new_map ~key:new_key ~old_rank)
          (* If both the map and the key changed, this can be simulated as the
             map changing followed by the key changing *)
          | Some (old_map, old_key, Some old_rank) ->
            (* We call [process] recursively instead of directly calling
               [when_map_changed] followed by [when_key_changed] since it might be the
               case that [old_key] is in [old_map] and [new_key] is in [new_map], but
               [old_key] is not in [new_map]. *)
            let old_rank =
              process ~old:(Some (old_map, old_key, Some old_rank)) new_map old_key
            in
            process ~old:(Some (new_map, old_key, old_rank)) new_map new_key
          (* If the previous key was not in the map or this is the first stabilization,
             compute the rank from scratch *)
          | Some (_, _, None) | None -> Map.rank new_map new_key)
      in
      with_old2 ~instrumentation map key ~f:process)
  ;;

  (** Range map by indices *)
  let subrange_by_rank
    (type k state_witness)
    ?(instrumentation = no_instrumentation)
    ?data_equal
    (map : ((k, _, _) Map.t, state_witness) Incremental.t)
    (range : (int Maybe_bound.t * int Maybe_bound.t, state_witness) Incremental.t)
    =
    let find_key_range (range : (int * int, state_witness) Incremental.t)
      : ((k * k option) option, state_witness) Incremental.t
      =
      with_old2 map range ~instrumentation ~f:(fun ~old map (from, to_) ->
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
             map changes, and [find_key_range_linear] is fast in such case. *)
          find_key_range_linear map ~from ~to_)
    in
    (* Handle different Maybe_bound cases and call find_key_range if necessary. It's
       nicer to do this here as opposed to making find_key_range even more complicated *)
    let open Incremental.Let_syntax in
    let ( >>> ) new_ bound = Maybe_bound.map ~f:(fun _ -> new_) bound in
    let return = Incremental.return (Incremental.state map) in
    let key_range =
      match%pattern_bind range with
      | Maybe_bound.Unbounded, Maybe_bound.Unbounded ->
        return (Some (Maybe_bound.Unbounded, Maybe_bound.Unbounded))
      | ( ((Maybe_bound.Incl l | Maybe_bound.Excl l) as lb)
        , ((Maybe_bound.Incl u | Maybe_bound.Excl u) as ub) ) ->
        let%map key_range = find_key_range (Incremental.both l u)
        and lb
        and ub in
        (match key_range with
         | Some (begin_key, Some end_key) -> Some (begin_key >>> lb, end_key >>> ub)
         | Some (begin_key, None) -> Some (begin_key >>> lb, Unbounded)
         | None -> None)
      | ((Maybe_bound.Incl l | Maybe_bound.Excl l) as lb), Maybe_bound.Unbounded ->
        let%map key_range = find_key_range (Incremental.both l l)
        and lb in
        (match key_range with
         | Some (key, _) -> Some (key >>> lb, Unbounded)
         | None -> None)
      | Maybe_bound.Unbounded, ((Maybe_bound.Incl u | Maybe_bound.Excl u) as ub) ->
        let%map key_range = find_key_range (Incremental.both u u)
        and ub in
        (match key_range with
         | Some (key, _) -> Some (Unbounded, key >>> ub)
         (* In this case, the upper bound was larger than the number of elements in the
            map, so the upper bound for the key range is [Unbounded].
            This behavior is demonstrated in a test in [../test/test_subrange.ml]. *)
         | None -> Some (Unbounded, Unbounded))
    in
    subrange ?data_equal map key_range
  ;;

  let transpose
    : type k1 k2 v k1_cmp k2_cmp state_witness.
      ?instrumentation:Instrumentation.t
      -> ?comparator:(k1, k1_cmp) Comparator.Module.t
      -> ?data_equal:(v -> v -> bool)
      -> (k2, k2_cmp) Comparator.Module.t
      -> ((k1, (k2, v, k2_cmp) Map.t, k1_cmp) Map.t, state_witness) Incremental.t
      -> ((k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t, state_witness) Incremental.t
    =
    fun ?(instrumentation = no_instrumentation)
      ?comparator
      ?(data_equal = phys_equal)
      k2_comparator
      m ->
    with_comparator ~comparator m (fun k1_comparator ->
      let update
        :  key:k1 -> old_data:(k2, v, k2_cmp) Map.t -> new_data:(k2, v, k2_cmp) Map.t
        -> (k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t
        -> (k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t
        =
        fun ~key:k1 ~old_data ~new_data acc ->
        Map.fold_symmetric_diff
          old_data
          new_data
          ~data_equal
          ~init:acc
          ~f:(fun acc (k2, diff) ->
            let value =
              match diff with
              | `Left _ -> None
              | `Right x | `Unequal (_, x) -> Some x
            in
            Map.change acc k2 ~f:(fun acc_inner ->
              let acc_inner =
                Map.change
                  (Option.value acc_inner ~default:(Map.empty k1_comparator))
                  k1
                  ~f:(fun _ -> value)
              in
              if Map.is_empty acc_inner then None else Some acc_inner))
      in
      let add ~key ~data =
        update ~key ~old_data:(Map.empty k2_comparator) ~new_data:data
      in
      let remove ~key ~data =
        update ~key ~old_data:data ~new_data:(Map.empty k2_comparator)
      in
      unordered_fold
        m
        ~instrumentation
        ~init:(Map.empty k2_comparator)
        ~revert_to_init_when_empty:true
        ~update
        ~add
        ~remove)
  ;;

  let collapse_by
    (type outer_key outer_cmp inner_key inner_cmp combined_key combined_cmp)
    ?(instrumentation = no_instrumentation)
    ?data_equal
    (map_incr :
      ((outer_key, (inner_key, _, inner_cmp) Map.t, outer_cmp) Map.t, _) Incremental.t)
    ~(merge_keys : outer_key -> inner_key -> combined_key)
    ~(comparator : (combined_key, combined_cmp) Comparator.Module.t)
    =
    unordered_fold_nested_maps
      ~instrumentation
      ?data_equal
      map_incr
      ~init:(Map.empty comparator)
      ~revert_to_init_when_empty:true
      ~update:(fun ~outer_key ~inner_key ~old_data:_ ~new_data acc ->
        Map.set acc ~key:(merge_keys outer_key inner_key) ~data:new_data)
      ~add:(fun ~outer_key ~inner_key ~data acc ->
        Map.add_exn acc ~key:(merge_keys outer_key inner_key) ~data)
      ~remove:(fun ~outer_key ~inner_key ~data:_ acc ->
        Map.remove acc (merge_keys outer_key inner_key))
  ;;

  let collapse_by_loosened_requirements
    (type outer_key outer_cmp inner_key inner_cmp combined_key combined_cmp)
    ?(instrumentation = no_instrumentation)
    ?data_equal
    (map_incr :
      ((outer_key, (inner_key, _, inner_cmp) Map.t, outer_cmp) Map.t, _) Incremental.t)
    ~(merge_keys : outer_key -> inner_key -> combined_key)
    ~(comparator : (combined_key, combined_cmp) Comparator.Module.t)
    =
    unordered_fold_nested_maps_with_comparators
      ~instrumentation
      ?data_equal
      map_incr
      ~init:(Map.empty comparator)
      ~revert_to_init_when_empty:true
      ~update:
        (fun
          ~outer_cmp:_ ~inner_cmp:_ ~outer_key ~inner_key ~old_data:_ ~new_data acc ->
        Map.set
          acc
          ~key:(merge_keys outer_key inner_key)
          ~data:(outer_key, inner_key, new_data))
      ~add:(fun ~outer_cmp:_ ~inner_cmp:_ ~outer_key ~inner_key ~data acc ->
        Map.set
          acc
          ~key:(merge_keys outer_key inner_key)
          ~data:(outer_key, inner_key, data))
      ~remove:(fun ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~data:_ acc ->
        let merged_keys = merge_keys outer_key inner_key in
        Map.change acc merged_keys ~f:(function
          | None -> None
          | Some ((prev_outer_key, prev_inner_key, _) as prev) ->
            if outer_cmp.compare prev_outer_key outer_key = 0
               && inner_cmp.compare prev_inner_key inner_key = 0
            then None
            else
              (* if the outer and inner keys aren't the same, then that's because the same
              merged-key has been added in the meantime, so we don't need to remove anything *)
              Some prev))
    |> map ~f:(fun (_, _, data) -> data)
  ;;

  let collapse
    (type outer_key outer_cmp inner_key inner_cmp)
    ?instrumentation
    ?outer_comparator
    ?data_equal
    (map_incr :
      ((outer_key, (inner_key, _, inner_cmp) Map.t, outer_cmp) Map.t, _) Incremental.t)
    ~comparator:(inner_comparator : (inner_key, inner_cmp) Comparator.Module.t)
    =
    with_comparator ~comparator:outer_comparator map_incr (fun outer_comparator ->
      let module Cmp = struct
        type t = outer_key * inner_key
        type comparator_witness = (outer_cmp, inner_cmp) Tuple2.comparator_witness

        let comparator =
          let inner_comparator =
            let module M = (val inner_comparator) in
            M.comparator
          in
          let outer_comparator =
            let module M = (val outer_comparator) in
            M.comparator
          in
          Tuple2.comparator outer_comparator inner_comparator
        ;;
      end
      in
      collapse_by
        ?instrumentation
        ?data_equal
        map_incr
        ~merge_keys:Tuple2.create
        ~comparator:(module Cmp))
  ;;

  let expand
    ?(instrumentation = no_instrumentation)
    ?data_equal
    map_incr
    ~outer_comparator
    ~inner_comparator
    =
    unordered_fold
      ~instrumentation
      ?data_equal
      map_incr
      ~init:(Map.empty outer_comparator)
      ~revert_to_init_when_empty:true
      ~update:(fun ~key:(outer_key, inner_key) ~old_data:_ ~new_data acc ->
        Map.update acc outer_key ~f:(function
          | None -> Map.singleton inner_comparator inner_key new_data
          | Some map -> Map.set map ~key:inner_key ~data:new_data))
      ~add:(fun ~key:(outer_key, inner_key) ~data acc ->
        Map.update acc outer_key ~f:(function
          | None -> Map.singleton inner_comparator inner_key data
          | Some map -> Map.add_exn map ~key:inner_key ~data))
      ~remove:(fun ~key:(outer_key, inner_key) ~data:_ acc ->
        Map.change acc outer_key ~f:(function
          | None -> None
          | Some map ->
            let map = Map.remove map inner_key in
            Option.some_if (not (Map.is_empty map)) map))
  ;;

  let counti ?(instrumentation = no_instrumentation) ?data_equal map_incr ~f =
    unordered_fold
      ~instrumentation
      ?data_equal
      map_incr
      ~init:0
      ~revert_to_init_when_empty:true
      ~add:(fun ~key ~data count -> if f ~key ~data then count + 1 else count)
      ~remove:(fun ~key ~data count -> if f ~key ~data then count - 1 else count)
  ;;

  let count ?instrumentation ?data_equal map_incr ~f =
    counti ?instrumentation ?data_equal map_incr ~f:(fun ~key:_ ~data -> f data)
  ;;

  let existsi ?instrumentation ?data_equal map_incr ~f =
    Incremental.map (counti ?instrumentation ?data_equal map_incr ~f) ~f:(fun count ->
      count <> 0)
  ;;

  let exists ?instrumentation ?data_equal map_incr ~f =
    existsi ?instrumentation ?data_equal map_incr ~f:(fun ~key:_ ~data -> f data)
  ;;

  let sum
    (type u)
    ?(instrumentation = no_instrumentation)
    ?data_equal
    (map_incr : ((_, _, _) Map.t, _) Incremental.t)
    (module Group : Abstract_algebra.Commutative_group.Without_sexp with type t = u)
    ~f
    =
    unordered_fold
      ~instrumentation
      ?data_equal
      map_incr
      ~init:Group.zero
      ~revert_to_init_when_empty:true
      ~add:(fun ~key:_ ~data:v acc -> Group.( + ) acc (f v))
      ~remove:(fun ~key:_ ~data:v acc -> Group.( - ) acc (f v))
  ;;

  let observe_changes_exn ?(data_equal = phys_equal) map ~f =
    let state = Incremental.state map in
    let scope = Incremental.Scope.current state () in
    if not (Incremental.Scope.is_top scope)
    then failwith "[Incr_map.observe_changes_exn] called in scope that is not top-level";
    let on_diff v1 v2 =
      Map.fold_symmetric_diff v1 v2 ~data_equal ~init:() ~f:(fun () update -> f update)
    in
    let empty_version_of map = Map.empty (Map.comparator_s map) in
    let observer = Incremental.observe map in
    Incremental.Observer.on_update_exn observer ~f:(fun diff_elt ->
      let before_and_after =
        match diff_elt with
        | Invalidated ->
          (match Incremental.Observer.value observer with
           | Ok final_value -> Some (final_value, empty_version_of final_value)
           | Error _ -> None)
        | Initialized v -> Some (empty_version_of v, v)
        | Changed (v1, v2) -> Some (v1, v2)
      in
      match before_and_after with
      | None -> ()
      | Some (before, after) -> on_diff before after)
  ;;

  let for_alli ?instrumentation ?data_equal map_incr ~f =
    Incremental.map
      (counti ?instrumentation ?data_equal map_incr ~f:(fun ~key ~data ->
         not (f ~key ~data)))
      ~f:(fun count -> count = 0)
  ;;

  let for_all ?instrumentation ?data_equal map_incr ~f =
    for_alli ?instrumentation ?data_equal map_incr ~f:(fun ~key:_ ~data -> f data)
  ;;

  let unordered_fold
    ?(instrumentation = no_instrumentation)
    ?data_equal
    ?update
    ?specialized_initial
    ?finalize
    ?revert_to_init_when_empty
    map
    ~init
    ~add
    ~remove
    =
    unordered_fold
      ~instrumentation
      ?data_equal
      ?update
      ?specialized_initial
      ?finalize
      ?revert_to_init_when_empty
      map
      ~init
      ~add
      ~remove
  ;;

  let unordered_fold_nested_maps
    ?(instrumentation = no_instrumentation)
    ?data_equal
    ?revert_to_init_when_empty
    ?update
    map
    ~init
    ~add
    ~remove
    =
    unordered_fold_nested_maps
      ~instrumentation
      ?data_equal
      ?revert_to_init_when_empty
      ?update
      map
      ~init
      ~add
      ~remove
  ;;

  module For_testing = struct
    let find_key_range_linear = find_key_range_linear
  end

  module Lookup = struct
    type ('v, 'w) entry =
      { mutable saved_value : 'v option
      ; node : ('v option, 'w) Incremental.Expert.Node.t
      }

    let update_entry entry value =
      entry.saved_value <- value;
      Incremental.Expert.Node.make_stale entry.node
    ;;

    type ('k, 'v, 'cmp, 'w) t =
      { mutable saved_map : ('k, 'v, 'cmp) Map.t
          (* We may have multiple entries per key if nodes become necessary again after being
         removed. *)
      ; mutable lookup_entries : ('k, ('v, 'w) entry list, 'cmp) Map.t
      ; updater_node : (unit, 'w) Incremental.t
      ; scope : 'w Incremental.Scope.t
      }

    module M (K : sig
        type t
        type comparator_witness
      end) =
    struct
      type nonrec ('v, 'w) t = (K.t, 'v, K.comparator_witness, 'w) t
    end

    let create
      ?(instrumentation = no_instrumentation)
      ?(data_equal = phys_equal)
      comparator
      input_map
      =
      let rec self =
        lazy
          (let updater_node =
             Incremental.map input_map ~f:(fun input_map ->
               instrumentation.Instrumentation.f (fun () ->
                 let (lazy self) = self in
                 Map.fold_symmetric_diff
                   self.saved_map
                   input_map
                   ~data_equal
                   ~init:()
                   ~f:(fun () (key, changed_value) ->
                     let entries = Map.find_multi self.lookup_entries key in
                     List.iter entries ~f:(fun entry ->
                       update_entry
                         entry
                         (match changed_value with
                          | `Left _ -> None
                          | `Right new_value | `Unequal (_, new_value) -> Some new_value)));
                 self.saved_map <- input_map))
           in
           let empty_map = Map.empty comparator in
           { saved_map = empty_map
           ; lookup_entries = empty_map
           ; updater_node
           ; scope = Incremental.Scope.current (Incremental.state input_map) ()
           })
      in
      Lazy.force self
    ;;

    let[@cold] slow_path_link_entry t entry ~key ~is_now_observable =
      let (lazy entry) = entry in
      let current_entries = Map.find_multi t.lookup_entries key in
      let is_linked = List.exists current_entries ~f:(phys_equal entry) in
      if Bool.equal is_linked is_now_observable
      then ()
      else if is_now_observable
      then
        t.lookup_entries
        <- Map.update t.lookup_entries key ~f:(function
             | Some (other_entry :: _ as other_entries) ->
               (* Update this entry's value to be current. *)
               update_entry entry other_entry.saved_value;
               entry :: other_entries
             | None | Some [] ->
               update_entry entry (Map.find t.saved_map key);
               [ entry ])
      else (
        let new_entries =
          List.filter current_entries ~f:(fun x -> not (phys_equal entry x))
        in
        t.lookup_entries
        <- (if List.is_empty new_entries
            then Map.remove t.lookup_entries key
            else Map.set t.lookup_entries ~key ~data:new_entries))
    ;;

    let[@cold] slow_path_create_node t key =
      let incremental_state = Incremental.state t.updater_node in
      Incremental.Scope.within incremental_state t.scope ~f:(fun () ->
        let rec entry =
          lazy
            { saved_value = Map.find t.saved_map key
            ; node =
                Incremental.Expert.Node.create
                  incremental_state
                  (fun () -> (force entry).saved_value)
                  ~on_observability_change:(slow_path_link_entry t entry ~key)
            }
        in
        let (lazy entry) = entry in
        Incremental.Expert.Node.add_dependency
          entry.node
          (Incremental.Expert.Dependency.create t.updater_node);
        Incremental.Expert.Node.watch entry.node)
    ;;

    let find t key =
      match Map.find_multi t.lookup_entries key with
      | entry :: _ -> Incremental.Expert.Node.watch entry.node
      | [] -> slow_path_create_node t key
    ;;

    module For_debug = struct
      let[@cold] sexp_of_entry sexp_of_value entry =
        let { saved_value; node } = entry in
        let node = Incremental.Expert.Node.watch node in
        [%sexp
          { saved_value : value option
          ; node_info = (Incremental.user_info node : (Info.t option[@sexp.option]))
          ; node_is_const =
              (Option.some_if (Incremental.is_const node) ()
               : (unit option[@sexp.option]))
          ; node_is_invalid =
              (Option.some_if (not (Incremental.is_valid node)) ()
               : (unit option[@sexp.option]))
          ; node_is_unnecessary =
              (Option.some_if (not (Incremental.is_necessary node)) ()
               : (unit option[@sexp.option]))
          }]
      ;;

      let[@cold] sexp_of_t sexp_of_key sexp_of_value t =
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

module type S = sig
  type state_witness

  include
    S_gen
    with type 'a Incr.t = ('a, state_witness) Incremental.t
     and type 'a Incr.Cutoff.t = 'a Incremental.Cutoff.t
     and type ('k, 'v, 'cmp) Lookup.t = ('k, 'v, 'cmp, state_witness) Generic.Lookup.t
end

module Make (Incr : Incremental.S) = struct
  include Generic
  module Instrumentation = Instrumentation

  let flatten x = flatten Incr.State.t x

  module Lookup = struct
    include Lookup

    type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp, Incr.state_witness) Lookup.t

    module M (K : sig
        type t
        type comparator_witness
      end) : sig
      type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
    end = struct
      type nonrec 'v t = (K.t, 'v, K.comparator_witness) t
    end
  end
end

include Generic
