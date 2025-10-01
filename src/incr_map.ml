open! Core
include Incr_map_intf

let no_instrumentation = { Instrumentation.f = (fun f -> f ()) }

(** This type lets us capture the kind of map function being performed, so we can with one
    implementation perform map and filter-map operations.

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

(** This module encapsulates some operations on values that have the type
    [_ Map.t option ref]. It is used in the implementation of many Incr_map functions that
    need an empty map, but don't yet have access to a comparator in order to build one. *)
module Map_option_ref : sig @@ portable
  type ('k, 'v, 'cmp) t

  (** Creates a value initialzed to [ref None] *)
  val create_none : unit -> _ t

  (** Gets the value inside the [option ref], and throws an exception if the option is
      None *)
  val value_exn : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Map.t

  (** If the [Map_option_ref] is [None] then set it to [Some] containing an empty map
      whose comparitor is the same as the map passed in as an argument. *)
  val if_none_then_fill_with_empty_map
    :  ('k, _, 'cmp) t
    -> using_the_comparator_from:('k, _, 'cmp) Map.t
    -> unit

  (** Sets the ref to [Some] with the map passed in as an argument *)
  val set : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Map.t -> unit
end = struct
  (* use of uopt is ok because with this type you can't have nested uopts. *)
  type ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) Map.t Uopt.t ref

  let create_none () = ref (Uopt.get_none ())
  let value_exn t = Uopt.value_exn !t

  let if_none_then_fill_with_empty_map t ~using_the_comparator_from =
    if Uopt.is_none !t
    then t := Uopt.some (Map.empty (Map.comparator_s using_the_comparator_from))
  ;;

  let set t v = t := Uopt.some v
end

let within_scope ~incremental_state:state =
  let scope = Incremental.Scope.current state () in
  Staged.stage (fun f -> Incremental.Scope.within state scope ~f)
;;

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
    ?(data_equal = [%eta2 phys_equal])
    ?update
    ?specialized_initial
    ?(finalize = Fn.id)
    ?(revert_to_init_when_empty = false)
    map
    ~compute_init
    ~add
    ~remove
    =
    let update =
      let default ~cmp ~key ~old_data ~new_data acc =
        add ~cmp ~key ~data:new_data (remove ~cmp ~key ~data:old_data acc)
      in
      Option.value update ~default
    in
    let cmp_and_init = ref (Uopt.get_none ()) in
    with_old ~instrumentation map ~f:(fun ~old new_in ->
      let cmp, init =
        match%optional.Uopt !cmp_and_init with
        | Some cmp_and_init -> cmp_and_init
        | None ->
          let cmp = Map.comparator_s new_in in
          let init = compute_init cmp in
          cmp_and_init := Uopt.some (cmp, init);
          cmp, init
      in
      let acc =
        match old with
        | None ->
          (match specialized_initial with
           | None -> Map.fold ~init ~f:(add ~cmp) new_in
           | Some initial -> initial ~init new_in)
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
    let add ~cmp:_ ~key ~data acc = add ~key ~data acc in
    let remove ~cmp:_ ~key ~data acc = remove ~key ~data acc in
    let update =
      Option.map update ~f:(fun update ~cmp:_ ~key ~old_data ~new_data acc ->
        update ~key ~old_data ~new_data acc)
    in
    unordered_fold_with_comparator
      ~instrumentation
      ?data_equal
      ?update
      ?finalize
      ?revert_to_init_when_empty
      ?specialized_initial
      map
      ~compute_init:(fun _ -> init)
      ~add
      ~remove
  ;;

  let unordered_fold_nested_maps_with_comparators
    ~instrumentation
    ?(data_equal = [%eta2 phys_equal])
    ?revert_to_init_when_empty
    ?update
    incr_map
    ~compute_init
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
      ~compute_init
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
              update ~outer_cmp ~inner_cmp ~outer_key ~inner_key ~old_data ~new_data acc)
        [@nontail])
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
      ~compute_init:(fun _cmp -> init)
      ~add
      ~remove
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
    ?(data_equal = [%eta2 phys_equal])
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
    ?(data_equal = [%eta2 phys_equal])
    ?(extra_equal = [%eta2 phys_equal])
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
    ?(data_equal = [%eta2 phys_equal])
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
        if (Comparator.compare M.comparator) prev_key new_key = 0
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
    let local_ apply_right output (key, diff_element) =
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
          ~f:(local_ fun (output, right_diffs) (left_key, left_diff_element) ->
            let rec local_ loop
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
              (Comparator.compare comparator)
              ~old_output
              ~output
              right_diffs
              left_key
              left_diff_element
              ~f [@nontail])
      in
      Option.value_map right_diffs ~default:output ~f:(fun (hd, tl) ->
        Sequence.fold ~init:(apply_right output hd) tl ~f:apply_right)
      [@nontail])
  ;;

  let new_data_from_diff_element = function
    | `Left _ -> None
    | `Right x | `Unequal (_, x) -> Some x
  ;;

  let merge
    ?(instrumentation = no_instrumentation)
    ?(data_equal_left = [%eta2 phys_equal])
    ?(data_equal_right = [%eta2 phys_equal])
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
    ?(data_equal_left = [%eta2 phys_equal])
    ?(data_equal_right = [%eta2 phys_equal])
    ?(out_equal = [%eta2 phys_equal])
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
    ?(data_equal = [%eta2 phys_equal])
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
                  ~key:((Comparator.sexp_of_t comparator) key : Sexp.t)])
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

  let generic_mapi'
    (type input_data output_data f_output state_witness)
    (witness : (input_data, output_data, f_output) Map_type.t)
    ~instrumentation
    ?cutoff
    ?(data_equal = [%eta2 phys_equal])
    (lhs : (('key, input_data, 'cmp) Map.t, state_witness) Incremental.t)
    ~(f :
        key:'key
        -> data:(input_data, state_witness) Incremental.t
        -> (f_output, state_witness) Incremental.t)
    : (('key, output_data, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state lhs in
    let within_scope = Staged.unstage (within_scope ~incremental_state) in
    let prev_map = Map_option_ref.create_none () in
    let prev_nodes = Map_option_ref.create_none () in
    let acc : ('key, output_data, 'cmp) Map_option_ref.t =
      Map_option_ref.create_none ()
    in
    let result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn acc)
    in
    let (on_inner_change : key:'key -> f_output -> unit) =
      match witness with
      | Map_type.Map ->
        fun ~key data ->
          let old = Map_option_ref.value_exn acc in
          Map_option_ref.set acc (Map.set old ~key ~data)
      | Map_type.Filter_map ->
        fun ~key opt ->
          let old = Map_option_ref.value_exn acc in
          Map_option_ref.set
            acc
            (match opt with
             | None -> Map.remove old key
             | Some data -> Map.set old ~key ~data)
    in
    let rec lhs_change =
      lazy
        (Incremental.map lhs ~f:(fun map ->
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_map
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_nodes
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             acc
             ~using_the_comparator_from:map;
           instrumentation.Instrumentation.f (fun () ->
             let new_nodes =
               Map.fold_symmetric_diff
                 ~data_equal
                 (Map_option_ref.value_exn prev_map)
                 map
                 ~init:(Map_option_ref.value_exn prev_nodes)
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
                     Map_option_ref.set
                       acc
                       (Map.remove (Map_option_ref.value_exn acc) key);
                     E.Node.invalidate node;
                     nodes
                   | `Right _ ->
                     let node =
                       within_scope (fun () ->
                         E.Node.create incremental_state (fun () ->
                           Map.find_exn (Map_option_ref.value_exn prev_map) key))
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
             Map_option_ref.set prev_nodes new_nodes;
             Map_option_ref.set prev_map map)))
    in
    E.Node.add_dependency result (E.Dependency.create (force lhs_change));
    E.Node.watch result
  ;;

  let filter_mapi' ?(instrumentation = no_instrumentation) ?cutoff ?data_equal map ~f =
    generic_mapi' Map_type.Filter_map ~instrumentation ?cutoff ?data_equal map ~f
  ;;

  let mapi' ?(instrumentation = no_instrumentation) ?cutoff ?data_equal map ~f =
    generic_mapi' Map_type.Map ~instrumentation ?cutoff ?data_equal map ~f
  ;;

  let map' ?instrumentation ?cutoff ?data_equal map ~f =
    mapi' ?instrumentation ?cutoff ?data_equal map ~f:(fun ~key:_ ~data -> f data)
  ;;

  let filter_map' ?instrumentation ?cutoff ?data_equal map ~f =
    filter_mapi' ?instrumentation ?cutoff ?data_equal map ~f:(fun ~key:_ ~data -> f data)
  ;;

  let merge' ?instrumentation ?cutoff ?data_equal_left ?data_equal_right map1 map2 ~f =
    merge
      ?instrumentation
      ?data_equal_left
      ?data_equal_right
      map1
      map2
      ~f:(fun ~key:_ diff -> Some diff)
    |> filter_mapi' ?instrumentation ?cutoff ~f:(fun ~key ~data:diff -> f ~key diff)
  ;;

  let unzip_mapi
    (type v v1 v2 state_witness)
    ?(instrumentation = no_instrumentation)
    ?(data_equal : v -> v -> bool = phys_equal)
    ?(left_result_equal : v1 -> v1 -> bool = phys_equal)
    ?(right_result_equal : v2 -> v2 -> bool = phys_equal)
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
    ~(f : key:'key -> data:v -> v1 * v2)
    : (('key, v1, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v2, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state input in
    let left_acc : ('key, v1, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let left_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn left_acc)
    in
    let right_acc : ('key, v2, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let right_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn right_acc)
    in
    let prev_map = Map_option_ref.create_none () in
    let input_change =
      Incremental.map input ~f:(fun map ->
        Map_option_ref.if_none_then_fill_with_empty_map
          left_acc
          ~using_the_comparator_from:map;
        Map_option_ref.if_none_then_fill_with_empty_map
          right_acc
          ~using_the_comparator_from:map;
        Map_option_ref.if_none_then_fill_with_empty_map
          prev_map
          ~using_the_comparator_from:map;
        let empty_map = lazy (Map.empty (Map.comparator_s map)) in
        instrumentation.Instrumentation.f (fun () ->
          let left, right =
            match Map.is_empty (Map_option_ref.value_exn prev_map), Map.is_empty map with
            | true, true | false, true -> Lazy.force empty_map, Lazy.force empty_map
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
                (Map_option_ref.value_exn prev_map)
                map
                ~init:
                  (Map_option_ref.value_exn left_acc, Map_option_ref.value_exn right_acc)
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
          if not (phys_equal (Map_option_ref.value_exn left_acc) left)
          then E.Node.make_stale left_result;
          if not (phys_equal (Map_option_ref.value_exn right_acc) right)
          then E.Node.make_stale right_result;
          Map_option_ref.set left_acc left;
          Map_option_ref.set right_acc right;
          Map_option_ref.set prev_map map))
    in
    E.Node.add_dependency left_result (E.Dependency.create input_change);
    E.Node.add_dependency right_result (E.Dependency.create input_change);
    E.Node.watch left_result, E.Node.watch right_result
  ;;

  let unzip ?instrumentation ?left_result_equal ?right_result_equal input =
    let data_equal =
      Option.map2 left_result_equal right_result_equal ~f:(fun l r ->
        Tuple2.equal ~eq1:l ~eq2:r)
    in
    unzip_mapi
      ?instrumentation
      ?data_equal
      ?left_result_equal
      ?right_result_equal
      input
      ~f:(fun ~key:_ ~data -> data)
  ;;

  let unzip_mapi'
    (type v v1 v2 state_witness)
    ~instrumentation
    ?cutoff
    ?(data_equal = [%eta2 phys_equal])
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
    ~(f :
        key:'key
        -> data:(v, state_witness) Incremental.t
        -> (v1, state_witness) Incremental.t * (v2, state_witness) Incremental.t)
    : (('key, v1, 'cmp) Map.t, state_witness) Incremental.t
      * (('key, v2, 'cmp) Map.t, state_witness) Incremental.t
    =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state input in
    let within_scope = Staged.unstage (within_scope ~incremental_state) in
    let prev_map = Map_option_ref.create_none () in
    let prev_nodes = Map_option_ref.create_none () in
    let left_acc : ('key, v1, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let left_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn left_acc)
    in
    let right_acc : ('key, v2, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let right_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn right_acc)
    in
    let left_on_inner_change ~key data =
      Map_option_ref.set left_acc (Map.set (Map_option_ref.value_exn left_acc) ~key ~data)
    in
    let right_on_inner_change ~key data =
      Map_option_ref.set
        right_acc
        (Map.set (Map_option_ref.value_exn right_acc) ~key ~data)
    in
    let rec input_change =
      lazy
        (Incremental.map input ~f:(fun map ->
           Map_option_ref.if_none_then_fill_with_empty_map
             left_acc
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             right_acc
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_map
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_nodes
             ~using_the_comparator_from:map;
           instrumentation.Instrumentation.f (fun () ->
             let new_nodes =
               Map.fold_symmetric_diff
                 ~data_equal
                 (Map_option_ref.value_exn prev_map)
                 map
                 ~init:(Map_option_ref.value_exn prev_nodes)
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
                     Map_option_ref.set
                       left_acc
                       (Map.remove (Map_option_ref.value_exn left_acc) key);
                     Map_option_ref.set
                       right_acc
                       (Map.remove (Map_option_ref.value_exn right_acc) key);
                     E.Node.invalidate node;
                     nodes
                   | `Right _ ->
                     let node =
                       within_scope (fun () ->
                         E.Node.create incremental_state (fun () ->
                           Map.find_exn (Map_option_ref.value_exn prev_map) key))
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
             Map_option_ref.set prev_nodes new_nodes;
             Map_option_ref.set prev_map map)))
    in
    E.Node.add_dependency left_result (E.Dependency.create (force input_change));
    E.Node.add_dependency right_result (E.Dependency.create (force input_change));
    E.Node.watch left_result, E.Node.watch right_result
  ;;

  let unzip_mapi' ?(instrumentation = no_instrumentation) ?cutoff ?data_equal map ~f =
    let pair =
      map
      |> unzip_mapi' ~instrumentation ?cutoff ?data_equal ~f
      |> Incremental.return (Incremental.state map)
    in
    Incremental.bind ~f:fst pair, Incremental.bind ~f:snd pair
  ;;

  let unzip3_mapi'
    (type v v1 v2 v3 state_witness)
    ?(instrumentation = no_instrumentation)
    ?cutoff
    ?(data_equal = [%eta2 phys_equal])
    (input : (('key, v, 'cmp) Map.t, state_witness) Incremental.t)
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
    let within_scope = Staged.unstage (within_scope ~incremental_state) in
    let prev_map = Map_option_ref.create_none () in
    let prev_nodes = Map_option_ref.create_none () in
    let left_acc : ('key, v1, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let left_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn left_acc)
    in
    let middle_acc : ('key, v2, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let middle_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn middle_acc)
    in
    let right_acc : ('key, v3, 'cmp) Map_option_ref.t = Map_option_ref.create_none () in
    let right_result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn right_acc)
    in
    let left_on_inner_change ~key data =
      Map_option_ref.set left_acc (Map.set (Map_option_ref.value_exn left_acc) ~key ~data)
    in
    let middle_on_inner_change ~key data =
      Map_option_ref.set
        middle_acc
        (Map.set (Map_option_ref.value_exn middle_acc) ~key ~data)
    in
    let right_on_inner_change ~key data =
      Map_option_ref.set
        right_acc
        (Map.set (Map_option_ref.value_exn right_acc) ~key ~data)
    in
    let rec input_change =
      lazy
        (Incremental.map input ~f:(fun map ->
           Map_option_ref.if_none_then_fill_with_empty_map
             left_acc
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             right_acc
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             middle_acc
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_map
             ~using_the_comparator_from:map;
           Map_option_ref.if_none_then_fill_with_empty_map
             prev_nodes
             ~using_the_comparator_from:map;
           instrumentation.Instrumentation.f (fun () ->
             let new_nodes =
               Map.fold_symmetric_diff
                 ~data_equal
                 (Map_option_ref.value_exn prev_map)
                 map
                 ~init:(Map_option_ref.value_exn prev_nodes)
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
                     Map_option_ref.set
                       left_acc
                       (Map.remove (Map_option_ref.value_exn left_acc) key);
                     Map_option_ref.set
                       middle_acc
                       (Map.remove (Map_option_ref.value_exn middle_acc) key);
                     Map_option_ref.set
                       right_acc
                       (Map.remove (Map_option_ref.value_exn right_acc) key);
                     E.Node.invalidate node;
                     nodes
                   | `Right _ ->
                     let node =
                       within_scope (fun () ->
                         E.Node.create incremental_state (fun () ->
                           Map.find_exn (Map_option_ref.value_exn prev_map) key))
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
             Map_option_ref.set prev_nodes new_nodes;
             Map_option_ref.set prev_map map)))
    in
    E.Node.add_dependency left_result (E.Dependency.create (force input_change));
    E.Node.add_dependency middle_result (E.Dependency.create (force input_change));
    E.Node.add_dependency right_result (E.Dependency.create (force input_change));
    E.Node.watch left_result, E.Node.watch middle_result, E.Node.watch right_result
  ;;

  let keys ?(instrumentation = no_instrumentation) map =
    let add ~cmp:_ ~key ~data:_ acc = Set.add acc key in
    let remove ~cmp:_ ~key ~data:_ acc = Set.remove acc key in
    let data_equal _ _ = true in
    unordered_fold_with_comparator
      map
      ~instrumentation
      ~compute_init:(fun cmp -> Set.empty cmp)
      ~revert_to_init_when_empty:true
      ~data_equal
      ~add
      ~remove
  ;;

  let partition_mapi ?(instrumentation = no_instrumentation) ?data_equal map ~f =
    unordered_fold_with_comparator
      ?data_equal
      map
      ~instrumentation
      ~compute_init:(fun cmp ->
        let empty = Map.empty cmp in
        empty, empty)
      ~revert_to_init_when_empty:true
      ~update:(fun ~cmp:_ ~key ~old_data:_ ~new_data:data (first, second) ->
        match f ~key ~data with
        | First data -> Map.set first ~key ~data, Map.remove second key
        | Second data -> Map.remove first key, Map.set second ~key ~data)
      ~add:(fun ~cmp:_ ~key ~data (first, second) ->
        match f ~key ~data with
        | First data -> Map.add_exn first ~key ~data, second
        | Second data -> first, Map.add_exn second ~key ~data)
      ~remove:(fun ~cmp:_ ~key ~data:_ (first, second) ->
        Map.remove first key, Map.remove second key)
  ;;

  let partition_mapi' ?instrumentation ?cutoff ?data_equal map ~f =
    mapi' ?instrumentation ?cutoff ?data_equal map ~f
    |> partition_mapi ?instrumentation ~f:(fun ~key:_ ~data -> data)
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

  let join ?(instrumentation = no_instrumentation) map_incr =
    let module E = Incremental.Expert in
    let incremental_state = Incremental.state map_incr in
    let result_map = Map_option_ref.create_none () in
    let old_map_of_incrs = Map_option_ref.create_none () in
    let current_dependencies = Map_option_ref.create_none () in
    let result =
      E.Node.create incremental_state (fun () -> Map_option_ref.value_exn result_map)
    in
    let add_subnode current_dependencies ~key ~data_node =
      let new_dep =
        E.Dependency.create data_node ~on_change:(fun data ->
          Map_option_ref.set
            result_map
            (Map.set (Map_option_ref.value_exn result_map) ~key ~data))
      in
      E.Node.add_dependency result new_dep;
      Map.set current_dependencies ~key ~data:new_dep
    in
    let remove_subnode current_dependencies ~key =
      let dep = Map.find_exn current_dependencies key in
      E.Node.remove_dependency result dep;
      Map_option_ref.set result_map (Map.remove (Map_option_ref.value_exn result_map) key);
      Map.remove current_dependencies key
    in
    let lhs_change =
      Incremental.map map_incr ~f:(fun map_of_incrs ->
        Map_option_ref.if_none_then_fill_with_empty_map
          result_map
          ~using_the_comparator_from:map_of_incrs;
        Map_option_ref.if_none_then_fill_with_empty_map
          old_map_of_incrs
          ~using_the_comparator_from:map_of_incrs;
        Map_option_ref.if_none_then_fill_with_empty_map
          current_dependencies
          ~using_the_comparator_from:map_of_incrs;
        instrumentation.Instrumentation.f (fun () ->
          let new_dependency_map =
            Map.fold_symmetric_diff
              ~data_equal:phys_equal
              (Map_option_ref.value_exn old_map_of_incrs)
              map_of_incrs
              ~init:(Map_option_ref.value_exn current_dependencies)
              ~f:(fun current_dependencies (key, diff) ->
                match diff with
                | `Left _ -> remove_subnode current_dependencies ~key
                | `Right data_node -> add_subnode current_dependencies ~key ~data_node
                | `Unequal (_, data_node) ->
                  remove_subnode current_dependencies ~key |> add_subnode ~key ~data_node)
          in
          Map_option_ref.set current_dependencies new_dependency_map;
          Map_option_ref.set old_map_of_incrs map_of_incrs))
    in
    E.Node.add_dependency result (E.Dependency.create lhs_change);
    E.Node.watch result
  ;;

  module Separate_state = struct
    type ('k, 'v, 'cmp, 'w) t =
      { input_map : ('k, 'v, 'cmp) Map_option_ref.t
      ; expert_nodes : ('k, ('v, 'w) Incremental.Expert.Node.t, 'cmp) Map_option_ref.t
      ; output_map : ('k, ('v, 'w) Incremental.t, 'cmp) Map_option_ref.t
      }

    let create () =
      { input_map = Map_option_ref.create_none ()
      ; expert_nodes = Map_option_ref.create_none ()
      ; output_map = Map_option_ref.create_none ()
      }
    ;;

    let create_lookup_node state t key =
      Incremental.Expert.Node.create state (fun () ->
        Map.find_exn (Map_option_ref.value_exn t.input_map) key)
    ;;

    let fill { input_map; expert_nodes; output_map } ~using_the_comparator_from =
      Map_option_ref.if_none_then_fill_with_empty_map input_map ~using_the_comparator_from;
      Map_option_ref.if_none_then_fill_with_empty_map
        expert_nodes
        ~using_the_comparator_from;
      Map_option_ref.if_none_then_fill_with_empty_map
        output_map
        ~using_the_comparator_from
    ;;
  end

  let separate ?(instrumentation = no_instrumentation) input_map ~data_equal =
    let incremental_state = Incremental.state input_map in
    let within_scope = Staged.unstage (within_scope ~incremental_state) in
    let state = Separate_state.create () in
    let output_map_node =
      Incremental.Expert.Node.create incremental_state (fun () ->
        Map_option_ref.value_exn state.output_map)
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
         recomputed when they are explicitly made stale. *)
    let rec input_map_changed =
      lazy
        (Incremental.map input_map ~f:(fun input_map ->
           Separate_state.fill state ~using_the_comparator_from:input_map;
           instrumentation.Instrumentation.f (fun () ->
             let prev_input_map = state.input_map in
             let expert_nodes, output_map =
               Map.fold_symmetric_diff
                 (Map_option_ref.value_exn prev_input_map)
                 input_map
                 ~data_equal
                 ~init:
                   ( Map_option_ref.value_exn state.expert_nodes
                   , Map_option_ref.value_exn state.output_map )
                 ~f:(fun (expert_nodes, output_map) (key, change) ->
                   match change with
                   | `Left _old_value ->
                     let old_node = Map.find_exn expert_nodes key in
                     Incremental.Expert.Node.invalidate old_node;
                     Incremental.Expert.Node.make_stale output_map_node;
                     Map.remove expert_nodes key, Map.remove output_map key
                   | `Right _new_value ->
                     let node =
                       within_scope (fun () ->
                         Separate_state.create_lookup_node incremental_state state key)
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
             Map_option_ref.set state.input_map input_map;
             Map_option_ref.set state.expert_nodes expert_nodes;
             Map_option_ref.set state.output_map output_map)))
    in
    make_node_depend_on_input_map_changed output_map_node ~input_map_changed;
    Incremental.Expert.Node.watch output_map_node
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
    ?(data_equal = [%eta2 phys_equal])
    (map_incr : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    range
    =
    with_old2 ~instrumentation map_incr range ~f:(fun ~old map range ->
      let compare = Comparator.compare (Map.comparator map) in
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
        if (Comparator.compare (Map.comparator output)) prev_key new_key = 0
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
    ?data_equal
    map_incr
    ~comparator:outer_comparator
    ~index
    =
    unordered_fold_with_comparator
      ?data_equal
      ~instrumentation
      map_incr
      ~compute_init:(fun _ -> Map.empty outer_comparator)
      ~revert_to_init_when_empty:true
      ~add:(fun ~cmp ~key:inner_key ~data outer_map ->
        match index ~key:inner_key ~data with
        | None -> outer_map
        | Some outer_key ->
          Map.update outer_map outer_key ~f:(function
            | None -> Map.singleton cmp inner_key data
            | Some inner_map -> Map.add_exn inner_map ~key:inner_key ~data))
      ~remove:(fun ~cmp:_ ~key:inner_key ~data outer_map ->
        match index ~key:inner_key ~data with
        | None -> outer_map
        | Some outer_key ->
          Map.change outer_map outer_key ~f:(function
            | None -> failwith "BUG: Hit supposedly impossible case in Incr_map.index_by"
            | Some inner_map ->
              let inner_map = Map.remove inner_map inner_key in
              if Map.is_empty inner_map then None else Some inner_map))
  ;;

  let index_by ?instrumentation ?data_equal map_incr ~comparator ~index =
    index_byi
      ?instrumentation
      ?data_equal
      map_incr
      ~comparator
      ~index:(fun ~key:_ ~data -> index data)
  ;;

  let rank
    (type k v cmp state_witness)
    ?(instrumentation = no_instrumentation)
    (map : ((k, v, cmp) Map.t, state_witness) Incremental.t)
    (key : (k, state_witness) Incremental.t)
    =
    Incremental.map2 map key ~f:(fun map key ->
      instrumentation.Instrumentation.f (fun () -> Map.rank map key))
  ;;

  (** Range map by indices *)
  let subrange_by_rank
    (type k state_witness)
    ?(instrumentation = no_instrumentation)
    (map : ((k, _, _) Map.t, state_witness) Incremental.t)
    (range : (int Maybe_bound.t * int Maybe_bound.t, state_witness) Incremental.t)
    =
    Incremental.map2 map range ~f:(fun map (lower_bound, upper_bound) ->
      instrumentation.Instrumentation.f (fun () ->
        if Map.is_empty map
        then map
        else (
          let lower_bound_inclusive =
            let n =
              match lower_bound with
              | (Incl x | Excl x) when x < 0 -> 0
              | Incl x -> x
              | Excl x -> x + 1
              | Unbounded -> 0
            in
            Map.nth map n
          in
          let upper_bound_inclusive =
            let n =
              match upper_bound with
              | (Incl x | Excl x) when x >= Map.length map -> Map.length map - 1
              | Incl x -> x
              | Excl x -> x - 1
              | Unbounded -> Map.length map - 1
            in
            Map.nth map n
          in
          match lower_bound_inclusive, upper_bound_inclusive with
          | None, _ | _, None ->
            Map.Using_comparator.empty ~comparator:(Map.comparator map)
          | Some (lower_bound_inclusive, _), Some (upper_bound_inclusive, _) ->
            Map.subrange
              ~lower_bound:(Incl lower_bound_inclusive)
              ~upper_bound:(Incl upper_bound_inclusive)
              map)))
  ;;

  let transpose
    : type k1 k2 v k1_cmp k2_cmp state_witness.
      ?instrumentation:Instrumentation.t
      -> ?data_equal:(v -> v -> bool)
      -> (k2, k2_cmp) Comparator.Module.t
      -> ((k1, (k2, v, k2_cmp) Map.t, k1_cmp) Map.t, state_witness) Incremental.t
      -> ((k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t, state_witness) Incremental.t
    =
    fun ?(instrumentation = no_instrumentation)
      ?(data_equal = [%eta2 phys_equal])
      k2_comparator
      m ->
    let update
      :  cmp:(k1, k1_cmp) Comparator.Module.t -> key:k1 -> old_data:(k2, v, k2_cmp) Map.t
      -> new_data:(k2, v, k2_cmp) Map.t -> (k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t
      -> (k2, (k1, v, k1_cmp) Map.t, k2_cmp) Map.t
      =
      fun ~cmp:k1_comparator ~key:k1 ~old_data ~new_data acc ->
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
    let add ~cmp ~key ~data =
      update ~cmp ~key ~old_data:(Map.empty k2_comparator) ~new_data:data
    in
    let remove ~cmp ~key ~data =
      update ~cmp ~key ~old_data:data ~new_data:(Map.empty k2_comparator)
    in
    unordered_fold_with_comparator
      m
      ~instrumentation
      ~compute_init:(fun _ -> Map.empty k2_comparator)
      ~revert_to_init_when_empty:true
      ~update
      ~add
      ~remove
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
      ~compute_init:(fun _cmp -> Map.empty comparator)
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
        let module Outer_cmp = (val outer_cmp) in
        let merged_keys = merge_keys outer_key inner_key in
        Map.change acc merged_keys ~f:(function
          | None -> None
          | Some ((prev_outer_key, prev_inner_key, _) as prev) ->
            if (Comparator.compare Outer_cmp.comparator) prev_outer_key outer_key = 0
               && (Comparator.compare inner_cmp) prev_inner_key inner_key = 0
            then None
            else
              (* if the outer and inner keys aren't the same, then that's because the same
              merged-key has been added in the meantime, so we don't need to remove anything *)
              Some prev))
    |> map ~f:(fun (_, _, data) -> data)
  ;;

  let collapse
    (type outer_key outer_cmp inner_key inner_cmp)
    ?(instrumentation = no_instrumentation)
    ?data_equal
    (map_incr :
      ((outer_key, (inner_key, _, inner_cmp) Map.t, outer_cmp) Map.t, _) Incremental.t)
    ~comparator:(inner_comparator : (inner_key, inner_cmp) Comparator.Module.t)
    =
    unordered_fold_nested_maps_with_comparators
      ~instrumentation
      ?data_equal
      map_incr
      ~compute_init:(fun outer_comparator ->
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
        Map.empty (module Cmp))
      ~revert_to_init_when_empty:true
      ~update:
        (fun
          ~outer_cmp:_ ~inner_cmp:_ ~outer_key ~inner_key ~old_data:_ ~new_data acc ->
        Map.set acc ~key:(outer_key, inner_key) ~data:new_data)
      ~add:(fun ~outer_cmp:_ ~inner_cmp:_ ~outer_key ~inner_key ~data acc ->
        Map.add_exn acc ~key:(outer_key, inner_key) ~data)
      ~remove:(fun ~outer_cmp:_ ~inner_cmp:_ ~outer_key ~inner_key ~data:_ acc ->
        Map.remove acc (outer_key, inner_key))
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

  let observe_changes_exn ?(data_equal = [%eta2 phys_equal]) map ~f =
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

  let cartesian_product
    ?(instrumentation = no_instrumentation)
    ?(data_equal_left = [%eta2 phys_equal])
    ?(data_equal_right = [%eta2 phys_equal])
    m1
    m2
    =
    with_old2 ~instrumentation m1 m2 ~f:(fun ~old new_m1 new_m2 ->
      (* old is None if this is the first call to this function - we must compute the
         cartesian product from scratch from the two maps.

         Otherwise, we can incrementally compute the cartesian product given the two
         previous maps, the new maps, and the old output. *)
      match old with
      | None ->
        let cmp1 = Map.comparator new_m1 in
        let cmp2 = Map.comparator new_m2 in
        let output =
          List.cartesian_product (Map.to_alist new_m1) (Map.to_alist new_m2)
          |> List.map ~f:(fun ((k1, v1), (k2, v2)) -> (k1, k2), (v1, v2))
          |> Map.Using_comparator.of_alist_exn ~comparator:(Tuple2.comparator cmp1 cmp2)
        in
        output
      | Some (old_m1, old_m2, old_output) ->
        (* Find changes between the old m1 and the new m1 and update output by
           updating/removing all tuples matching the changed key/value
        *)
        let output =
          Map.fold_symmetric_diff
            old_m1
            new_m1
            ~data_equal:data_equal_left
            ~init:old_output
            ~f:(fun output -> function
            | removed_key, `Left _ ->
              Map.fold old_m2 ~init:output ~f:(fun ~key ~data:_ output ->
                Map.remove output (removed_key, key))
            | new_key, `Right new_value | new_key, `Unequal (_, new_value) ->
              Map.fold new_m2 ~init:output ~f:(fun ~key ~data output ->
                Map.set output ~key:(new_key, key) ~data:(new_value, data)))
        in
        (* Find changes between the old m2 and the new m2 and update output accordingly *)
        let output =
          Map.fold_symmetric_diff
            old_m2
            new_m2
            ~data_equal:data_equal_right
            ~init:output
            ~f:(fun output -> function
            | removed_key, `Left _ ->
              Map.fold old_m1 ~init:output ~f:(fun ~key ~data:_ output ->
                Map.remove output (key, removed_key))
            | new_key, `Right new_value | new_key, `Unequal (_, new_value) ->
              Map.fold new_m1 ~init:output ~f:(fun ~key ~data output ->
                Map.set output ~key:(key, new_key) ~data:(data, new_value)))
        in
        output)
  ;;

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
      ?(data_equal = [%eta2 phys_equal])
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
