open! Core
open! Import
open! Incremental.Let_syntax

let with_old i ~f =
  let old = ref None in
  Incremental.map i ~f:(fun a ->
    let b = f ~old:!old a in
    old := Some (a, b);
    b)
;;

let with_old2 i1 i2 ~f =
  let old = ref None in
  Incremental.map2 i1 i2 ~f:(fun a1 a2 ->
    let b = f ~old:!old a1 a2 in
    old := Some (a1, a2, b);
    b)
;;

let union s1 s2 =
  with_old2 s1 s2 ~f:(fun ~old new_s1 new_s2 ->
    match old with
    | None -> Set.union new_s1 new_s2
    | Some (old_s1, old_s2, old_output) ->
      let update_output_by_diffing ~old_set ~new_set ~new_other_set ~output =
        Sequence.fold
          (Set.symmetric_diff old_set new_set)
          ~init:output
          ~f:(fun output -> function
          | Second added -> Set.add output added
          | First removed ->
            if Set.mem new_other_set removed then output else Set.remove output removed)
      in
      let output =
        update_output_by_diffing
          ~old_set:old_s1
          ~new_set:new_s1
          ~new_other_set:new_s2
          ~output:old_output
      in
      let output =
        update_output_by_diffing
          ~old_set:old_s2
          ~new_set:new_s2
          ~new_other_set:new_s1
          ~output
      in
      output)
;;

let inter s1 s2 =
  with_old2 s1 s2 ~f:(fun ~old new_s1 new_s2 ->
    match old with
    | None -> Set.inter new_s1 new_s2
    | Some (old_s1, old_s2, old_output) ->
      let update_output_by_diffing ~old_set ~new_set ~new_other_set ~output =
        Sequence.fold
          (Set.symmetric_diff old_set new_set)
          ~init:output
          ~f:(fun output -> function
          | First removed -> Set.remove output removed
          | Second added ->
            if Set.mem new_other_set added then Set.add output added else output)
      in
      let output =
        update_output_by_diffing
          ~old_set:old_s1
          ~new_set:new_s1
          ~new_other_set:new_s2
          ~output:old_output
      in
      let output =
        update_output_by_diffing
          ~old_set:old_s2
          ~new_set:new_s2
          ~new_other_set:new_s1
          ~output
      in
      output)
;;

let filter s ~f =
  with_old s ~f:(fun ~old new_s ->
    match old with
    | None -> Set.filter new_s ~f
    | Some (old_s, output) ->
      Sequence.fold
        (Set.symmetric_diff old_s new_s)
        ~init:output
        ~f:(fun output -> function
        | First removed -> Set.remove output removed
        | Second added -> if f added then Set.add output added else output))
;;

let unordered_fold s ~init ~add ~remove =
  with_old s ~f:(fun ~old new_s ->
    match old with
    | None -> Set.fold new_s ~init ~f:add
    | Some (old_s, output) ->
      Sequence.fold
        (Set.symmetric_diff old_s new_s)
        ~init:output
        ~f:(fun output -> function
        | First removed -> remove output removed
        | Second added -> add output added))
;;

let diff s1 s2 =
  with_old2 s1 s2 ~f:(fun ~old new_s1 new_s2 ->
    match old with
    | None -> Set.diff new_s1 new_s2
    | Some (old_s1, old_s2, output) ->
      let output =
        Sequence.fold
          (Set.symmetric_diff old_s1 new_s1)
          ~init:output
          ~f:(fun output -> function
          | First removed -> Set.remove output removed
          | Second added -> if Set.mem new_s2 added then output else Set.add output added)
      in
      let output =
        Sequence.fold
          (Set.symmetric_diff old_s2 new_s2)
          ~init:output
          ~f:(fun output -> function
          | First removed ->
            if Set.mem new_s1 removed then Set.add output removed else output
          | Second added ->
            if Set.mem new_s1 added then Set.remove output added else output)
      in
      output)
;;

let cartesian_product s1 s2 =
  with_old2 s1 s2 ~f:(fun ~old new_s1 new_s2 ->
    match old with
    | None ->
      let cmp1 = Set.comparator new_s1 in
      let cmp2 = Set.comparator new_s2 in
      List.cartesian_product (Set.to_list new_s1) (Set.to_list new_s2)
      |> Set.Using_comparator.of_list ~comparator:(Tuple2.comparator cmp1 cmp2)
    | Some (old_s1, old_s2, old_output) ->
      let output =
        Set.symmetric_diff old_s1 new_s1
        |> Sequence.fold ~init:old_output ~f:(fun output -> function
          | First removed ->
            Set.fold old_s2 ~init:output ~f:(fun output e2 ->
              Set.remove output (removed, e2))
          | Second added ->
            Set.fold new_s2 ~init:output ~f:(fun output e2 -> Set.add output (added, e2)))
      in
      Set.symmetric_diff old_s2 new_s2
      |> Sequence.fold ~init:output ~f:(fun output -> function
        | First removed ->
          Set.fold old_s1 ~init:output ~f:(fun output e1 ->
            Set.remove output (e1, removed))
        | Second added ->
          Set.fold new_s1 ~init:output ~f:(fun output e1 -> Set.add output (e1, added))))
;;

let union_map_data ~comparator m =
  Incr_map.unordered_fold
    m
    ~init:(Counting_multi_set.empty ~comparator)
    ~update:(fun ~key:_ ~old_data:old_set ~new_data:new_set acc ->
      Set.symmetric_diff old_set new_set
      |> Sequence.fold ~init:acc ~f:(fun acc -> function
        | First ele -> Counting_multi_set.remove acc ele
        | Second ele -> Counting_multi_set.add acc ele))
    ~add:(fun ~key:_ ~data:set acc -> Set.fold set ~init:acc ~f:Counting_multi_set.add)
    ~remove:(fun ~key:_ ~data:set acc ->
      Set.fold set ~init:acc ~f:Counting_multi_set.remove)
  >>| Counting_multi_set.to_set
;;

let observe_changes_exn set ~on_add ~on_remove =
  let state = Incremental.state set in
  let scope = Incremental.Scope.current state () in
  if not (Incremental.Scope.is_top scope)
  then failwith "[Incr_set.observe_changes_exn] called in scope that is not top-level";
  let on_diff v1 v2 =
    Set.symmetric_diff v1 v2
    |> Sequence.iter ~f:(function
      | First x -> on_remove x
      | Second x -> on_add x)
  in
  let empty_version_of set = Set.empty (Set.comparator_s set) in
  let observer = Incremental.observe set in
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
