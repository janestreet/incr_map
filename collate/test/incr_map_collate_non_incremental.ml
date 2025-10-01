open! Core
open Incr_map_collate

let apply_filter filter_to_predicate filter (lst : (string * int) list) =
  match filter_to_predicate filter with
  | None -> lst
  | Some predicate -> List.filter lst ~f:(fun (key, data) -> predicate ~key ~data)
;;

let apply_order order_to_compare order ~compare_key (lst : (string * int) list) =
  match order_to_compare order with
  | Compare.Unchanged -> lst
  | Compare.Reversed -> List.rev lst
  | Compare.Custom_by_value { compare } ->
    List.sort lst ~compare:(fun (k1, v1) (k2, v2) ->
      let c = compare v1 v2 in
      if c <> 0 then c else compare_key k1 k2)
  | Compare.Custom_by_key_and_value { compare } ->
    List.sort lst ~compare:(fun (k1, v1) (k2, v2) ->
      let c = compare (k1, v1) (k2, v2) in
      if c <> 0 then c else compare_key k1 k2)
;;

let collate ~operation_order ~filter_to_predicate ~order_to_compare map params =
  let open Collate_params in
  let { filter; order; key_range; rank_range; widen_range_by } = params in
  let num_unfiltered_rows = Map.length map in
  let compare_key = Comparator.compare (Map.comparator map) in
  let apply_filter_and_order list =
    match operation_order with
    | `Filter_first ->
      list
      |> apply_filter filter_to_predicate filter
      |> apply_order order_to_compare order ~compare_key
    | `Sort_first ->
      list
      |> apply_order order_to_compare order ~compare_key
      |> apply_filter filter_to_predicate filter
  in
  let list = Map.to_alist map in
  let filtered_sorted_list = apply_filter_and_order list in
  let num_filtered_rows = List.length filtered_sorted_list in
  (* Compute key_range as a rank range (bounds are inclusive/exclusive via Maybe_bound). Handles "missing" keys as best as possible. *)
  let key_range_as_rank_range_from_start =
    let find_gte_index k =
      List.findi filtered_sorted_list ~f:(fun _ (key, _) -> compare_key key k >= 0)
      |> Option.map ~f:fst
    in
    let find_lte_index k =
      List.foldi filtered_sorted_list ~init:None ~f:(fun i acc (key, _) ->
        if compare_key key k <= 0 then Some i else acc)
    in
    let find_exact_index k =
      List.findi filtered_sorted_list ~f:(fun _ (key, _) -> compare_key key k = 0)
      |> Option.map ~f:fst
    in
    (* NB: we branch on whether the dataset is ordered because in the unordered case we
       are able to have smarter behavior for "missing" keys. *)
    match order_to_compare order, key_range with
    | Compare.Unchanged, Collate_params.Which_range.All_rows -> Unbounded, Unbounded
    | Compare.Unchanged, From k ->
      (match find_gte_index k, find_lte_index k with
       | Some i, _ -> Incl i, Unbounded
       | None, Some i -> Excl i, Unbounded
       | None, None -> Unbounded, Unbounded)
    | Compare.Unchanged, To k ->
      (match find_exact_index k, find_lte_index k, find_gte_index k with
       | Some i, _, _ -> Unbounded, Incl i
       | None, Some i, _ -> Unbounded, Incl i
       | None, None, Some i -> Unbounded, Excl i
       | None, None, None -> Unbounded, Unbounded)
    | Compare.Unchanged, Between (k1, k2) ->
      let l =
        match find_gte_index k1, find_lte_index k1 with
        | Some i, _ -> Incl i
        | None, Some i -> Excl i
        | None, None -> Unbounded
      in
      let u =
        match find_exact_index k2, find_lte_index k2, find_gte_index k2 with
        | Some i, _, _ -> Incl i
        | None, Some i, _ -> Incl i
        | None, None, Some i -> Excl i
        | None, None, None -> Unbounded
      in
      l, u
    | _, Collate_params.Which_range.All_rows -> Unbounded, Unbounded
    | _, From k ->
      (match find_exact_index k with
       | Some i -> Incl i, Unbounded
       | None -> Unbounded, Unbounded)
    | _, To k ->
      (match find_exact_index k with
       | Some i -> Unbounded, Incl i
       | None -> Unbounded, Unbounded)
    | _, Between (k1, k2) ->
      let l =
        match find_exact_index k1 with
        | Some i -> Incl i
        | None -> Unbounded
      in
      let u =
        match find_exact_index k2 with
        | Some i -> Incl i
        | None -> Unbounded
      in
      l, u
  in
  (* Compute the length of the key range segment *)
  let length_of_range ~data_length (l, u) =
    let length_to = function
      | Maybe_bound.Unbounded -> data_length
      | Incl i -> i + 1
      | Excl i -> i
    in
    match l with
    | Maybe_bound.Unbounded -> length_to u
    | Incl l -> length_to u - l
    | Excl l -> length_to u - l + 1
  in
  let data_length_of_key_range =
    Int.max
      0
      (length_of_range ~data_length:num_filtered_rows key_range_as_rank_range_from_start)
  in
  (* Convert [rank_range] (relative to key-range) to from-start integer bounds *)
  let of_rank ~data_length = function
    | Collate_params.Rank.From_start i -> i
    | From_end i -> data_length - i - 1
  in
  let which_rank_range_from_start =
    match rank_range with
    | Collate_params.Which_range.All_rows -> Maybe_bound.Unbounded, Maybe_bound.Unbounded
    | From r ->
      ( Maybe_bound.Incl (of_rank ~data_length:data_length_of_key_range r)
      , Maybe_bound.Unbounded )
    | To r ->
      ( Maybe_bound.Unbounded
      , Maybe_bound.Incl (of_rank ~data_length:data_length_of_key_range r) )
    | Between (l, u) ->
      ( Maybe_bound.Incl (of_rank ~data_length:data_length_of_key_range l)
      , Maybe_bound.Incl (of_rank ~data_length:data_length_of_key_range u) )
  in
  (* Remove basis (offset by key-range start) to get absolute bounds in data coordinates *)
  let remove_basis ~basis (l, u) =
    let basis_start, basis_end = basis in
    let start_offset =
      (match basis_start with
       | Maybe_bound.Unbounded -> 0
       | Incl l -> l
       | Excl l -> l + 1)
      |> Int.max 0
    in
    let start =
      match l with
      | Maybe_bound.Unbounded -> basis_start
      | Incl s -> Incl (Int.max 0 s + start_offset)
      | Excl s -> Excl (Int.max (-1) s + start_offset)
    in
    let end_ =
      match u with
      | Maybe_bound.Unbounded -> basis_end
      | Incl e -> Incl (e + start_offset)
      | Excl e -> Excl (e + start_offset)
    in
    let min_bound b1 b2 =
      match b1, b2 with
      | Maybe_bound.Unbounded, _ -> b2
      | _, Maybe_bound.Unbounded -> b1
      | Incl l1, Incl l2 -> Incl (Int.min l1 l2)
      | Incl l1, Excl l2 -> Excl (Int.min (l1 + 1) l2)
      | Excl l1, Incl l2 -> Excl (Int.min l1 (l2 + 1))
      | Excl l1, Excl l2 -> Excl (Int.min l1 l2)
    in
    start, min_bound basis_end end_
  in
  let combined_range_before_widening =
    remove_basis ~basis:key_range_as_rank_range_from_start which_rank_range_from_start
  in
  (* Widen the absolute bounds, clamping to dataset size *)
  let shift_rank_bound ~by ~max_rank = function
    | Maybe_bound.Unbounded -> Maybe_bound.Unbounded, 0
    | Incl i ->
      let max_rank = Int.max i max_rank in
      let min_rank = Int.min i 0 in
      let i_offset = Int.clamp_exn ~min:min_rank ~max:max_rank (i + by) in
      Maybe_bound.Incl i_offset, i_offset - i
    | Excl i ->
      let max_excl = Int.max i (max_rank + 1) in
      let min_excl = Int.min i (-1) in
      let i_offset = Int.clamp_exn ~min:min_excl ~max:max_excl (i + by) in
      Maybe_bound.Excl i_offset, i_offset - i
  in
  let combined_range_for_selecting, widened_before_by, widened_after_by =
    let by_before, by_after = widen_range_by in
    let l, u = combined_range_before_widening in
    let max_rank = num_filtered_rows - 1 in
    let l', shifted_before_by = shift_rank_bound ~by:(-by_before) ~max_rank l in
    let widened_before_by = -shifted_before_by in
    let u', widened_after_by = shift_rank_bound ~by:by_after ~max_rank u in
    (l', u'), widened_before_by, widened_after_by
  in
  (* Apply the widened bounds to select data *)
  let start_index =
    match fst combined_range_for_selecting with
    | Maybe_bound.Unbounded -> 0
    | Incl i -> Int.max 0 i
    | Excl i -> Int.max 0 (i + 1)
  in
  let end_excl_index =
    match snd combined_range_for_selecting with
    | Maybe_bound.Unbounded -> num_filtered_rows
    | Incl i -> Int.min num_filtered_rows (i + 1)
    | Excl i -> Int.min num_filtered_rows i
  in
  let final_list =
    if start_index >= end_excl_index
    then []
    else
      filtered_sorted_list
      |> (fun l -> List.drop l start_index)
      |> fun l -> List.take l (end_excl_index - start_index)
  in
  (* num_before_range is computed from the pre-widened, absolute coMaybe_boundined range *)
  let count_before = function
    | Maybe_bound.Unbounded, _ -> 0
    | Incl i, _ -> Int.max 0 i
    | Excl i, _ -> Int.max 0 (i + 1)
  in
  let total_num_before_range =
    count_before combined_range_before_widening |> Int.min num_filtered_rows
  in
  (* Convert rank_range to from-start integer ranks based on the key-range length *)
  let int_rank_range =
    let open Collate_params in
    Which_range.map rank_range ~f:(fun rank ->
      match rank with
      | Rank.From_start n -> n
      | Rank.From_end n -> data_length_of_key_range - n - 1)
  in
  Collated.For_testing.of_list
    ~num_filtered_rows
    ~key_range
    ~rank_range:int_rank_range
    ~num_before_range:total_num_before_range
    ~range_widened_by:(widened_before_by, widened_after_by)
    ~num_unfiltered_rows
    final_list
;;
