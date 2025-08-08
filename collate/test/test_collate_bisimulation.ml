open! Core
module Incr = Incremental.Make ()
open Incr_map_collate

(* Non-incremental implementation of collate *)
module Non_incremental = struct
  let apply_filter filter_to_predicate filter map =
    match filter_to_predicate filter with
    | None -> map
    | Some predicate ->
      Map.filter_mapi map ~f:(fun ~key ~data ->
        if predicate ~key ~data then Some data else None)
  ;;

  let apply_order _order_to_compare _order map = map

  let apply_key_range key_range filtered_sorted_list =
    (* Apply key range and return (result_list, num_before_key_range) *)
    match key_range with
    | Collate_params.Which_range.All_rows -> filtered_sorted_list, 0
    | To k ->
      (* Take all elements with key <= k *)
      let rec take_until lst acc =
        match lst with
        | [] -> List.rev acc
        | ((key, _) as hd) :: tl ->
          if String.compare key k <= 0 then take_until tl (hd :: acc) else List.rev acc
      in
      take_until filtered_sorted_list [], 0
    | From k | Between (k, _) ->
      (* Fixed: Count keys strictly less than k for correct num_before_range *)
      let num_before =
        List.count filtered_sorted_list ~f:(fun (key, _) -> String.compare key k < 0)
      in
      (* Now get the actual range *)
      let result_list =
        match key_range with
        | From k ->
          (* Take all elements with key >= k *)
          List.filter filtered_sorted_list ~f:(fun (key, _) -> String.compare key k >= 0)
        | Between (k1, k2) ->
          (* Take all elements with k1 <= key <= k2 *)
          List.filter filtered_sorted_list ~f:(fun (key, _) ->
            String.compare key k1 >= 0 && String.compare key k2 <= 0)
        | _ -> assert false
      in
      result_list, num_before
  ;;

  let apply_rank_range rank_range after_key_range_list =
    (* Apply rank range and return (final_list, num_before_rank_range) *)
    let len = List.length after_key_range_list in
    match rank_range with
    | Collate_params.Which_range.All_rows -> after_key_range_list, 0
    | From rank ->
      let start_idx =
        match rank with
        | Collate_params.Rank.From_start n -> max 0 n
        | From_end n -> max 0 (len - n - 1)
      in
      List.drop after_key_range_list start_idx, start_idx
    | To rank ->
      let end_idx =
        match rank with
        | Collate_params.Rank.From_start n -> n + 1
        | From_end n -> len - n
      in
      if end_idx <= 0
      then (* Negative or zero upper bound returns empty *)
        [], 0
      else (* Normal case *)
        List.take after_key_range_list end_idx, 0
    | Between (from_rank, to_rank) ->
      let start_idx =
        match from_rank with
        | Collate_params.Rank.From_start n -> max 0 n
        | From_end n -> max 0 (len - n - 1)
      in
      let end_idx =
        match to_rank with
        | Collate_params.Rank.From_start n -> n + 1
        | From_end n -> max 0 (len - n)
      in
      if start_idx >= len
      then (* Start beyond end - empty result *)
        [], start_idx
      else if start_idx > end_idx || end_idx <= 0
      then (* Invalid range *)
        [], start_idx
      else (
        (* Normal case *)
        let count = min (end_idx - start_idx) (len - start_idx) in
        ( (after_key_range_list
           |> (fun l -> List.drop l start_idx)
           |> fun l -> List.take l count)
        , start_idx ))
  ;;

  let collate ~operation_order ~filter_to_predicate ~order_to_compare map params =
    let open Collate_params in
    let { filter; order; key_range; rank_range } = params in
    let num_unfiltered_rows = Map.length map in
    let apply_filter_and_order map =
      match operation_order with
      | `Filter_first ->
        map
        |> apply_filter filter_to_predicate filter
        |> apply_order order_to_compare order
      | `Sort_first ->
        map
        |> apply_order order_to_compare order
        |> apply_filter filter_to_predicate filter
    in
    let filtered_sorted_map = apply_filter_and_order map in
    let num_filtered_rows = Map.length filtered_sorted_map in
    let filtered_sorted_list = Map.to_alist filtered_sorted_map in
    (* Apply key range *)
    let after_key_range_list, num_before_key_range =
      apply_key_range key_range filtered_sorted_list
    in
    (* Apply rank range *)
    let final_list, num_before_rank_range =
      apply_rank_range rank_range after_key_range_list
    in
    let total_num_before_range =
      num_before_key_range + num_before_rank_range |> Int.min num_filtered_rows
    in
    let int_rank_range =
      Collate_params.Which_range.map rank_range ~f:(fun rank ->
        match rank with
        | Rank.From_start n -> n
        | Rank.From_end n -> -(n + 1))
    in
    Collated.For_testing.of_list
      ~num_filtered_rows
      ~key_range
      ~rank_range:int_rank_range
      ~num_before_range:total_num_before_range
      ~num_unfiltered_rows
      final_list
  ;;
end

(* Test harness *)
module Test_harness = struct
  module Key = String
  module Value = Int

  type t =
    { map_var : Value.t Key.Map.t Incr.Var.t
    ; params_var : (Key.t, unit, unit) Collate_params.t Incr.Var.t
    ; incremental_result :
        ( Key.t
          , Value.t
          , Key.comparator_witness
          , unit
          , Incr.state_witness )
          Incr_map_collate.t
    ; incremental_observer : (Key.t, Value.t) Collated.t Incr.Observer.t
    }

  let create ~operation_order map params =
    let map_var = Incr.Var.create map in
    let params_var = Incr.Var.create params in
    let incremental_result =
      Incr_map_collate.collate
        ~operation_order
        ~filter_equal:Unit.equal
        ~order_equal:Unit.equal
        ~filter_to_predicate:(fun () -> None)
        ~order_to_compare:(fun () -> Compare.Unchanged)
        (Incr.Var.watch map_var)
        (Incr.Var.watch params_var)
    in
    let incremental_observer =
      incremental_result |> Incr_map_collate.collated |> Incr.observe
    in
    { map_var; params_var; incremental_result; incremental_observer }
  ;;

  let get_incremental_result t =
    Incr.stabilize ();
    Incr.Observer.value_exn t.incremental_observer
  ;;
end

(* Quickcheck generators *)
module Generators = struct
  open Quickcheck.Generator.Let_syntax

  let small_string_gen =
    let%bind len = Int.gen_incl 1 3 in
    String.gen_with_length len Char.gen_lowercase
  ;;

  let small_map_gen =
    let%bind size = Int.gen_incl 0 10 in
    let%bind alist =
      Quickcheck.Generator.list_with_length
        size
        (Quickcheck.Generator.both small_string_gen (Int.gen_incl 0 100))
    in
    match String.Map.of_alist alist with
    | `Ok map -> return map
    | `Duplicate_key _ ->
      return
        (String.Map.of_alist_exn
           (List.dedup_and_sort alist ~compare:(fun (k1, _) (k2, _) ->
              String.compare k1 k2)))
  ;;

  let rank_gen =
    Quickcheck.Generator.union
      [ (let%map n = Int.gen_incl (-5) 10 in
         Collate_params.Rank.From_start n)
      ; (let%map n = Int.gen_incl (-5) 10 in
         Collate_params.Rank.From_end n)
      ]
  ;;

  let rank_range_gen =
    Quickcheck.Generator.weighted_union
      [ 2.0, return Collate_params.Which_range.All_rows
      ; ( 1.0
        , let%map x = rank_gen in
          Collate_params.Which_range.From x )
      ; ( 1.0
        , let%map x = rank_gen in
          Collate_params.Which_range.To x )
      ; ( 1.0
        , let%map x = rank_gen
          and y = rank_gen in
          Collate_params.Which_range.Between (x, y) )
      ]
  ;;

  let key_gen = String.quickcheck_generator

  let key_range_gen =
    Quickcheck.Generator.weighted_union
      [ 2.0, Quickcheck.Generator.return Collate_params.Which_range.All_rows
      ; ( 1.0
        , let%map k = key_gen in
          Collate_params.Which_range.From k )
      ; ( 1.0
        , let%map k = key_gen in
          Collate_params.Which_range.To k )
      ; ( 1.0
        , let%map k1 = key_gen
          and k2 = key_gen in
          Collate_params.Which_range.Between (k1, k2) )
      ]
  ;;

  let params_gen =
    let%map key_range = key_range_gen
    and rank_range = rank_range_gen in
    { Collate_params.filter = (); order = (); key_range; rank_range }
  ;;

  let operation_order_gen = Quickcheck.Generator.of_list [ `Filter_first; `Sort_first ]
end

(* The actual bisimulation test *)
let%test_unit "collate bisimulation (with bugs 1 and 2 fixed)" =
  Quickcheck.test
    ~sexp_of:
      [%sexp_of:
        [ `Filter_first | `Sort_first ]
        * Int.t String.Map.t
        * (String.t, unit, unit) Collate_params.t]
    (Quickcheck.Generator.tuple3
       Generators.operation_order_gen
       Generators.small_map_gen
       Generators.params_gen)
    ~f:(fun (operation_order, map, params) ->
      (* Run non-incremental version *)
      let non_incr_result =
        try
          Ok
            (Non_incremental.collate
               ~operation_order
               ~filter_to_predicate:(fun () -> None)
               ~order_to_compare:(fun () -> Compare.Unchanged)
               map
               params)
        with
        | exn -> Error exn
      in
      (* Run incremental version *)
      let incr_result =
        try
          let harness = Test_harness.create ~operation_order map params in
          Ok (Test_harness.get_incremental_result harness)
        with
        | exn -> Error exn
      in
      (* Compare results *)
      match non_incr_result, incr_result with
      | Ok non_incr, Ok incr ->
        (* Don't compare the Collated.t directly because of the indexes from
           [opaque_map]. [%test_result: (string, int) Collated.t] ~expect:non_incr incr; *)
        (* Compare the actual data *)
        [%test_result: (String.t * Int.t) list]
          ~expect:(Collated.to_alist non_incr)
          (Collated.to_alist incr);
        (* Compare metadata *)
        [%test_result: int]
          ~expect:(Collated.num_filtered_rows non_incr)
          (Collated.num_filtered_rows incr);
        [%test_result: int]
          ~expect:(Collated.num_unfiltered_rows non_incr)
          (Collated.num_unfiltered_rows incr);
        [%test_result: int]
          ~expect:(Collated.num_before_range non_incr)
          (Collated.num_before_range incr)
      | Error _, Error _ ->
        (* Both threw exceptions, which is fine *)
        ()
      | Ok _, Error exn ->
        raise_s
          [%message
            "Incremental version threw exception but non-incremental didn't" (exn : exn)]
      | Error exn, Ok _ ->
        raise_s
          [%message
            "Non-incremental version threw exception but incremental didn't" (exn : exn)])
;;
