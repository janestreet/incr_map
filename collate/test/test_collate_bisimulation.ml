open! Core
module Incr = Incremental.Make ()
open Incr_map_collate
module Filter = Quickcheck_generators.Filter

module Order = struct
  include Quickcheck_generators.Order

  let to_compare : t -> (string, int, String.comparator_witness) Compare.t = function
    | Unchanged -> Compare.Unchanged
    | Reversed_keys -> Compare.Reversed
    | Value_asc -> Compare.Custom_by_value { compare = Int.compare }
    | Value_desc -> Compare.Custom_by_value { compare = (fun a b -> Int.compare b a) }
  ;;
end

(* Test harness *)
module Test_harness = struct
  module Key = String
  module Value = Int

  type t =
    { map_var : Value.t Key.Map.t Incr.Var.t
    ; params_var : (Key.t, Filter.t, Order.t) Collate_params.t Incr.Var.t
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
        ~filter_equal:Filter.equal
        ~order_equal:Order.equal
        ~filter_to_predicate:Filter.to_predicate
        ~order_to_compare:Order.to_compare
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

(* The actual bisimulation test *)
let%test_unit "collate bisimulation (with bugs 1 and 2 fixed)" =
  Quickcheck.test
    ~sexp_of:
      [%sexp_of:
        [ `Filter_first | `Sort_first ]
        * Int.t String.Map.t
        * (String.t, Filter.t, Order.t) Collate_params.t]
    (Quickcheck.Generator.tuple3
       Quickcheck_generators.operation_order_gen
       Quickcheck_generators.small_map_gen
       Quickcheck_generators.params_gen)
    ~f:(fun (operation_order, map, params) ->
      (* Run non-incremental version *)
      let non_incr_result =
        try
          Ok
            (Incr_map_collate_non_incremental.collate
               ~operation_order
               ~filter_to_predicate:Filter.to_predicate
               ~order_to_compare:Order.to_compare
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
