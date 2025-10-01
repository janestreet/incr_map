open! Core
open Incr_map_collate
module Generator = Base_quickcheck.Generator
open Generator.Let_syntax

module Filter = struct
  type t =
    | No_filter
    | Key_has_prefix of string
    | Value_is_even
    | Value_is_odd
  [@@deriving sexp, compare]

  let equal (x : t) (y : t) = [%compare.equal: t] x y

  let to_predicate (t : t) : (key:string -> data:int -> bool) option =
    match t with
    | No_filter -> None
    | Key_has_prefix prefix -> Some (fun ~key ~data:_ -> String.is_prefix key ~prefix)
    | Value_is_even -> Some (fun ~key:_ ~data -> Int.(data % 2 = 0))
    | Value_is_odd -> Some (fun ~key:_ ~data -> Int.(data % 2 <> 0))
  ;;
end

module Order = struct
  type t =
    | Unchanged
    | Reversed_keys
    | Value_asc
    | Value_desc
  [@@deriving sexp, compare]

  let equal (x : t) (y : t) = [%compare.equal: t] x y
end

let small_string_gen =
  let%bind len = Int.gen_incl 1 3 in
  String.gen_with_length len Char.gen_lowercase
;;

let small_map_gen =
  let%bind size = Int.gen_incl 0 10 in
  let%bind alist =
    Base_quickcheck.Generator.list_with_length
      ~length:size
      (Base_quickcheck.Generator.both small_string_gen (Int.gen_incl 0 100))
  in
  match String.Map.of_alist alist with
  | `Ok map -> Generator.return map
  | `Duplicate_key _ ->
    Generator.return
      (String.Map.of_alist_exn
         (List.dedup_and_sort alist ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)))
;;

let rank_gen =
  Generator.union
    [ (let%map n = Int.gen_incl (-5) 10 in
       Collate_params.Rank.From_start n)
    ; (let%map n = Int.gen_incl (-5) 10 in
       Collate_params.Rank.From_end n)
    ]
;;

let rank_range_gen =
  Generator.weighted_union
    [ 2.0, Generator.return Collate_params.Which_range.All_rows
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
  Generator.weighted_union
    [ 2.0, Generator.return Collate_params.Which_range.All_rows
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

let filter_gen =
  Generator.weighted_union
    [ 2.0, Generator.return Filter.No_filter
    ; ( 1.0
      , let%map p = small_string_gen in
        Filter.Key_has_prefix p )
    ; 1.0, Generator.return Filter.Value_is_even
    ; 1.0, Generator.return Filter.Value_is_odd
    ]
;;

let order_gen =
  Generator.weighted_union
    [ 2.0, Generator.return Order.Unchanged
    ; 1.0, Generator.return Order.Reversed_keys
    ; 1.0, Generator.return Order.Value_asc
    ; 1.0, Generator.return Order.Value_desc
    ]
;;

let params_gen =
  let%map key_range = key_range_gen
  and rank_range = rank_range_gen
  and filter = filter_gen
  and order = order_gen
  and widen_before = Int.gen_incl (-2) 5
  and widen_after = Int.gen_incl (-2) 5 in
  { Collate_params.filter
  ; order
  ; key_range
  ; rank_range
  ; widen_range_by = widen_before, widen_after
  }
;;

let operation_order_gen = Generator.of_list [ `Filter_first; `Sort_first ]
