open! Core
open Incremental.Let_syntax

module Key = struct
  include Bignum
  include Bignum.Unstable

  let to_string s =
    (* It's important that different numbers serialize to different strings, since the
       partial render table uses the serialization as a virtual-dom key in a context which
       requires that all the keys be unique. Thus, we use [to_string_accurate] to ensure
       no loss of precision. *)
    to_string_accurate s
  ;;

  module Stable = struct
    module V1 = Bignum.Stable.V3
  end
end

type 'a t = 'a Map.M(Key).t [@@deriving sexp, compare, equal, bin_io]

module Stable = struct
  module V1 = struct
    include%template
      Comparable.Stable.V1.With_stable_witness.Make [@mode portable] (struct
        type t = Bignum.Stable.V3.t [@@deriving bin_io, sexp, compare, stable_witness]
        type comparator_witness = Bignum.comparator_witness

        let comparator = Bignum.comparator
      end)

    type 'a t = 'a Map.t
    [@@deriving sexp, bin_io, compare, diff ~stable_version:1, stable_witness]

    let equal equal_v = Core.Map.equal equal_v

    module Diff = struct
      include Diff

      let map t ~f_value ~f_value_diff =
        List.map t ~f:(function
          | Diffable.Map_diff.Stable.V1.Change.Remove k_opaque ->
            Diffable.Map_diff.Stable.V1.Change.Remove k_opaque
          | Add (k_opaque, a) -> Add (k_opaque, f_value a)
          | Diff (k_opaque, a_diff) -> Diff (k_opaque, f_value_diff a_diff))
      ;;
    end
  end
end

module Diff = Stable.V1.Diff

let with_comparator x f =
  Incremental.bind (Incremental.freeze (Incremental.map x ~f:Map.comparator_s)) ~f
;;

let nearest map k =
  ( Map.closest_key map `Less_than k |> Option.map ~f:snd
  , Map.closest_key map `Greater_than k |> Option.map ~f:snd )
;;

(* To avoid a degenerate case when inserting where the insertions are sorted (and so the
   denominator doubles with every insertion) we can perform insertions in a more evenly
   distributed order where each subsequent insertions each bisect a previous pair of
   insertions. *)
let distribute_elements_evenly (ls : 'a list) : 'a list =
  let rec split_across ~(ls : 'a list) (acc1 : 'a list) (acc2 : 'a list) =
    match ls with
    | [] -> acc1, acc2
    | a :: [] -> a :: acc1, acc2
    | a :: b :: ls -> split_across ~ls (a :: acc1) (b :: acc2)
  in
  let rec traverse_layers ~(ls : 'a list) (acc : 'a list) : 'a list =
    if List.is_empty ls
    then acc
    else (
      let acc, ls = split_across ~ls acc [] in
      traverse_layers ~ls acc)
  in
  traverse_layers ~ls []
;;

let%expect_test "distribute_elements_evenly" =
  let ls = [] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf ()) |}];
  let ls = [ 0 ] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf (0)) |}];
  let ls = [ 1; 2; 3 ] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf (2 3 1)) |}];
  let ls = [ 1; 2; 3; 4 ] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf (2 4 3 1)) |}];
  let ls = [ 1; 2; 3; 4; 5; 6; 7 ] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf (4 2 6 7 5 3 1)) |}];
  let ls = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let bf = distribute_elements_evenly ls in
  print_s [%message (bf : int list)];
  [%expect {| (bf (8 4 2 6 10 9 7 5 3 1)) |}]
;;

let ( + ) = Bignum.( + )
let ( - ) = Bignum.( - )
let ( / ) = Bignum.( / )
let ( < ) = Bignum.( < )
let ( > ) = Bignum.( > )
let zero = Bignum.zero
let two = Bignum.one + Bignum.one
let denom_rebalance_cutoff = Bigint.of_int 100_000_000
let separation = Bignum.of_int 100

(** [key_between_bounds ~prev ~next] computes a key that falls between the optional [prev]
    and [next] bounds. Handles all cases:
    - No bounds: returns [zero]
    - Only [prev]: returns [prev + separation]
    - Only [next]: returns [next - separation]
    - Both bounds: returns midpoint, preferring integers when possible

    The function tries to use integer keys (via truncation) when possible to minimize
    denominator growth. *)
let key_between_bounds ~prev ~next =
  match prev, next with
  | None, None -> zero
  | None, Some next_key -> Bignum.truncate (next_key - separation)
  | Some prev_key, None -> Bignum.truncate (prev_key + separation)
  | Some prev_key, Some next_key ->
    let precise = (prev_key + next_key) / two in
    let truncated = Bignum.truncate precise in
    if truncated > prev_key && truncated < next_key then truncated else precise
;;

let erase_key_incrementally
  (type key data res cmp)
  ?data_equal
  (map : ((key, data, cmp) Map.t, 'w) Incremental.t)
  ~(get : key:key -> data:data -> res)
  : (res t, 'incr_witness) Incremental.t
  =
  let module Acc = struct
    type t =
      { key_to_bignum : (key, Bignum.t, cmp) Map.t
      ; out : res Bignum.Map.t
      ; comparator : (key, cmp) Comparator.Module.t
      ; additions : (key * data) list
      ; removals : key list
      ; rebalance_necessary : bool
      }

    let empty cmp =
      { key_to_bignum = Map.empty cmp
      ; out = Map.empty (module Bignum)
      ; comparator = cmp
      ; additions = []
      ; removals = []
      ; rebalance_necessary = false
      }
    ;;

    let of_maps cmp ~key_to_bignum ~out =
      { key_to_bignum
      ; out
      ; comparator = cmp
      ; additions = []
      ; removals = []
      ; rebalance_necessary = false
      }
    ;;

    let add ~key ~data ({ key_to_bignum; out; _ } as t) =
      let prev, next = nearest key_to_bignum key in
      let bignum = key_between_bounds ~prev ~next in
      let rebalance_necessary =
        t.rebalance_necessary
        || Bigint.(Bignum.den_as_bigint bignum > denom_rebalance_cutoff)
      in
      let key_to_bignum = Map.add_exn key_to_bignum ~key ~data:bignum in
      let out = Map.add_exn out ~key:bignum ~data:(get ~key ~data) in
      { t with key_to_bignum; out; rebalance_necessary }
    ;;

    let remove ~key ({ key_to_bignum; out; _ } as t) =
      let bignum = Map.find_exn key_to_bignum key in
      let key_to_bignum = Map.remove key_to_bignum key in
      let out = Map.remove out bignum in
      { t with key_to_bignum; out }
    ;;

    let update ~key ~data ({ key_to_bignum; out; _ } as t) =
      let bignum = Map.find_exn key_to_bignum key in
      let out = Map.set out ~key:bignum ~data:(get ~key ~data) in
      { t with key_to_bignum; out }
    ;;

    let add_all l acc =
      List.fold l ~init:acc ~f:(fun acc (key, data) -> add ~key ~data acc)
    ;;

    let process_removals_and_additions acc =
      let acc = List.fold acc.removals ~init:acc ~f:(fun acc key -> remove ~key acc) in
      let acc =
        let lower_than_lowest, middle, higher_than_highest =
          match Map.min_elt acc.key_to_bignum with
          | None -> [], [], acc.additions
          | Some (lowest, _) ->
            let highest, _ = Map.max_elt_exn acc.key_to_bignum in
            List.partition3_map acc.additions ~f:(fun addition ->
              let cmp = Comparator.compare (Comparator.of_module acc.comparator) in
              let key, _ = addition in
              if Int.(cmp key lowest < 0)
              then `Fst addition
              else if Int.(cmp key highest < 0)
              then `Snd addition
              else `Trd addition)
        in
        acc
        |> add_all lower_than_lowest
        |> add_all (distribute_elements_evenly middle)
        |> add_all (List.rev higher_than_highest)
      in
      { acc with removals = []; additions = [] }
    ;;

    let rebalance acc =
      let fresh = empty acc.comparator in
      let i = ref zero in
      let init = fresh.key_to_bignum, fresh.out in
      let key_to_bignum, out =
        Map.fold
          acc.key_to_bignum
          ~init
          ~f:(fun ~key ~data:prev_bignum (key_to_bignum, out) ->
            let prev_res = Map.find_exn acc.out prev_bignum in
            let k = !i in
            i := k + separation;
            Map.add_exn key_to_bignum ~key ~data:k, Map.add_exn out ~key:k ~data:prev_res)
      in
      of_maps acc.comparator ~key_to_bignum ~out
    ;;

    let finalize acc =
      let acc = process_removals_and_additions acc in
      if acc.rebalance_necessary then rebalance acc else acc
    ;;
  end
  in
  let%pattern_bind { Acc.out; _ } =
    with_comparator map (fun cmp ->
      Incr_map.unordered_fold
        ?data_equal
        ~init:(Acc.empty cmp)
        ~specialized_initial:(fun ~init data ->
          let i = ref zero in
          let init = init.key_to_bignum, init.out in
          let key_to_bignum, out =
            Map.fold data ~init ~f:(fun ~key ~data (key_to_bignum, out) ->
              let k = !i in
              i := k + separation;
              ( Map.add_exn key_to_bignum ~key ~data:k
              , Map.add_exn out ~key:k ~data:(get ~key ~data) ))
          in
          Acc.of_maps cmp ~key_to_bignum ~out)
        ~add:(fun ~key ~data acc -> { acc with additions = (key, data) :: acc.additions })
        ~remove:(fun ~key ~data:_ acc -> { acc with removals = key :: acc.removals })
        ~update:(fun ~key ~old_data:_ ~new_data:data acc -> Acc.update ~key ~data acc)
        ~finalize:Acc.finalize
        map)
  in
  out
;;

let empty = Bignum.Map.empty

let init len ~f =
  Map.of_increasing_iterator_unchecked (module Bignum) ~len ~f:(fun i ->
    Bignum.(of_int i * separation), f i)
;;

let of_list xs =
  Bignum.Map.of_alist_exn (List.mapi xs ~f:(fun i x -> Bignum.(of_int i * separation), x))
;;

let of_array arr =
  Bignum.Map.of_sorted_array_unchecked
    (Array.mapi arr ~f:(fun i x -> Bignum.(of_int i * separation), x))
;;

let of_sequence seq =
  Bignum.Map.of_increasing_sequence
    (Sequence.mapi seq ~f:(fun i x -> Bignum.(of_int i * separation), x))
  |> Or_error.ok_exn
;;

let insert_before map ~key elem =
  let prev = Map.closest_key map `Less_than key |> Option.map ~f:fst in
  let new_key = key_between_bounds ~prev ~next:(Some key) in
  Map.add_exn map ~key:new_key ~data:elem
;;

let insert_after map ~key elem =
  let next = Map.closest_key map `Greater_than key |> Option.map ~f:fst in
  let new_key = key_between_bounds ~prev:(Some key) ~next in
  Map.add_exn map ~key:new_key ~data:elem
;;

let append map elem =
  match Map.max_elt map with
  | None -> Key.Map.singleton Key.zero elem
  | Some (max_key, _) -> insert_after map ~key:max_key elem
;;

let prepend map elem =
  match Map.min_elt map with
  | None -> Key.Map.singleton Key.zero elem
  | Some (min_key, _) -> insert_before map ~key:min_key elem
;;
