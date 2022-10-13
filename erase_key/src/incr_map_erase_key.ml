open! Core
open Incremental.Let_syntax

module Key = struct
  include Bignum
  include Bignum.Unstable

  let to_string s =
    (* It's important that different numbers serialize to different strings,
       since the partial render table uses the serialization as a virtual-dom
       key in a context which requires that all the keys be unique. Thus, we
       use [to_string_accurate] to ensure no loss of precision. *)
    to_string_accurate s
  ;;

  module Stable = struct
    module V1 = Bignum.Stable.V3
  end
end

type 'a t = 'a Map.M(Key).t [@@deriving sexp, compare, equal, bin_io]

let with_comparator x f =
  Incremental.bind (Incremental.freeze (Incremental.map x ~f:Map.comparator_s)) ~f
;;

let nearest map k =
  ( Map.closest_key map `Less_than k |> Option.map ~f:snd
  , Map.closest_key map `Greater_than k |> Option.map ~f:snd )
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

let erase
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
      ; out = Bignum.Map.empty
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
      let bignum =
        match nearest key_to_bignum key with
        | None, None -> zero
        | None, Some lowest ->
          (* Round to a nearby integer so that we don't retain the
             potentially large fractional part of the lowest key.
             We assume that [separation > 1] so that we don't round to a
             number greater than [lowest]. *)
          Bignum.truncate (lowest - separation)
        | Some highest, None -> Bignum.truncate (highest + separation)
        | Some low, Some high ->
          let precise = (low + high) / two in
          let truncated = Bignum.truncate precise in
          if truncated > low && truncated < high then truncated else precise
      in
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

    let process_removals_and_additions acc =
      let acc = List.fold acc.removals ~init:acc ~f:(fun acc key -> remove ~key acc) in
      let acc =
        List.fold (List.rev acc.additions) ~init:acc ~f:(fun acc (key, data) ->
          add ~key ~data acc)
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
        ~add:(fun ~key ~data acc ->
          { acc with additions = (key, data) :: acc.additions })
        ~remove:(fun ~key ~data:_ acc -> { acc with removals = key :: acc.removals })
        ~update:(fun ~key ~old_data:_ ~new_data:data acc -> Acc.update ~key ~data acc)
        ~finalize:Acc.finalize
        map)
  in
  out
;;

module For_testing = struct
  let of_list xs =
    Bignum.Map.of_alist_exn (List.mapi xs ~f:(fun i x -> Bignum.of_int i, x))
  ;;
end
