open Core_kernel

type 'a t = 'a Int63.Map.t [@@deriving sexp, equal, compare, bin_io]

let with_old i ~f =
  let old = ref None in
  let%map.Incremental a = i in
  let b = f ~old:!old a in
  old := Some (a, b);
  b
;;

exception Needs_rebalance
exception Needs_rebalance_overflow

let of_map
      (type res)
      ?(data_equal = phys_equal)
      ?(separation = Int63.of_int 100)
      (map : (('a, 'b, 'cmp) Map.t, 'incr_witness) Incremental.t)
      ~(get : key:'a -> data:'b -> res)
  : (res t, 'incr_witness) Incremental.t
  =
  let open Incremental.Let_syntax in
  let add_exn here map ~key ~data =
    match Map.add map ~key ~data with
    | `Ok map -> map
    | `Duplicate ->
      raise_s
        [%message
          "BUG in Incr_map_collate.Map_list - duplicate while adding to a map"
            ~key:((Map.comparator map).sexp_of_t key : Sexp.t)
            (here : Source_code_position.t)]
  in
  let find_exn here map key =
    match Map.find map key with
    | Some x -> x
    | None ->
      raise_s
        [%message
          "BUG in Incr_map_collate.Map_list - key noy found"
            ~key:((Map.comparator map).sexp_of_t key : Sexp.t)
            (here : Source_code_position.t)]
  in
  let from_scratch map ~separation =
    let key_to_pos, pos_to_res, _ =
      Map.fold
        map
        ~init:(Map.empty (Map.comparator_s map), Int63.Map.empty, Int63.zero)
        ~f:(fun ~key ~data (key_to_pos, pos_to_res, next_pos) ->
          ( add_exn [%here] key_to_pos ~key ~data:next_pos
          , add_exn [%here] pos_to_res ~key:next_pos ~data:(get ~key ~data)
          , Int63.(next_pos + separation) ))
    in
    key_to_pos, pos_to_res
  in
  with_old map ~f:(fun ~old new_in ->
    match old with
    | None -> from_scratch new_in ~separation
    | Some (old_in, (old_key_to_pos, old_pos_to_res)) ->
      (try
         Map.fold_symmetric_diff
           ~data_equal
           old_in
           new_in
           ~init:(old_key_to_pos, old_pos_to_res)
           ~f:(fun (key_to_pos, pos_to_res) (key, change) ->
             match change with
             | `Left _old ->
               let pos = find_exn [%here] key_to_pos key in
               Map.remove key_to_pos key, Map.remove pos_to_res pos
             | `Right new_ ->
               let below_pos = Map.closest_key key_to_pos `Less_or_equal_to key in
               let above_pos = Map.closest_key key_to_pos `Greater_or_equal_to key in
               let new_pos =
                 (* Note: this code needs to take integer overflows into account *)
                 let open Int63.O in
                 match below_pos, above_pos with
                 (* we found values both above and below the location that we would be inserting into *)
                 | Some (_, below_pos), Some (_, above_pos) ->
                   let v = below_pos + ((above_pos - below_pos) / Int63.of_int 2) in
                   if v <= below_pos || v >= above_pos
                   then Exn.raise_without_backtrace Needs_rebalance
                   else v
                 (* we found an entry below our value, so add this one on top of it *)
                 | Some (_, below_pos), None ->
                   let v = below_pos + separation in
                   if v <= below_pos
                   then
                     (* an overflow occurred *)
                     Exn.raise_without_backtrace Needs_rebalance_overflow
                   else v
                 (* we found an entry above our value, so add this one below it *)
                 | None, Some (_, above_pos) ->
                   let v = above_pos - separation in
                   if v >= above_pos
                   then
                     (* an underflow occurred *)
                     Exn.raise_without_backtrace Needs_rebalance_overflow
                   else v
                 | None, None -> Int63.zero
               in
               let pos_to_res =
                 match Map.add pos_to_res ~key:new_pos ~data:(get ~key ~data:new_) with
                 | `Ok pos_to_res -> pos_to_res
                 | `Duplicate ->
                   (* I believe this case would represent a bug, since [new_pos] is
                      strictly between [below_pos] and [above_pos] (when they exist), and
                      so it should not exist in [pos_to_res] yet if our invariants hold.
                      But, it seems likely that I'm overlooking something, so it makes
                      sense to just gracefully rebalance. *)
                   Exn.raise_without_backtrace Needs_rebalance
               in
               add_exn [%here] key_to_pos ~key ~data:new_pos, pos_to_res
             | `Unequal (_old, new_) ->
               let pos = find_exn [%here] key_to_pos key in
               key_to_pos, Map.set pos_to_res ~key:pos ~data:(get ~key ~data:new_))
       with
       | Needs_rebalance -> from_scratch new_in ~separation
       | Needs_rebalance_overflow ->
         from_scratch
           new_in
           ~separation:
             Int63.(if separation > of_int 10 then separation / of_int 2 else separation)))
  >>| snd
;;
