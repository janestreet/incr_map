open Core
open Import

module Without_stabilize = struct
  type ('key, 'data) t =
    [ `Add of 'key * 'data [@quickcheck.weight 10.]
    | `Remove of 'key [@quickcheck.weight 4.]
    ]
  [@@deriving quickcheck, sexp_of]

  let apply t map =
    match t with
    | `Add (key, data) -> Map.set map ~key ~data
    | `Remove key -> Map.remove map key
  ;;

  let apply_nested t ~inner_map_comparator map =
    match t with
    | `Add (key, nested_operations) ->
      Map.update map key ~f:(fun inner_map ->
        List.fold
          nested_operations
          ~init:(Option.value inner_map ~default:(Map.empty inner_map_comparator))
          ~f:(fun inner_map t -> apply t inner_map))
    | `Remove key -> Map.remove map key
  ;;

  let add ~key ~data = `Add (key, data)
  let remove key = `Remove key
end

type ('key, 'data) t =
  [ `Stabilize
  | ('key, 'data) Without_stabilize.t
  ]
[@@deriving sexp_of]

let add = Without_stabilize.add
let remove = Without_stabilize.remove
let stabilize = `Stabilize

let int_key_gen ~keys_size =
  let open Quickcheck.Generator.Let_syntax in
  let%bind keys =
    let%bind len =
      match keys_size with
      | Some n -> return n
      | None -> Int.gen_incl 5 150
    in
    List.gen_with_length len Int.quickcheck_generator
    >>| List.dedup_and_sort ~compare:Int.compare
  in
  Quickcheck.Generator.of_list keys
;;

let quickcheck_generator_with_stabilize ~key_gen ?operations data_gen =
  let open Quickcheck.Generator.Let_syntax in
  let elt_gen =
    Quickcheck.Generator.weighted_union
      [ 1., return `Stabilize
      ; 14., Without_stabilize.quickcheck_generator key_gen data_gen
      ]
  in
  match operations with
  | None -> List.quickcheck_generator elt_gen
  | Some len -> List.gen_with_length len elt_gen
;;

let quickcheck_generator ?keys_size ?operations data_gen =
  quickcheck_generator_with_stabilize
    ~key_gen:(int_key_gen ~keys_size)
    ?operations
    data_gen
;;

let tuple_key_quickcheck_generator ?(keys_size = 12) ?operations data_gen =
  quickcheck_generator_with_stabilize
    ~key_gen:
      (Quickcheck.Generator.both
         (int_key_gen ~keys_size:(Some keys_size))
         (int_key_gen ~keys_size:(Some keys_size)))
    ?operations
    data_gen
;;

let inner_map_generator ~keys_size ~operations data_gen =
  let elt_gen =
    Without_stabilize.quickcheck_generator (int_key_gen ~keys_size) data_gen
  in
  match operations with
  | None -> List.quickcheck_generator elt_gen
  | Some len -> List.gen_with_length len elt_gen
;;

let nested_quickcheck_generator
  ?outer_map_keys_size
  ?outer_map_operations
  ?inner_map_keys_size
  ?inner_map_operations
  data_gen
  =
  quickcheck_generator
    ?keys_size:outer_map_keys_size
    ?operations:outer_map_operations
    (inner_map_generator
       ~keys_size:inner_map_keys_size
       ~operations:inner_map_operations
       data_gen)
;;

let run_operations_general ~apply operations ~into:var ~after_stabilize =
  let init = Incr.Var.latest_value var in
  List.fold operations ~init ~f:(fun map oper ->
    match oper with
    | #Without_stabilize.t as without_stabilize -> apply without_stabilize map
    | `Stabilize ->
      Incr.Var.set var map;
      Incr.stabilize ();
      after_stabilize ();
      map)
  |> ignore
;;

let run_operations operations ~into ~after_stabilize =
  run_operations_general ~apply:Without_stabilize.apply operations ~into ~after_stabilize
;;

let nested_run_operations operations ~inner_map_comparator ~into ~after_stabilize =
  run_operations_general
    ~apply:(Without_stabilize.apply_nested ~inner_map_comparator)
    operations
    ~into
    ~after_stabilize
;;
