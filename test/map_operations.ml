open Core
open Import

type 'a t =
  | Stabilize
  | Add of int * 'a
  | Remove of int
[@@deriving sexp_of]

let quickcheck_generator ?keys_size ?operations data_gen =
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
  let key_gen = Quickcheck.Generator.of_list keys in
  let elt_gen =
    Quickcheck.Generator.weighted_union
      [ 1., return Stabilize
      ; ( 4.
        , let%map key = key_gen in
          Remove key )
      ; ( 10.
        , let%map key = key_gen
          and data = data_gen in
          Add (key, data) )
      ]
  in
  match operations with
  | None -> List.quickcheck_generator elt_gen
  | Some len -> List.gen_with_length len elt_gen
;;

let run_operations operations ~into:var ~after_stabilize =
  let init = Incr.Var.latest_value var in
  List.fold operations ~init ~f:(fun map oper ->
    match oper with
    | Add (key, data) -> Map.set map ~key ~data
    | Remove key -> Map.remove map key
    | Stabilize ->
      Incr.Var.set var map;
      Incr.stabilize ();
      after_stabilize ();
      map)
  |> ignore
;;
