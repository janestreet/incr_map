open! Core
open Import

(* In this section, we're doing a summation over nested maps containing floats in the
   inner maps. *)

let sum_map = Sum.sum_map
let sum_map' m = sum_map (Incr_map.mapi' m ~f:(fun ~key:_ ~data -> sum_map data))

(* Sets an element in the inner map, given both outer and inner index *)
let set_el m o i v =
  Map.update m o ~f:(function
    | None -> Map.singleton (module Int) i v
    | Some m' -> Map.set m' ~key:i ~data:v)
;;

(* Initializes a map full of zeros for all combinations of inner and outer indices *)
let initialize ~outer ~inner =
  Sequence.fold
    (Sequence.range 0 outer)
    ~init:(Map.empty (module Int))
    ~f:(fun acc o ->
      Sequence.fold (Sequence.range 0 inner) ~init:acc ~f:(fun acc i -> set_el acc o i 0.))
;;

module Sum_map_direct = struct
  (* From the Ocaml manual, chapter 20:

     "As an optimization, records whose fields all have static type float are
     represented as arrays of floating-point numbers, with tag Double_array_tag."

     This means that a mutable float member of such an record type is effectively
     unboxed (other than the record itself), and in particular can be mutated in-place
     without allocation. Interestingly, this optimization is not, as of 4.07, applied
     to the type [float ref].

     On the other hand, a [float ref] which does not escape a single scope can be
     lowered to a register, so this trick isn't necessary if we're (say) for-looping
     over an array. But in any case where we have to use a ref inside a closure we're
     far better off using this type. Similarly, we can't return a float in %xmm
     registers. So the obvious [Map.fold] here actually allocates one box per map
     entry, so we're better off using a ref.

     *)
  type float_ref = { mutable contents : float }

  let float_ref contents = { contents }
  let ( := ) ref x = ref.contents <- x
  let ( ! ) ref = ref.contents
  let acc = float_ref 0.0

  let sum m ~get =
    acc := 0.0;
    Map.iter m ~f:(fun v -> acc := !acc +. get v);
    !acc
  ;;
end

module M (M : sig
    val outer : int
    val inner : int
  end) =
struct
  let outer = M.outer
  let inner = M.inner
  let suffix = sprintf "(%d, %d)" outer inner

  (* Exposed for testing. Computes the nested sum incrementally, using [sum_map']. *)
  let nested_sum_raw () =
    let open Infix in
    let input = Var.create (initialize ~outer ~inner) in
    let sum = Incr.observe (sum_map' (Var.watch input)) in
    fun () ->
      let o = Random.int outer in
      let i = Random.int inner in
      input := set_el !input o i (Random.float 1.0);
      Incr.stabilize ();
      ignore (Obs.value_exn sum : float)
  ;;

  let%bench_module (""[@name_suffix suffix]) =
    (module struct
      let%bench_fun "incr" = nested_sum_raw ()

      (* Compute the outer sum incrementally using [sum_map], but do the inner sum
         all-at-once. *)
      let%bench_fun "out" =
        let open Infix in
        let input = Var.create (initialize ~outer ~inner) in
        let inner_summed =
          Incr_map.mapi (Var.watch input) ~f:(fun ~key:_ ~data ->
            Sum_map_direct.sum data ~get:Fn.id)
        in
        let sum = Incr.observe (sum_map inner_summed) in
        fun () ->
          let o = Random.int outer in
          let i = Random.int inner in
          input := set_el !input o i (Random.float 1.0);
          Incr.stabilize ();
          ignore (Obs.value_exn sum : float)
      ;;

      (* Does the nested sum in an all-at-once way, using ordinary map folds *)
      let%bench_fun "ord" =
        let input = ref (initialize ~outer ~inner) in
        let sum () = Sum_map_direct.sum !input ~get:(Sum_map_direct.sum ~get:Fn.id) in
        fun () ->
          let o = Random.int outer in
          let i = Random.int inner in
          input := set_el !input o i (Random.float 1.0);
          ignore (sum ())
      ;;
    end)
  ;;
end

module M1 = M (struct
    let outer = 10000
    let inner = 10
  end)

module M2 = M (struct
    let outer = 1000
    let inner = 100
  end)

module M3 = M (struct
    let outer = 100
    let inner = 1000
  end)

module M4 = M (struct
    let outer = 10
    let inner = 10000
  end)

(* Looks like it doesn't matter all that much how you choose to structure the nested maps,
   though more outer elements clearly adds expense.  Note also that despite the decently
   large cost of the incremental update, the all-at-once computation is ~200-300x worse.
   {v
┌────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬───────┐
│ Name                               │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │  %age │
├────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼───────┤
│ [nested_sum.ml:M:(10, 10000)] incr │   2.32us │ 277.17w │   24.88w │   24.88w │ 0.26% │
│ [nested_sum.ml:M:(100, 1000)] incr │   2.73us │ 296.97w │   25.81w │   25.81w │ 0.31% │
│ [nested_sum.ml:M:(1000, 100)] incr │   3.18us │ 325.29w │   30.82w │   30.83w │ 0.36% │
│ [nested_sum.ml:M:(10000, 10)] incr │   4.93us │ 358.42w │   44.97w │   45.00w │ 0.56% │
│ [nested_sum.ml:M:(10, 10000)] ord  │ 669.97us │ 160.35w │   34.33w │   34.33w │ 76.3% │
│ [nested_sum.ml:M:(100, 1000)] ord  │ 804.13us │ 155.24w │   16.73w │   16.73w │ 91.6% │
│ [nested_sum.ml:M:(1000, 100)] ord  │ 769.96us │ 154.80w │   15.60w │   15.60w │ 87.7% │
│ [nested_sum.ml:M:(10000, 10)] ord  │ 877.80us │ 160.23w │   21.29w │   21.29w │  100% │
│ [nested_sum.ml:M:(10, 10000)] out  │  80.39us │ 245.98w │   24.79w │   24.79w │ 9.16% │
│ [nested_sum.ml:M:(100, 1000)] out  │  12.60us │ 265.92w │   24.87w │   24.87w │ 1.44% │
│ [nested_sum.ml:M:(1000, 100)] out  │   3.75us │ 294.54w │   28.27w │   28.27w │ 0.43% │
│ [nested_sum.ml:M:(10000, 10)] out  │   3.17us │ 330.02w │   39.67w │   39.67w │ 0.36% │
└────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴───────┘

   v}
*)

let%expect_test "stats" =
  let stats = unstage (Stats.reporter ()) in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    0)) |}];
  let run = M2.nested_sum_raw () in
  stats ();
  [%expect {|
    ((recomputed 0)
     (changed    0)
     (created    6)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 2008)
     (changed    2008)
     (created    2002)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 7)
     (changed    6)
     (created    0)) |}];
  run ();
  stats ();
  [%expect {|
    ((recomputed 7)
     (changed    6)
     (created    0)) |}]
;;
