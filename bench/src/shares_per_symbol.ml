open! Core
open! Import
module Symbol : Identifiable = String

module Dir = struct
  type t =
    | Buy
    | Sell
  [@@deriving equal]
end

module Order = struct
  module Id : Identifiable = String

  type t =
    { sym : Symbol.t
    ; size : int
    ; price : float
    ; dir : Dir.t
    ; id : Id.t
    }
end

let shares orders =
  Incr_map.unordered_fold
    orders
    ~init:0
    ~add:(fun ~key:_ ~data:(o : Order.t) acc -> acc + o.size)
    ~remove:(fun ~key:_ ~data:(o : Order.t) acc -> acc - o.size)
;;

let index_by inner_comparator outer_comparator map get_outer_index =
  let add ~key ~data acc =
    let idx = get_outer_index data in
    Map.update acc idx ~f:(function
      | None -> Map.singleton inner_comparator key data
      | Some inner_map -> Map.set inner_map ~key ~data)
  in
  let remove ~key ~data acc =
    let idx = get_outer_index data in
    Map.change acc idx ~f:(function
      | None -> assert false
      | Some inner_map ->
        let inner_map = Map.remove inner_map key in
        if Map.is_empty inner_map then None else Some inner_map)
  in
  Incr_map.unordered_fold map ~init:(Map.empty outer_comparator) ~add ~remove
;;

let%expect_test "index_by" =
  let open Expect_test_helpers_core in
  let v = Incr.Var.create (Map.empty (module Int)) in
  let o =
    index_by (module Int) (module String) (Incr.Var.watch v) String.uppercase
    |> Incr.observe
  in
  let change f =
    Incr.Var.set v (f (Incr.Var.value v));
    Incr.stabilize ();
    print_s [%sexp (Incr.Observer.value_exn o : string Map.M(Int).t Map.M(String).t)]
  in
  let add key data m = Map.set m ~key ~data in
  change (add 1 "bar");
  [%expect {| ((BAR ((1 bar)))) |}];
  change (add 1 "foo");
  [%expect {| ((FOO ((1 foo)))) |}];
  change (add 2 "foo");
  [%expect {|
    ((
      FOO (
        (1 foo)
        (2 foo)))) |}];
  change (add 3 "bar");
  [%expect {|
    ((BAR ((3 bar)))
     (FOO (
       (1 foo)
       (2 foo)))) |}];
  change (add 2 "bar");
  [%expect {|
    ((BAR (
       (2 bar)
       (3 bar)))
     (FOO ((1 foo)))) |}]
;;

let shares_per_symbol orders =
  let orders_by_symbol =
    index_by (module Order.Id) (module Symbol) orders (fun (o : Order.t) -> o.sym)
  in
  Incr_map.mapi' orders_by_symbol ~f:(fun ~key:_ ~data -> shares data)
;;

let shares_per_symbol_flat (orders : Order.t Map.M(Order.Id).t Incr.t) =
  let update_sym_map op ~key:_ ~data:(o : Order.t) m =
    Map.update m o.sym ~f:(function
      | None -> o.size
      | Some x -> op x o.size)
  in
  Incr_map.unordered_fold
    orders
    ~init:(Map.empty (module Symbol))
    ~add:(update_sym_map ( + ))
    ~remove:(update_sym_map ( - ))
;;

let random_order rstate : Order.t =
  let num_symbols = 100 in
  let sym = Symbol.of_string (sprintf "SYM-%2i" (Random.int num_symbols)) in
  let size = Random.State.int rstate 10_000 in
  let price = Random.State.int rstate 10_000 // 100 in
  let dir = if Random.State.bool rstate then Dir.Buy else Dir.Sell in
  let id = Order.Id.of_string (sprintf "ID-%i" (Random.State.int rstate Int.max_value)) in
  { sym; size; price; dir; id }
;;

let random_orders rstate n =
  List.init n ~f:(fun _ -> random_order rstate)
  |> List.fold
       ~init:(Map.empty (module Order.Id))
       ~f:(fun m o -> Map.set m ~key:o.id ~data:o)
;;

let shares_per_symbol_bench n shares_per_symbol_fn =
  let open Infix in
  let rstate = Random.State.make [| 1 |] in
  let init_orders = random_orders rstate n in
  let orders = Var.create init_orders in
  let shares = Incr.observe (shares_per_symbol_fn (Var.watch orders)) in
  fun () ->
    let o = random_order rstate in
    orders := Map.set init_orders ~key:o.id ~data:o;
    Incr.stabilize ();
    ignore (Obs.value_exn shares : int Map.M(Symbol).t)
;;

let%bench_fun "nested" = shares_per_symbol_bench 1_000_000 shares_per_symbol
let%bench_fun "flat" = shares_per_symbol_bench 1_000_000 shares_per_symbol_flat

(* Benchmark results:

   The flat version is better, but not massively better, than the original, showing that
   having a bunch of extra incremental nodes is expensive, but not horribly so.  (The
   memory numbers are obviously messed up, and I don't know why...)

   {v
┌───────────────────────────────┬──────────┬────────────┬──────────┬──────────┬────────────┐
│ Name                          │ Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────────────┼──────────┼────────────┼──────────┼──────────┼────────────┤
│ [shares_per_symbol.ml] nested │  16.96us │    363.42w │   39.34w │   39.34w │    100.00% │
│ [shares_per_symbol.ml] flat   │   6.16us │ -5_755.70w │   -2.56w │   -2.56w │     36.31% │
└───────────────────────────────┴──────────┴────────────┴──────────┴──────────┴────────────┘

      v}

*)
