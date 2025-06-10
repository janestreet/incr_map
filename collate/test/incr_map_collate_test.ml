open! Core
open Expect_test_helpers_core
module Incr = Incremental.Make ()
open Incr_map_collate
module Key = String (* "symbol" *)

module Value = struct
  type t = int * float (* "size" and "price" *) [@@deriving sexp, bin_io, equal, compare]
end

module Concrete = Collated.Make_concrete (Key) (Value)

module Order = struct
  module T = struct
    type t =
      | By_symbol
      | By_symbol_reversed
      | By_price
      | By_price_reversed
      | By_price_and_key
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_compare = function
    | By_symbol -> Compare.Unchanged
    | By_symbol_reversed -> Reversed
    | By_price ->
      Custom_by_value { compare = (fun a b -> Comparable.lift ~f:snd Float.compare a b) }
    | By_price_reversed ->
      Custom_by_value
        { compare =
            (fun a b ->
              Comparable.reverse (fun a b -> Comparable.lift ~f:snd Float.compare a b) a b)
        }
    | By_price_and_key ->
      let compare (* By "price" first, then by key descending *) a_1 b_1 =
        Comparable.lexicographic
          [ (fun a b -> Comparable.lift Float.compare ~f:(fun (_, (_, x)) -> x) a b)
          ; (fun a b -> Comparable.lift String.descending ~f:fst a b)
          ]
          a_1
          b_1
      in
      Custom_by_key_and_value { compare }
  ;;
end

module Filter = struct
  module T = struct
    type t =
      | None
      | True
      | Key_has_vowel
    [@@deriving hash, compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_predicate = function
    | None -> (None : _ option)
    | True -> Some (fun ~key:_ ~data:_ -> true)
    | Key_has_vowel ->
      Some
        (fun ~key ~data:_ ->
          String.exists (String.lowercase key) ~f:(fun ch ->
            List.exists [ 'a'; 'e'; 'i'; 'o'; 'u' ] ~f:(Char.equal ch)))
  ;;
end

type t =
  { map : Value.t Key.Map.t Incr.Var.t
  ; collate : (Key.t, Filter.t, Order.t) Collate_params.t Incr.Var.t
  ; observer : Concrete.t Incr.Observer.t
  }

let get_res t =
  Incr.stabilize ();
  Incr.Observer.value_exn t.observer
;;

let print_res ?(full_sexp = false) t =
  let res = get_res t in
  if full_sexp
  then print_s [%sexp (res : Concrete.t)]
  else
    print_s
      [%message
        ""
          ~_:(Collated.to_alist res : (Key.t * Value.t) list)
          ~num_filtered_rows:(Collated.num_filtered_rows res : int)
          ~num_unfiltered_rows:(Collated.num_unfiltered_rows res : int)]
;;

let modify_map t ~f = Incr.Var.set t.map (f (Incr.Var.value t.map))

let set_collate ?filter ?rank_range ?key_range ?order t =
  let collate = Incr.Var.value t.collate in
  let collate =
    { Collate_params.filter = Option.value filter ~default:collate.filter
    ; key_range = Option.value key_range ~default:collate.key_range
    ; rank_range = Option.value rank_range ~default:collate.rank_range
    ; order = Option.value order ~default:collate.order
    }
  in
  Incr.Var.set t.collate collate
;;

let do_collate_default ?operation_order input collate =
  Incr_map_collate.collate
    ?operation_order
    ~filter_to_predicate:Filter.to_predicate
    ~order_to_compare:Order.to_compare
    ~filter_equal:Filter.equal
    ~order_equal:Order.equal
    input
    collate
;;

let init_test
  ?(data = [ "AAPL", (10, 1.0); "GOOG", (10, 3.0); "VOD", (10, 2.0) ])
  ?operation_order
  ?(filter = Filter.None)
  ?(order = Order.By_symbol)
  ?(key_range = Collate_params.Which_range.All_rows)
  ?(rank_range = Collate_params.Which_range.All_rows)
  ?(do_collate = do_collate_default)
  ()
  =
  let initial = Key.Map.of_alist_exn data in
  let map = Incr.Var.create initial in
  let collate =
    Incr.Var.create ({ filter; order; key_range; rank_range } : _ Collate_params.t)
  in
  let observer =
    let collated =
      do_collate ?operation_order (Incr.Var.watch map) (Incr.Var.watch collate)
      |> Incr_map_collate.collated
    in
    Incr.observe collated
  in
  (* Make sure that our math works out *)
  Incr.Observer.on_update_exn observer ~f:(function
    | Incr.Observer.Update.Invalidated -> ()
    | Incr.Observer.Update.Initialized a | Incr.Observer.Update.Changed (_, a) ->
      let total = Incr_map_collate.Collated.num_filtered_rows a in
      let before = Incr_map_collate.Collated.num_before_range a in
      let inside = Incr_map_collate.Collated.length a in
      let after = Incr_map_collate.Collated.num_after_range a in
      assert (before + after + inside = total));
  { map; collate; observer }
;;

let%expect_test "full range, no sort, no filter" =
  let t = init_test () in
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (FB   (10 4))
      (GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 4))
    |}]
;;

let%expect_test "full range, no sort, filter" =
  let t = init_test ~filter:Filter.Key_has_vowel () in
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (EEE  (10 0))
      (GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "full range, sort, filter" =
  let t = init_test ~filter:Filter.Key_has_vowel ~order:Order.By_price () in
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  [%expect
    {|
    (((EEE  (10 0))
      (AAPL (10 1))
      (VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "second & third, sort, filter" =
  let t =
    init_test
      ~filter:Filter.Key_has_vowel
      ~order:Order.By_price
      ~rank_range:(Collate_params.Which_range.Between (From_start 1, From_start 2))
      ()
  in
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "second last & last, sort, filter" =
  let t =
    init_test
      ~filter:Filter.Key_has_vowel
      ~order:Order.By_price
      ~rank_range:(Collate_params.Which_range.Between (From_end 1, From_end 0))
      ()
  in
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  (* We expect to see the same last 2 results because the insertion was made before *)
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 5))
    |}];
  modify_map t ~f:(Map.set ~key:"EEE" ~data:(10, 4.0));
  print_res t;
  (* We expect to see EEE moved at the end *)
  [%expect
    {|
    (((GOOG (10 3))
      (EEE  (10 4)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "changing range" =
  let t =
    init_test
      ~order:Order.By_price
      ~rank_range:(Collate_params.Which_range.Between (From_start 1, From_start 2))
      ()
  in
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  set_collate ~rank_range:(Between (From_start 3, From_start 4)) t;
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (FB   (10 4)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "changing to from_end and mixed ranges" =
  let t =
    init_test
      ~order:Order.By_price
      ~rank_range:(Collate_params.Which_range.Between (From_start 1, From_start 2))
      ()
  in
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 0.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  set_collate ~rank_range:(Between (From_end 1, From_end 0)) t;
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (FB   (10 4)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  (* This is the same range as above but we mix From_start/From_end indices*)
  set_collate ~rank_range:(Between (From_start 3, From_end 0)) t;
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (FB   (10 4)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  set_collate ~rank_range:(Between (From_end 1, From_start 4)) t;
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (FB   (10 4)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "changing range" =
  let t =
    init_test
      ~data:
        [ "AAPL", (10, 1.0)
        ; "GOOG", (10, 3.0)
        ; "Foo", (10, 3.0)
        ; "bar", (10, 3.0)
        ; "baz", (10, 3.0)
        ; "buz", (10, 3.0)
        ; "whip", (10, 3.0)
        ; "VOD", (10, 2.0)
        ]
      ~rank_range:(Collate_params.Which_range.Between (From_start 1, From_start 5))
      ()
  in
  let res1 = get_res t in
  set_collate ~rank_range:(Between (From_start 2, From_start 6)) t;
  let res2 = get_res t in
  let update = Concrete.diffs ~from:res1 ~to_:res2 in
  print_s [%sexp (update : Concrete.Update.t)];
  [%expect
    {|
    ((Elements_prior_to_range 2)
     (Rank_range (Between 2 6))
     (Data (Add 500 (buz (10 3))))
     (Data (Remove 0)))
    |}]
;;

let%expect_test "changing sort" =
  let t = init_test () in
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  set_collate ~order:Order.By_price t;
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  set_collate ~order:Order.By_price_reversed t;
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (VOD  (10 2))
      (AAPL (10 1)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}]
;;

let%expect_test "no sort, no filter, key range" =
  let t = init_test ~key_range:(From "GOOG") () in
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((GOOG (10 3))
      (VOD  (10 2)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 4))
    |}]
;;

let%expect_test "sort, no filter, key range" =
  let t = init_test ~order:Order.By_price ~key_range:(From "VOD") () in
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3))
      (FB   (10 4)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 4))
    |}]
;;

let%expect_test "no sort, no filter, key range + rank range" =
  let t =
    init_test
      ~key_range:(From "GOOG")
      ~rank_range:(Between (From_start 0, From_start 0))
      ()
  in
  print_res t;
  [%expect
    {|
    (((GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((GOOG (10 3)))
     (num_filtered_rows   4)
     (num_unfiltered_rows 4))
    |}]
;;

let%expect_test "reversed key sort" =
  let t = init_test ~order:Order.By_symbol_reversed () in
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3))
      (AAPL (10 1)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}]
;;

let%expect_test "sort by key & value" =
  let t = init_test ~order:Order.By_price_and_key () in
  modify_map t ~f:(Map.add_exn ~key:"FB" ~data:(10, 3.0));
  modify_map t ~f:(Map.add_exn ~key:"EEE" ~data:(10, 2.0));
  print_res t;
  [%expect
    {|
    (((AAPL (10 1))
      (VOD  (10 2))
      (EEE  (10 2))
      (GOOG (10 3))
      (FB   (10 3)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "sort by key, update values" =
  (* This test is important, as the implementation stores a map keyed by 'k * 'v.
     It's crucial that it gets updated correctly - we got this wrong once already. *)
  let t =
    init_test
      ~operation_order:`Sort_first
      ~order:Order.By_symbol_reversed
      ~filter:Filter.True
      ()
  in
  modify_map t ~f:(Map.set ~key:"AAPL" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3))
      (AAPL (10 4)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.set ~key:"AAPL" ~data:(10, 1.0));
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3))
      (AAPL (10 1)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  modify_map t ~f:(Map.set ~key:"AAPL" ~data:(10, 4.0));
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3))
      (AAPL (10 4)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}]
;;

let%expect_test "update values so that they compare equal" =
  let t =
    init_test
      ~operation_order:`Sort_first
      ~order:Order.By_price
      ~filter:Filter.True
      ~key_range:(From "VOD")
      ~rank_range:(Between (From_start 0, From_start 100))
      ()
  in
  print_res t;
  [%expect
    {|
    (((VOD  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  (* Modify to something that compares equal *)
  modify_map t ~f:(Map.set ~key:"VOD" ~data:(11, 2.0));
  print_res t;
  [%expect
    {|
    (((VOD  (11 2))
      (GOOG (10 3)))
     (num_filtered_rows   3)
     (num_unfiltered_rows 3))
    |}];
  (* Add entries with values that compares equal (but not keys, we don't want
     duplicates!) *)
  modify_map t ~f:(Map.set ~key:"AAA" ~data:(10, 2.0));
  modify_map t ~f:(Map.set ~key:"ZZZ" ~data:(10, 2.0));
  print_res t;
  [%expect
    {|
    (((VOD  (11 2))
      (ZZZ  (10 2))
      (GOOG (10 3)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  (* Modify to bigger value *)
  modify_map t ~f:(Map.set ~key:"VOD" ~data:(11, 2.5));
  print_res t;
  [%expect
    {|
    (((VOD  (11 2.5))
      (GOOG (10 3)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}];
  (* Move GOOG to ensure it gets compared to the new value of VOD *)
  modify_map t ~f:(Map.set ~key:"GOOG" ~data:(10, 2.3));
  print_res t;
  [%expect
    {|
    (((VOD (11 2.5)))
     (num_filtered_rows   5)
     (num_unfiltered_rows 5))
    |}]
;;

let%expect_test "don't trigger rebalance" =
  let t = init_test ~data:[ "A", (0, 0.); "B", (0, 100.) ] () in
  print_res ~full_sexp:true t;
  [%expect
    {|
    ((data (
       (0   (A (0 0)))
       (100 (B (0 100)))))
     (num_filtered_rows   2)
     (key_range           All_rows)
     (rank_range          All_rows)
     (num_before_range    0)
     (num_unfiltered_rows 2))
    |}];
  modify_map t ~f:(Map.add_exn ~key:"AA" ~data:(0, 50.));
  modify_map t ~f:(Map.add_exn ~key:"AAA" ~data:(0, 75.));
  modify_map t ~f:(Map.add_exn ~key:"AAAA" ~data:(0, 87.));
  modify_map t ~f:(Map.add_exn ~key:"AAAAA" ~data:(0, 93.));
  modify_map t ~f:(Map.add_exn ~key:"AAAAAA" ~data:(0, 96.));
  modify_map t ~f:(Map.add_exn ~key:"AAAAAAA" ~data:(0, 98.));
  modify_map t ~f:(Map.add_exn ~key:"AAAAAAAA" ~data:(0, 99.));
  let next_key = ref "AAAAAAAAA" in
  for _ = 1 to 26 do
    modify_map t ~f:(Map.add_exn ~key:!next_key ~data:(0, 0.));
    next_key := !next_key ^ "A"
  done;
  (* Still didn't need rebalace *)
  print_res ~full_sexp:true t;
  [%expect
    {|
    ((data (
       (0                     (A                          (0 0)))
       (50                    (AA                         (0 50)))
       (75                    (AAA                        (0 75)))
       (87                    (AAAA                       (0 87)))
       (93                    (AAAAA                      (0 93)))
       (96                    (AAAAAA                     (0 96)))
       (98                    (AAAAAAA                    (0 98)))
       (99                    (AAAAAAAA                   (0 99)))
       (99.5                  (AAAAAAAAA                  (0 0)))
       (99.75                 (AAAAAAAAAA                 (0 0)))
       (99.875                (AAAAAAAAAAA                (0 0)))
       (99.9375               (AAAAAAAAAAAA               (0 0)))
       (99.96875              (AAAAAAAAAAAAA              (0 0)))
       (99.984375             (AAAAAAAAAAAAAA             (0 0)))
       (99.9921875            (AAAAAAAAAAAAAAA            (0 0)))
       (99.99609375           (AAAAAAAAAAAAAAAA           (0 0)))
       (99.998046875          (AAAAAAAAAAAAAAAAA          (0 0)))
       (99.9990234375         (AAAAAAAAAAAAAAAAAA         (0 0)))
       (99.99951171875        (AAAAAAAAAAAAAAAAAAA        (0 0)))
       (99.999755859375       (AAAAAAAAAAAAAAAAAAAA       (0 0)))
       (99.9998779296875      (AAAAAAAAAAAAAAAAAAAAA      (0 0)))
       (99.99993896484375     (AAAAAAAAAAAAAAAAAAAAAA     (0 0)))
       (99.999969482421875    (AAAAAAAAAAAAAAAAAAAAAAA    (0 0)))
       (99.9999847412109375   (AAAAAAAAAAAAAAAAAAAAAAAA   (0 0)))
       (99.99999237060546875  (AAAAAAAAAAAAAAAAAAAAAAAAA  (0 0)))
       (99.999996185302734375 (AAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.9999980926513671875 (AAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.99999904632568359375 (AAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.999999523162841796875 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.9999997615814208984375 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.99999988079071044921875 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.999999940395355224609375 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.9999999701976776123046875 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (99.99999998509883880615234375 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (100 (B (0 100)))))
     (num_filtered_rows   35)
     (key_range           All_rows)
     (rank_range          All_rows)
     (num_before_range    0)
     (num_unfiltered_rows 35))
    |}];
  (* But now it does *)
  modify_map t ~f:(Map.add_exn ~key:!next_key ~data:(0, 0.));
  print_res ~full_sexp:true t;
  [%expect
    {|
    ((data (
       (0    (A                                   (0 0)))
       (100  (AA                                  (0 50)))
       (200  (AAA                                 (0 75)))
       (300  (AAAA                                (0 87)))
       (400  (AAAAA                               (0 93)))
       (500  (AAAAAA                              (0 96)))
       (600  (AAAAAAA                             (0 98)))
       (700  (AAAAAAAA                            (0 99)))
       (800  (AAAAAAAAA                           (0 0)))
       (900  (AAAAAAAAAA                          (0 0)))
       (1000 (AAAAAAAAAAA                         (0 0)))
       (1100 (AAAAAAAAAAAA                        (0 0)))
       (1200 (AAAAAAAAAAAAA                       (0 0)))
       (1300 (AAAAAAAAAAAAAA                      (0 0)))
       (1400 (AAAAAAAAAAAAAAA                     (0 0)))
       (1500 (AAAAAAAAAAAAAAAA                    (0 0)))
       (1600 (AAAAAAAAAAAAAAAAA                   (0 0)))
       (1700 (AAAAAAAAAAAAAAAAAA                  (0 0)))
       (1800 (AAAAAAAAAAAAAAAAAAA                 (0 0)))
       (1900 (AAAAAAAAAAAAAAAAAAAA                (0 0)))
       (2000 (AAAAAAAAAAAAAAAAAAAAA               (0 0)))
       (2100 (AAAAAAAAAAAAAAAAAAAAAA              (0 0)))
       (2200 (AAAAAAAAAAAAAAAAAAAAAAA             (0 0)))
       (2300 (AAAAAAAAAAAAAAAAAAAAAAAA            (0 0)))
       (2400 (AAAAAAAAAAAAAAAAAAAAAAAAA           (0 0)))
       (2500 (AAAAAAAAAAAAAAAAAAAAAAAAAA          (0 0)))
       (2600 (AAAAAAAAAAAAAAAAAAAAAAAAAAA         (0 0)))
       (2700 (AAAAAAAAAAAAAAAAAAAAAAAAAAAA        (0 0)))
       (2800 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAA       (0 0)))
       (2900 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA      (0 0)))
       (3000 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA     (0 0)))
       (3100 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA    (0 0)))
       (3200 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA   (0 0)))
       (3300 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  (0 0)))
       (3400 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (0 0)))
       (3500 (B                                   (0 100)))))
     (num_filtered_rows   36)
     (key_range           All_rows)
     (rank_range          All_rows)
     (num_before_range    0)
     (num_unfiltered_rows 36))
    |}]
;;

let%expect_test "diffs" =
  let t = init_test ~data:[ "A", (0, 0.); "B", (0, 100.) ] () in
  let res1 = get_res t in
  modify_map t ~f:(Map.add_exn ~key:"AA" ~data:(0, 50.));
  modify_map t ~f:(Map.set ~key:"A" ~data:(0, 1.));
  modify_map t ~f:(Map.add_exn ~key:"BB" ~data:(0, 200.));
  let res2 = get_res t in
  let update = Concrete.diffs ~from:res1 ~to_:res2 in
  print_s [%sexp (update : Concrete.Update.t)];
  [%expect
    {|
    ((Num_unfiltered_rows 4)
     (Num_filtered_rows   4)
     (Data (Add 200 (BB (0 200))))
     (Data (Add 50 (AA (0 50))))
     (Data (Add 0 (A (0 1)))))
    |}];
  let patched = Concrete.update res1 update in
  require_equal (module Concrete) res2 patched;
  print_s [%message "" ~orig:(res2 : Concrete.t) ~patched:(patched : Concrete.t)];
  [%expect
    {|
    ((orig (
       (data (
         (0   (A  (0 1)))
         (50  (AA (0 50)))
         (100 (B  (0 100)))
         (200 (BB (0 200)))))
       (num_filtered_rows   4)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 4)))
     (patched (
       (data (
         (0   (A  (0 1)))
         (50  (AA (0 50)))
         (100 (B  (0 100)))
         (200 (BB (0 200)))))
       (num_filtered_rows   4)
       (key_range           All_rows)
       (rank_range          All_rows)
       (num_before_range    0)
       (num_unfiltered_rows 4))))
    |}]
;;

let%expect_test "duplicates in diff" =
  let t1 =
    Collated.For_testing.of_list
      ~num_filtered_rows:1
      ~num_unfiltered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      [ "1", (1, 1.0) ]
  in
  let t2 =
    Collated.For_testing.of_list
      ~num_filtered_rows:2
      ~num_unfiltered_rows:10
      ~key_range:All_rows
      ~rank_range:All_rows
      ~num_before_range:0
      [ "2", (2, 2.0) ]
  in
  let d1 = Concrete.to_diffs t1 in
  let d2 = Concrete.to_diffs t2 in
  print_s [%message "diffs" (d1 : Concrete.Update.t) (d2 : Concrete.Update.t)];
  let t1' = Concrete.of_diffs d1 in
  let t1'' = Concrete.update t1 d1 in
  let t1''' = Concrete.update t1 (d1 @ d1) in
  let t1'''' = Concrete.update t1 (d2 @ d1) in
  print_s [%message "t1s" ([ t1'; t1''; t1'''; t1'''' ] : Concrete.t list)];
  [%expect
    {|
    (diffs
      (d1 (
        (Num_unfiltered_rows     10)
        (Elements_prior_to_range 0)
        (Rank_range              All_rows)
        (Key_range               All_rows)
        (Num_filtered_rows       1)
        (Data (Add 0 (1 (1 1))))))
      (d2 (
        (Num_unfiltered_rows     10)
        (Elements_prior_to_range 0)
        (Rank_range              All_rows)
        (Key_range               All_rows)
        (Num_filtered_rows       2)
        (Data (Add 0 (2 (2 2)))))))
    (t1s (
      "[t1'; t1''; t1'''; t1'''']" (
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (1 (1 1)))))
         (num_filtered_rows   1)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))))
    |}];
  let t2' = Concrete.update t1 (d1 @ d2) in
  print_s [%message "t2s" ([ t2; t2' ] : Concrete.t list)];
  [%expect
    {|
    (t2s (
      "[t2; t2']" (
        ((data ((0 (2 (2 2)))))
         (num_filtered_rows   2)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10))
        ((data ((0 (2 (2 2)))))
         (num_filtered_rows   2)
         (key_range           All_rows)
         (rank_range          All_rows)
         (num_before_range    0)
         (num_unfiltered_rows 10)))))
    |}]
;;

let%expect_test "instrumentation: every instrumented stage gets hit once" =
  let hits = String.Table.create () in
  let instrument name =
    { Incr_map.Instrumentation.f =
        (fun f ->
          Hashtbl.update hits name ~f:(function
            | None -> 1
            | Some x -> x + 1);
          f ())
    }
  in
  let print_hits () =
    Expectable.print
      (Hashtbl.to_alist hits
       |> List.map ~f:(fun (stage, hits) -> [%sexp { stage : string; hits : int }]))
  in
  let instrumentation : Instrumentation.t =
    { filter = instrument "filter"
    ; fold = instrument "fold"
    ; sort = instrument "sort"
    ; key_subrange = instrument "key_subrange"
    ; key_to_rank = instrument "key_to_rank"
    ; rank_range = instrument "rank_range"
    }
  in
  let do_collate =
    Incr_map_collate.collate_and_fold
      ~instrumentation
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      ~fold:
        (Fold.create
           ~init:()
           ~add:(fun ~key:_ ~data:_ () -> ())
           ~remove:(fun ~key:_ ~data:_ () -> ())
           ())
  in
  let (_ : t) =
    init_test
      ~do_collate
      ~operation_order:`Sort_first
      ~order:Order.By_price
      ~filter:Filter.True
      ~key_range:(From "VOD")
      ~rank_range:(Between (From_start 0, From_start 100))
      ()
  in
  Incr.stabilize ();
  print_hits ();
  [%expect
    {|
    ┌──────────────┬──────┐
    │ stage        │ hits │
    ├──────────────┼──────┤
    │ sort         │ 1    │
    │ rank_range   │ 1    │
    │ key_subrange │ 1    │
    │ fold         │ 1    │
    │ filter       │ 1    │
    │ key_to_rank  │ 1    │
    └──────────────┴──────┘
    |}];
  (* Verifying we witnessed 6 instrumentation stages *)
  [%test_eq: int] (Hashtbl.length hits) 6;
  (* Verifying we only ran each stage once *)
  [%test_eq: int] (Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data acc -> data + acc) hits) 6
;;

module%test [@name "new API"] _ = struct
  module Range = Incr_map_collate.With_caching.Range_memoize_bucket

  module Order_filter = struct
    type t = Order.t * Filter.t [@@deriving equal, compare, sexp_of]
  end

  module Order_filter_range = struct
    type t = Order.t * Filter.t * Range.t [@@deriving equal, compare, sexp_of]
  end

  let do_collate_new_api
    ~order_cache_size
    ~order_filter_cache_size
    ~order_filter_range_cache_size
    ()
    ?operation_order
    input
    collate
    =
    (match operation_order with
     | None | Some `Sort_first -> ()
     | Some `Filter_first -> failwith "New API only supports sorting first");
    let order_cache_params =
      Incr_memoize.Store_params.with_hooks
        ~if_found:(printf !"Found  : %{sexp:Order.t}\n")
        ~if_added:(printf !"Updated: %{sexp:Order.t}\n")
        (Incr_memoize.Store_params.alist_based__lru
           ~equal:Order.equal
           ~max_size:order_cache_size)
    in
    let order_filter_cache_params =
      Incr_memoize.Store_params.with_hooks
        ~if_found:(fun (order, filter) ->
          printf !"Found  : %{sexp:Order.t}, %{sexp:Filter.t}\n" order filter)
        ~if_added:(fun (order, filter) ->
          printf !"Updated: %{sexp:Order.t}, %{sexp:Filter.t}\n" order filter)
        (Incr_memoize.Store_params.alist_based__lru
           ~equal:Order_filter.equal
           ~max_size:order_filter_cache_size)
    in
    let order_filter_range_cache_params =
      Incr_memoize.Store_params.with_hooks
        ~if_found:(fun (order, filter, range) ->
          printf
            !"Found  : %{sexp:Order.t}, %{sexp:Filter.t}, %{sexp:Range.t}\n"
            order
            filter
            range)
        ~if_added:(fun (order, filter, range) ->
          printf
            !"Updated: %{sexp:Order.t}, %{sexp:Filter.t}, %{sexp:Range.t}\n"
            order
            filter
            range)
        (Incr_memoize.Store_params.alist_based__lru
           ~equal:Order_filter_range.equal
           ~max_size:order_filter_range_cache_size)
    in
    Incr_map_collate.With_caching.collate__sort_first
      ~filter_to_predicate:Filter.to_predicate
      ~order_to_compare:Order.to_compare
      ~filter_equal:Filter.equal
      ~order_equal:Order.equal
      ~range_memoize_bucket_size:1
      ~order_cache_params
      ~order_filter_cache_params
      ~order_filter_range_cache_params
      input
      collate
  ;;

  let%expect_test "behavior of deep caches" =
    let t =
      init_test
        ~do_collate:
          (do_collate_new_api
             ~order_cache_size:1
             ~order_filter_cache_size:2
             ~order_filter_range_cache_size:3
             ())
        ()
    in
    (* Default sort + filter *)
    Incr.stabilize ();
    [%expect
      {|
      Updated: By_symbol
      Updated: By_symbol, None
      Updated: By_symbol, None, (All_rows All_rows)
      |}];
    (* Change range *)
    set_collate t ~rank_range:(Between (From_start 1, From_start 2));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (1 2)))
      |}];
    (* Change range again. Now the (order, filter, range) cache is full. *)
    set_collate t ~rank_range:(Between (From_start 2, From_start 3));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (2 3)))
      |}];
    (* (1,2) is still in the cache. *)
    set_collate t ~rank_range:(Between (From_start 1, From_start 2));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Found  : By_symbol, None, (All_rows (Between (1 2)))
      |}];
    (* Adding a 4th range... *)
    set_collate t ~rank_range:(Between (From_start 3, From_start 4));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (3 4)))
      |}];
    (* Select a From_end range for the first time *)
    set_collate t ~rank_range:(Between (From_end 1, From_end 0));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (-2 -1)))
      |}];
    (* Select the prev range, we should find it *)
    set_collate t ~rank_range:(Between (From_start 3, From_start 4));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Found  : By_symbol, None, (All_rows (Between (3 4)))
      |}];
    (* Re-select the From_end range, we should find it *)
    set_collate t ~rank_range:(Between (From_end 1, From_end 0));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Found  : By_symbol, None, (All_rows (Between (-2 -1)))
      |}];
    set_collate t ~rank_range:All_rows;
    (* [All_rows] was evicted from the cache, so this will add it again. *)
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows All_rows)
      |}]
  ;;

  let%expect_test "shallow cache is evicted while value is still in use by deeper cache" =
    let t =
      init_test
        ~do_collate:
          (do_collate_new_api
             ~order_cache_size:1
             ~order_filter_cache_size:2
             ~order_filter_range_cache_size:3
             ())
        ()
    in
    (* Default sort + filter *)
    Incr.stabilize ();
    [%expect
      {|
      Updated: By_symbol
      Updated: By_symbol, None
      Updated: By_symbol, None, (All_rows All_rows)
      |}];
    (* Change range *)
    set_collate t ~rank_range:(Between (From_start 1, From_start 2));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (1 2)))
      |}];
    (* Change range again. Now the (order, filter, range) cache is full. *)
    set_collate t ~rank_range:(Between (From_start 2, From_start 3));
    Incr.stabilize ();
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (2 3)))
      |}];
    (* This will evict [By_symbol] from the cache. *)
    set_collate t ~order:By_price ~rank_range:(Between (From_start 1, From_start 2));
    Incr.stabilize ();
    [%expect
      {|
      Updated: By_price
      Updated: By_price, None
      Updated: By_price, None, (All_rows (Between (1 2)))
      |}];
    (* As you can see, [By_symbol] is gone from the cache. *)
    set_collate t ~order:By_symbol ~filter:True;
    Incr.stabilize ();
    [%expect
      {|
      Updated: By_symbol
      Updated: By_symbol, True
      Updated: By_symbol, True, (All_rows (Between (1 2)))
      |}];
    (* The old value of (order, filter, range) is still present in the cache, but we
         don't want to use it, as this would mean having two copies of the "sort by
         symbol" intermediate computation (the one created just above, and the one kept
         alive by the reference from the (order, filter, range) cache.

         So, here we don't use it, but instead recreate the computation on top of the new
         sorting computation. *)
    set_collate
      t
      ~order:By_symbol
      ~filter:None
      ~rank_range:(Between (From_start 2, From_start 3));
    print_res t;
    [%expect
      {|
      Found  : By_symbol
      Found  : By_symbol, None, (All_rows (Between (2 3)))
      Updated: By_symbol, None
      Updated: By_symbol, None, (All_rows (Between (2 3)))
      (((VOD (10 2)))
       (num_filtered_rows   3)
       (num_unfiltered_rows 3))
      |}];
    set_collate t ~rank_range:All_rows
  ;;
end
