open! Core
module Incr = Incremental.Make ()
open Incr_map_collate

module Concrete = struct
  type t = (String.t, Int.t) Incr_map_collate.Collated.t
end

module Order = Unit
module Filter = Unit

type t =
  { map : int String.Map.t Incr.Var.t
  ; collate : (string, unit, unit) Collate_params.t Incr.Var.t
  ; observer : Concrete.t Incr.Observer.t
  }

let set_collate ?rank_range ?key_range t =
  let collate = Incr.Var.value t.collate in
  let collate =
    { Collate_params.filter = ()
    ; key_range = Option.value key_range ~default:collate.key_range
    ; rank_range = Option.value rank_range ~default:collate.rank_range
    ; order = ()
    ; widen_range_by = collate.widen_range_by
    }
  in
  Incr.Var.set t.collate collate
;;

let do_collate input collate =
  Incr_map_collate.collate
    ~filter_to_predicate:(fun () -> None)
    ~order_to_compare:(fun () -> Compare.Unchanged)
    ~filter_equal:Filter.equal
    ~order_equal:Order.equal
    input
    collate
;;

let init ~key_range ~rank_range map =
  let map = Incr.Var.create map in
  let collate =
    Incr.Var.create
      { Collate_params.filter = ()
      ; order = ()
      ; key_range
      ; rank_range
      ; widen_range_by = 0, 0
      }
  in
  let observer =
    do_collate (Incr.Var.watch map) (Incr.Var.watch collate)
    |> Incr_map_collate.collated
    |> Incr.observe
  in
  Incr.Observer.on_update_exn observer ~f:(function
    | Invalidated -> ()
    | Initialized result | Changed (_, result) ->
      result |> Collated.to_alist |> Expectable.print_alist [%sexp_of: int]);
  { map; collate; observer }
;;

let%expect_test "key range followed by rank range (forewards)" =
  let map =
    String.Map.of_alist_exn [ "a", 1; "b", 2; "c", 3; "d", 4; "e", 5; "f", 6; "g", 7 ]
  in
  let t = init ~key_range:All_rows ~rank_range:All_rows map in
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ a │ 1 │
    │ b │ 2 │
    │ c │ 3 │
    │ d │ 4 │
    │ e │ 5 │
    │ f │ 6 │
    │ g │ 7 │
    └───┴───┘
    |}];
  set_collate ~key_range:(From "c") t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ c │ 3 │
    │ d │ 4 │
    │ e │ 5 │
    │ f │ 6 │
    │ g │ 7 │
    └───┴───┘
    |}];
  set_collate ~rank_range:(From (From_start 2)) t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ e │ 5 │
    │ f │ 6 │
    │ g │ 7 │
    └───┴───┘
    |}]
;;

let%expect_test "key range followed by rank range (backwards)" =
  let map =
    String.Map.of_alist_exn [ "a", 1; "b", 2; "c", 3; "d", 4; "e", 5; "f", 6; "g", 7 ]
  in
  let t = init ~key_range:All_rows ~rank_range:All_rows map in
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ a │ 1 │
    │ b │ 2 │
    │ c │ 3 │
    │ d │ 4 │
    │ e │ 5 │
    │ f │ 6 │
    │ g │ 7 │
    └───┴───┘
    |}];
  set_collate ~key_range:(To "e") t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ a │ 1 │
    │ b │ 2 │
    │ c │ 3 │
    │ d │ 4 │
    │ e │ 5 │
    └───┴───┘
    |}];
  set_collate ~rank_range:(To (From_end 2)) t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ a │ 1 │
    │ b │ 2 │
    │ c │ 3 │
    └───┴───┘
    |}]
;;
