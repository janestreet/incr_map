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

let set_collate ?rank_range ?key_range ?widen_range_by t =
  let collate = Incr.Var.value t.collate in
  let collate =
    { Collate_params.filter = ()
    ; key_range = Option.value key_range ~default:collate.key_range
    ; rank_range = Option.value rank_range ~default:collate.rank_range
    ; order = ()
    ; widen_range_by = Option.value widen_range_by ~default:collate.widen_range_by
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

let get_result t =
  Incr.stabilize ();
  Incr.Observer.value_exn t.observer
;;

(* Test that [num_before_range] and [num_after_range] are computed correctly with
   [widen_range_by].

   The intent is that these values represent the pre-widened range, so a user can then
   derive other offset values using [range_widened_by]. *)
let%expect_test "num_before_range and num_after_range with widen_range_by" =
  (* Create a map with 10 elements *)
  let map =
    String.Map.of_alist_exn
      [ "a", 0; "b", 1; "c", 2; "d", 3; "e", 4; "f", 5; "g", 6; "h", 7; "i", 8; "j", 9 ]
  in
  (* Start with rank_range = [3, 6], which should select d, e, f, g (4 elements) *)
  let t =
    init ~key_range:All_rows ~rank_range:(Between (From_start 3, From_start 6)) map
  in
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ d │ 3 │
    │ e │ 4 │
    │ f │ 5 │
    │ g │ 6 │
    └───┴───┘
    |}];
  let result = get_result t in
  print_s
    [%message
      "Without widening"
        ~num_filtered_rows:(Collated.num_filtered_rows result : int)
        ~num_before_range:(Collated.num_before_range result : int)
        ~num_after_range:(Collated.num_after_range result : int)
        ~range_widened_by:(Collated.range_widened_by result : int * int)
        ~data_length:(Collated.length result : int)];
  [%expect
    {|
    ("Without widening" (num_filtered_rows 10) (num_before_range 3)
     (num_after_range 3) (range_widened_by (0 0)) (data_length 4))
    |}];
  (* Now widen by (1, 2). This should select c, d, e, f, g, h, i (7 elements) *)
  set_collate ~widen_range_by:(1, 2) t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ c │ 2 │
    │ d │ 3 │
    │ e │ 4 │
    │ f │ 5 │
    │ g │ 6 │
    │ h │ 7 │
    │ i │ 8 │
    └───┴───┘
    |}];
  let result = get_result t in
  print_s
    [%message
      "With widening (1, 2)"
        ~num_filtered_rows:(Collated.num_filtered_rows result : int)
        ~num_before_range:(Collated.num_before_range result : int)
        ~num_after_range:(Collated.num_after_range result : int)
        ~range_widened_by:(Collated.range_widened_by result : int * int)
        ~data_length:(Collated.length result : int)];
  [%expect
    {|
    ("With widening (1, 2)" (num_filtered_rows 10) (num_before_range 3)
     (num_after_range 3) (range_widened_by (1 2)) (data_length 7))
    |}];
  (* Test edge case: widening at the start *)
  set_collate ~rank_range:(Between (From_start 0, From_start 2)) ~widen_range_by:(2, 1) t;
  Incr.stabilize ();
  [%expect
    {|
    ┌┬┬┬┬┬┬┬┐
    ├┴┴┴┼┴┴┴┤
    │ a │ 0 │
    │ b │ 1 │
    │ c │ 2 │
    │ d │ 3 │
    └───┴───┘
    |}];
  let result = get_result t in
  print_s
    [%message
      "Widening at start (clamped)"
        ~num_filtered_rows:(Collated.num_filtered_rows result : int)
        ~num_before_range:(Collated.num_before_range result : int)
        ~num_after_range:(Collated.num_after_range result : int)
        ~range_widened_by:(Collated.range_widened_by result : int * int)
        ~data_length:(Collated.length result : int)];
  [%expect
    {|
    ("Widening at start (clamped)" (num_filtered_rows 10) (num_before_range 0)
     (num_after_range 7) (range_widened_by (0 1)) (data_length 4))
    |}]
;;
