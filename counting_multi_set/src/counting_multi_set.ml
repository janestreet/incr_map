open! Core
open! Import

type ('ele, 'cmp) t =
  { counts : ('ele, int, 'cmp) Map.t
  ; set : ('ele, 'cmp) Set.t
  }

let to_set t = t.set
let of_set set = { set; counts = Set.to_map set ~f:(const 1) }

let empty ~comparator =
  { counts = Map.Using_comparator.empty ~comparator
  ; set = Set.Using_comparator.empty ~comparator
  }
;;

let is_empty { counts = _; set } = Set.is_empty set

let add { counts; set } ele =
  let add_key = ref false in
  let counts =
    Map.update counts ele ~f:(function
      | None ->
        add_key := true;
        1
      | Some count -> count + 1)
  in
  let set = if !add_key then Set.add set ele else set in
  { counts; set }
;;

let remove { counts; set } ele =
  let remove_key = ref false in
  let counts =
    Map.change counts ele ~f:(function
      | None -> None
      | Some 1 ->
        remove_key := true;
        None
      | Some count -> Some (count - 1))
  in
  let set = if !remove_key then Set.remove set ele else set in
  { counts; set }
;;

let invariants { counts; set } =
  Set.equal (Map.key_set counts) set && Map.for_all counts ~f:Int.is_positive
;;
