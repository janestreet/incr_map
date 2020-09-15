open Core_kernel
module Which_range = Collate.Which_range

module Parametrized = struct
  type ('k, 'v) t =
    { data : ('k * 'v) Map_list.t
    ; num_filtered_rows : int
    ; key_range : 'k Which_range.t (** Ranges that this value was computed for *)
    ; rank_range : int Which_range.t
    ; num_before_range : int
    ; num_unfiltered_rows : int
    }
  [@@deriving sexp, compare, fields, equal, bin_io]

  let empty =
    { data = Int63.Map.empty
    ; num_filtered_rows = 0
    ; key_range = All_rows
    ; rank_range = All_rows
    ; num_before_range = 0
    ; num_unfiltered_rows = 0
    }
  ;;

  let num_after_range { num_before_range; num_filtered_rows; data; _ } =
    num_filtered_rows - num_before_range - Map.length data
  ;;

  let fold t ~init ~f = Map.fold t.data ~init ~f:(fun ~key:_ ~data acc -> f acc data)
  let iter t ~f = Map.iter t.data ~f
  let to_alist t = Map.data t.data
  let to_map_list t = t.data
  let first t = Map.min_elt t.data |> Option.map ~f:snd
  let last t = Map.max_elt t.data |> Option.map ~f:snd
  let mapi t ~f = { t with data = Map.map t.data ~f:(fun (k, v) -> k, f k v) }
  let length t = Map.length t.data

  module Private = struct
    let create = Fields.create
  end

  module For_testing = struct
    let of_list
          ~num_filtered_rows
          ~key_range
          ~rank_range
          ~num_before_range
          ~num_unfiltered_rows
          data
      =
      { data =
          List.mapi data ~f:(fun i x -> 100 * i |> Int63.of_int, x)
          |> Int63.Map.of_alist_exn
      ; num_filtered_rows
      ; rank_range
      ; key_range
      ; num_before_range
      ; num_unfiltered_rows
      }
    ;;
  end
end

include Parametrized

module type Concrete = Collated_intf.Concrete with type ('k, 'v) parametrized = ('k, 'v) t

module Make_concrete
    (Key : Collated_intf.Bin_comp_sexp)
    (Value : Collated_intf.Bin_comp_sexp) =
struct
  module Key = Key
  module Value = Value
  include Parametrized

  type ('k, 'v) parametrized = ('k, 'v) t

  module T = struct
    type t = (Key.t, Value.t) Parametrized.t [@@deriving sexp, bin_io, compare, equal]

    (* We have to implement this by hand, as ppx_diff (or Diffable really)
       doesn't support parametrized types *)

    module Update = struct
      module Map_data = struct
        type t = Key.t * Value.t [@@deriving sexp, bin_io, compare, equal]
      end

      module Map = Diffable.Map.Make (Int63) (Map_data)

      module Diff = struct
        type t =
          | Data of Map.Update.Diff.t
          | Num_filtered_rows of int
          | Key_range of Key.t Which_range.t
          | Rank_range of int Which_range.t
          | Elements_prior_to_range of int
          | Num_unfiltered_rows of int
        [@@deriving sexp, bin_io]
      end

      type t = Diff.t list [@@deriving sexp, bin_io]
    end

    let update (t : t) (update : Update.t) =
      List.fold update ~init:t ~f:(fun acc diff ->
        match diff with
        | Data map_diff -> { acc with data = Update.Map.update acc.data [ map_diff ] }
        | Num_filtered_rows num_filtered_rows -> { acc with num_filtered_rows }
        | Key_range key_range -> { acc with key_range }
        | Rank_range rank_range -> { acc with rank_range }
        | Elements_prior_to_range num_before_range -> { acc with num_before_range }
        | Num_unfiltered_rows num_unfiltered_rows -> { acc with num_unfiltered_rows })
    ;;

    let wrap_map_update = List.map ~f:(fun x -> Update.Diff.Data x)

    let diffs ~from ~to_ =
      let get = Fieldslib.Field.get in
      Fields.fold
        ~init:[]
        ~data:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          wrap_map_update (Update.Map.diffs ~from ~to_) @ acc)
        ~num_filtered_rows:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          if Int.equal from to_ then acc else Num_filtered_rows to_ :: acc)
        ~key_range:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          if Which_range.equal Key.equal from to_ then acc else Key_range to_ :: acc)
        ~rank_range:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          if Which_range.equal Int.equal from to_ then acc else Rank_range to_ :: acc)
        ~num_before_range:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          if Int.equal from to_ then acc else Elements_prior_to_range to_ :: acc)
        ~num_unfiltered_rows:(fun acc field ->
          let from, to_ = get field from, get field to_ in
          if Int.equal from to_ then acc else Num_unfiltered_rows to_ :: acc)
    ;;

    let to_diffs (t : t) =
      let get f = Fieldslib.Field.get f t in
      Fields.fold
        ~init:[]
        ~data:(fun acc field -> wrap_map_update (Update.Map.to_diffs (get field)) @ acc)
        ~num_filtered_rows:(fun acc field ->
          Update.Diff.Num_filtered_rows (get field) :: acc)
        ~key_range:(fun acc field -> Update.Diff.Key_range (get field) :: acc)
        ~rank_range:(fun acc field -> Update.Diff.Rank_range (get field) :: acc)
        ~num_before_range:(fun acc field ->
          Update.Diff.Elements_prior_to_range (get field) :: acc)
        ~num_unfiltered_rows:(fun acc field ->
          Update.Diff.Num_unfiltered_rows (get field) :: acc)
    ;;

    let of_diffs (update : Update.t) =
      let data_update = ref [] in
      let num_filtered_rows = ref None in
      let key_range = ref None in
      let rank_range = ref None in
      let num_before_range = ref 0 in
      let num_unfiltered_rows = ref None in
      List.iter update ~f:(function
        | Data map_diff -> data_update := map_diff :: !data_update
        | Num_filtered_rows n -> num_filtered_rows := Some n
        | Key_range r -> key_range := Some r
        | Rank_range r -> rank_range := Some r
        | Elements_prior_to_range i -> num_before_range := i
        | Num_unfiltered_rows n -> num_unfiltered_rows := Some n);
      let data = Update.Map.of_diffs !data_update in
      let get_exn ref ~name =
        Option.value_exn
          !ref
          ~message:(sprintf "[Collated.of_diffs]: %s missing from diffs list" name)
      in
      Fields.create
        ~data
        ~num_filtered_rows:(get_exn num_filtered_rows ~name:"num_filtered_rows")
        ~key_range:(get_exn key_range ~name:"key_range")
        ~rank_range:(get_exn rank_range ~name:"rank_range")
        ~num_before_range:!num_before_range
        ~num_unfiltered_rows:(get_exn num_unfiltered_rows ~name:"num_unfiltered_rows")
    ;;
  end

  include T
  include Diffable.Make_streamable (T)

  let findi_by_key t key =
    let found =
      Map.filteri ~f:(fun ~key:_pos ~data:(key', _value) -> Key.equal key key') t.data
      |> Map.to_alist
    in
    match found with
    | [ x ] -> Some x
    | [] -> None
    | _ ->
      raise_s
        [%message "[Collated.findi_by_key] BUG: multiple entries found" (key : Key.t)]
  ;;

  let find_by_key t key =
    Option.map (findi_by_key t key) ~f:(fun (_pos, (_key, value)) -> value)
  ;;

  let prev t key =
    let%bind.Option pos, _ = findi_by_key t key in
    let%map.Option _pos, res = Map.closest_key t.data `Less_than pos in
    res
  ;;

  let next t key =
    let%bind.Option pos, _ = findi_by_key t key in
    let%map.Option _pos, res = Map.closest_key t.data `Greater_than pos in
    res
  ;;
end
