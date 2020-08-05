(** A representation of a list that provides useful diffing. When constructed from a
    sorted map in an incremental way, you could reasonably expect that most of the time
    the diff size will be proportional to the number of elements that are
    added/removed/changed. *)

open Core_kernel

type 'a t = 'a Int63.Map.t [@@deriving sexp, equal, compare, bin_io]

val of_map
  :  ?data_equal:('b -> 'b -> bool)
  -> ?separation:Int63.t
  -> (('a, 'b, 'cmp) Map.t, 'incr_witness) Incremental.t
  -> get:(key:'a -> data:'b -> 'res)
  (** Make the result value from the key & data of the original map. Most of the time you
      just want [fun ~key ~data -> (key,data)], but the presence of this argument
      effectively lets you fuse a [mapi] operation into this one *)
  -> ('res t, 'incr_witness) Incremental.t
