open! Core

module Key : sig
  type t [@@deriving sexp, bin_io]

  include Comparable.S with type t := t

  val to_string : t -> string
  val zero : t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end
end

type 'a t = (Key.t, 'a, Key.comparator_witness) Map.t
[@@deriving sexp, compare, equal, bin_io]

val erase
  :  ?data_equal:('data -> 'data -> bool)
  -> (('key, 'data, _) Map.t, 'w) Incremental.t
  -> get:(key:'key -> data:'data -> 'a)
       (** Make the result value from the key & data of the original map. Most of the time you
      just want [fun ~key ~data -> (key,data)], but the presence of this argument
      effectively lets you fuse a [mapi] operation into this one *)
  -> ('a t, 'w) Incremental.t

module For_testing : sig
  val of_list : 'a list -> 'a t
end
