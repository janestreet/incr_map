open! Core

module Key : sig
  type t [@@deriving sexp, bin_io]

  include Comparable.S with type t := t

  val to_string : t -> string
  val zero : t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]
    end
  end
end

type 'a t = (Key.t, 'a, Key.comparator_witness) Map.t
[@@deriving sexp, compare, equal, bin_io]

include Diffable.S1 with type 'a t := 'a t

(** When Opaque_maps are created incrementally we can be smart about insertion and get
    good performance around insertion and rebalancing. *)
val erase_key_incrementally
  :  ?data_equal:('data -> 'data -> bool)
  -> (('key, 'data, _) Map.t, 'w) Incremental.t
  -> get:(key:'key -> data:'data -> 'a)
       (** Make the result value from the key & data of the original map. Most of the time
           you just want [fun ~key ~data -> (key,data)], but the presence of this argument
           effectively lets you fuse a [mapi] operation into this one *)
  -> ('a t, 'w) Incremental.t

(** [empty], [of_list], and [of_array] won't give you nice incrementality like
    [erase_key_incrementally], but they are still fine if the primary goal is using the
    type itself outside of an incremental context.

    But if possible, consider [erase_key_incrementally]. *)

val empty : _ t
val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving sexp, bin_io, diff, stable_witness]
  end
end
