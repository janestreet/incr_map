open Core
module Which_range = Collate_params.Which_range

module type Parametrized = sig @@ portable
  (** The result of collation - a filtered, sorted and restricted-to-a-range list of keys
      and values. The underlying data structure is a bit more sophisticated than a list,
      to provide better diffing.

      To get an implementation of [Diffable] interface, you'll need to instantiate
      [Make_concrete]. *)

  type ('k, 'v) t : value mod contended portable with 'k with 'v
  [@@deriving sexp_of, compare, equal]

  module (Unstable @@ nonportable) : sig @@ portable
    type nonrec ('k, 'v) t = ('k, 'v) t [@@deriving sexp, bin_io]

    include Diffable.S2 with type ('k, 'v) t := ('k, 'v) t @@ nonportable
  end

  val empty : _ t @@ nonportable
  val fold : ('k, 'v) t -> init:'accum -> f:('accum -> 'k * 'v -> 'accum) -> 'accum
  val iter : ('k, 'v) t -> f:('k * 'v -> unit) -> unit
  val to_alist : ('k, 'v) t -> ('k * 'v) list
  val to_opaque_map : ('k, 'v) t -> ('k * 'v) Opaque_map.t
  val first : ('k, 'v) t -> ('k * 'v) option
  val last : ('k, 'v) t -> ('k * 'v) option
  val mapi : ('k, 'v1) t -> f:('k -> 'v1 -> 'v2) -> ('k, 'v2) t
  val length : _ t -> int

  (** Total number of rows before filtering *)
  val num_unfiltered_rows : _ t -> int

  (** Total number of rows after filtering, but before limiting to range. *)
  val num_filtered_rows : _ t -> int

  (** Total number of rows that preceed the rank-range and key-range ranges. *)
  val num_before_range : _ t -> int

  (** Total number of rows that follow the rank-range and key-range ranges. *)
  val num_after_range : _ t -> int

  (** The key range this result was computed for *)
  val key_range : ('k, _) t -> 'k Which_range.t

  (** The rank range this result was computed for *)
  val rank_range : _ t -> int Which_range.t

  (** The amount that the resulting range was widened by. Closely related to
      [Collate_params.widen_range_by], [range_widened_by] specifies the actual amount of
      widening given bounds of the underlying dataset. *)
  val range_widened_by : _ t -> int * int
end

module type Bin_comp_sexp = sig
  type t [@@deriving bin_io, sexp, compare, equal]
end

module type%template [@modality p = (nonportable, portable)] Concrete = sig @@ p
  module Key : Bin_comp_sexp
  module Value : Bin_comp_sexp

  type ('k, 'v) parametrized
  type t = (Key.t, Value.t) parametrized [@@deriving sexp, bin_io, compare, equal]

  val empty : t @@ nonportable
  val fold : t -> init:'accum -> f:('accum -> Key.t * Value.t -> 'accum) -> 'accum
  val iter : t -> f:(Key.t * Value.t -> unit) -> unit
  val to_alist : t -> (Key.t * Value.t) list
  val to_opaque_map : t -> (Key.t * Value.t) Opaque_map.t
  val first : t -> (Key.t * Value.t) option
  val last : t -> (Key.t * Value.t) option
  val length : t -> int
  val num_filtered_rows : t -> int
  val num_unfiltered_rows : t -> int
  val key_range : t -> Key.t Which_range.t
  val rank_range : t -> int Which_range.t

  include Legacy_diffable.S with type t := t @@ nonportable
  include Streamable.S with type t := t @@ nonportable

  (** This strange value just encodes the fact that this type does not yet implement
      [Ldiffable.S]. When it does, delete this and then the compiler will show you places
      to update. *)
  val this_type_does_not_support_ldiffable : unit

  val find_by_key : t -> Key.t -> Value.t option
  val prev : t -> Key.t -> (Key.t * Value.t) option
  val next : t -> Key.t -> (Key.t * Value.t) option

  module Private : sig
    val create
      :  data:('k * 'v) Opaque_map.t
      -> num_filtered_rows:int
      -> key_range:'k Which_range.t
      -> rank_range:int Which_range.t
      -> num_before_range:int
      -> num_unfiltered_rows:int
      -> ('k, 'v) parametrized
  end
end

module type Collated = sig
  include Parametrized

  module Stable : sig @@ portable
    module (V1 @@ nonportable) : sig @@ portable
      type ('k, 'v) t : value mod contended portable with 'k with 'v
      [@@deriving sexp, bin_io, stable_witness, compare, equal]

      val empty : _ t @@ nonportable

      include Diffable.S2 with type ('k, 'v) t := ('k, 'v) t @@ nonportable

      (** The old [Concrete] / [Legacy_diffable] is only supported on Stable.V1 *)
      module type Concrete = Concrete with type ('k, 'v) parametrized = ('k, 'v) t

      module%template
        [@modality p = (nonportable, portable)] Make_concrete
          (Key : sig
           @@ p
             include Bin_comp_sexp
           end)
          (Value : sig
           @@ p
             include Bin_comp_sexp
           end) :
        Concrete [@modality p] with type Key.t = Key.t and type Value.t = Value.t
    end

    module (V2 @@ nonportable) : sig @@ portable
      type nonrec ('k, 'v) t : value mod contended portable with 'k with 'v = ('k, 'v) t
      [@@deriving sexp, bin_io, stable_witness, equal]

      val empty : _ t @@ nonportable

      include Diffable.S2 with type ('k, 'v) t := ('k, 'v) t @@ nonportable

      val of_v1 : ('k, 'v) V1.t -> ('k, 'v) t
      val to_v1 : ('k, 'v) t -> ('k, 'v) V1.t
    end
  end

  module Unstable : sig @@ portable
    type nonrec ('k, 'v) t : value mod contended portable with 'k with 'v = ('k, 'v) t
    [@@deriving sexp, bin_io, compare, equal]

    module Diff = Stable.V2.Diff
  end

  module Diff = Stable.V2.Diff

  val of_stable_v1 : ('k, 'v) Stable.V1.t -> ('k, 'v) t @@ portable
  val to_stable_v1 : ('k, 'v) t -> ('k, 'v) Stable.V1.t @@ portable

  module Private : sig
    val create
      :  data:('k * 'v) Opaque_map.t
      -> num_filtered_rows:int
      -> key_range:'k Which_range.t
      -> rank_range:int Which_range.t
      -> num_before_range:int
      -> range_widened_by:int * int
      -> num_unfiltered_rows:int
      -> ('k, 'v) t
      @@ portable

    module Stable : sig
      module V1 : sig
        val create
          :  data:('k * 'v) Opaque_map.t
          -> num_filtered_rows:int
          -> key_range:'k Which_range.t
          -> rank_range:int Which_range.t
          -> num_before_range:int
          -> num_unfiltered_rows:int
          -> ('k, 'v) Stable.V1.t
          @@ portable
      end
    end
  end

  module For_testing : sig
    (** Create Collated.t of a list of data. Note: no collation or checks are performed,
        it will contain exactly the data you provided *)
    val of_list
      :  num_filtered_rows:int
      -> key_range:'k Which_range.t
      -> rank_range:int Which_range.t
      -> num_before_range:int
      -> range_widened_by:int * int
      -> num_unfiltered_rows:int
      -> ('k * 'v) list
      -> ('k, 'v) t
  end
end
