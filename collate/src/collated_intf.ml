open Core_kernel
module Which_range = Collate.Which_range

module type Parametrized = sig
  (** The result of collation - a filtered, sorted and restricted-to-a-range list of keys
      and values. The underlying data structure is a bit more sophisticated than a list,
      to provide better diffing.

      To get an implementation of [Diffable] interface, you'll need to instantiate
      [Make_concrete].
  *)
  type ('k, 'v) t [@@deriving sexp, bin_io, compare, equal]

  val empty : _ t
  val fold : ('k, 'v) t -> init:'accum -> f:('accum -> 'k * 'v -> 'accum) -> 'accum
  val iter : ('k, 'v) t -> f:('k * 'v -> unit) -> unit
  val to_alist : ('k, 'v) t -> ('k * 'v) list
  val to_map_list : ('k, 'v) t -> ('k * 'v) Map_list.t
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

  module Private : sig
    val create
      :  data:('k * 'v) Int63.Map.t
      -> num_filtered_rows:int
      -> key_range:'k Which_range.t
      -> rank_range:int Which_range.t
      -> num_before_range:int
      -> num_unfiltered_rows:int
      -> ('k, 'v) t
  end

  module For_testing : sig
    (** Create Collated.t of a list of data. Note: no collation or checks are performed,
        it will contain exactly the data you provided *)
    val of_list
      :  num_filtered_rows:int
      -> key_range:'k Which_range.t
      -> rank_range:int Which_range.t
      -> num_before_range:int
      -> num_unfiltered_rows:int
      -> ('k * 'v) list
      -> ('k, 'v) t
  end
end

module type Bin_comp_sexp = sig
  type t [@@deriving bin_io, sexp, compare, equal]
end

module type Concrete = sig
  module Key : Bin_comp_sexp
  module Value : Bin_comp_sexp

  type ('k, 'v) parametrized
  type t = (Key.t, Value.t) parametrized [@@deriving sexp, bin_io, compare, equal]

  val empty : t
  val fold : t -> init:'accum -> f:('accum -> Key.t * Value.t -> 'accum) -> 'accum
  val iter : t -> f:(Key.t * Value.t -> unit) -> unit
  val to_alist : t -> (Key.t * Value.t) list
  val to_map_list : t -> (Key.t * Value.t) Map_list.t
  val first : t -> (Key.t * Value.t) option
  val last : t -> (Key.t * Value.t) option
  val length : t -> int
  val num_filtered_rows : t -> int
  val key_range : t -> Key.t Which_range.t
  val rank_range : t -> int Which_range.t

  include Diffable.S with type t := t
  include Streamable.S with type t := t


  val find_by_key : t -> Key.t -> Value.t option
  val prev : t -> Key.t -> (Key.t * Value.t) option
  val next : t -> Key.t -> (Key.t * Value.t) option
end

module type Collated = sig
  include Parametrized

  module type Concrete = Concrete with type ('k, 'v) parametrized = ('k, 'v) t

  module Make_concrete (Key : Bin_comp_sexp) (Value : Bin_comp_sexp) :
    Concrete with type Key.t = Key.t and type Value.t = Value.t
end
