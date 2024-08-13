(** Implements chains of sequential key, used as indices into a trie. *)

open! Base

(** The interface to a keychain type. *)
module type S = sig
  (** The type of a keychain implementation. See [Keychainable.t] below. *)
  type ('chain, 'desc) keychainable constraint 'desc = _ * _ * _ * _ * _

  (** Keychain elements are usable as keys into ordered collections. *)
  module Key : Comparator.S

  (** Keychains support iteration over each key in the chain. *)
  module Iterator : Iterator.S0 with type elt = Key.t

  (** The type of a keychain. *)
  type t = Iterator.seq

  (** A phantom type identifying the keychainable implementation. *)
  type keychain_witness

  (** A compound type used in the [Keychainable.t] argument normally hidden by a GADT. *)
  type keychain_description =
    keychain_witness
    * Key.t
    * Key.comparator_witness
    * Iterator.t
    * Iterator.iterator_witness

  (** The implementation of the keychain type. *)
  val keychainable : (t, keychain_description) keychainable
end

(** The implementation of a keychain type. *)
module type Impl = sig
  type keychain_witness

  (** Keychain elements must be usable as keys into ordered collections. *)
  module Key : Comparator.S

  (** Represents a keychain containing [Key.t] elements. *)
  type t [@@deriving sexp_of]

  (** Implements iteration over the keys of a keychain. *)
  module Iterator : Iterator.S0 with type seq = t and type elt = Key.t

  (** Constructs a keychain from keys. Proceeds in reverse order because it is most often
      applied to accumulators that are built up as stacks. *)
  val of_rev_keys : Key.t list -> t
end

(** Used to implement keychains via conversion to and from lists. *)
module type Listable = sig
  type elt
  type t [@@deriving sexp_of]

  val to_list : t -> elt list
  val of_list : elt list -> t
end

module type Keychainable = sig
  module type Impl = Impl
  module type Listable = Listable

  (** A first-class module implementing a keychain type, wrapped in a GADT to combine
      several type parameters that are irrelevant to most users.

      This type is [private] so that users can only construct it via [Make] and other
      related functors below. *)
  type ('chain, 'desc) t =
    private
    (module Impl
       with type t = 'chain
        and type Key.t = 'key
        and type Key.comparator_witness = 'cmp
        and type Iterator.t = 'iter
        and type Iterator.iterator_witness = 'idx
        and type keychain_witness = 'wit)
    constraint 'desc = 'wit * 'key * 'cmp * 'iter * 'idx

  module type S = S with type ('chain, 'desc) keychainable := ('chain, 'desc) t

  (** Accessors into an implementation. *)

  val sexp_of_keychain : ('chain, _) t -> 'chain -> Sexp.t
  val sexp_of_key : (_, _ * 'key * _ * _ * _) t -> 'key -> Sexp.t
  val keychain_of_rev_keys : ('chain, _ * 'key * _ * _ * _) t -> 'key list -> 'chain
  val comparator : (_, _ * 'key * 'cmp * _ * _) t -> ('key, 'cmp) Comparator.t

  val comparator_m
    :  (_, _ * 'key * 'cmp * _ * _) t
    -> (module Comparator.S with type t = 'key and type comparator_witness = 'cmp)

  val iterator
    :  ('chain, _ * 'key * _ * 'iter * 'idx) t
    -> ('iter, 'chain, 'key, 'idx) Iterator.t

  val iterator_m
    :  ('chain, _ * 'key * _ * 'iter * 'idx) t
    -> (module Iterator.S0
          with type t = 'iter
           and type seq = 'chain
           and type elt = 'key
           and type iterator_witness = 'idx)

  val start : ('chain, _ * _ * _ * 'iter * _) t -> 'chain -> 'iter
  val is_finished : ('chain, _ * _ * _ * 'iter * _) t -> 'iter -> 'chain -> bool
  val get_exn : ('chain, _ * 'key * _ * 'iter * _) t -> 'iter -> 'chain -> 'key
  val next_exn : ('chain, _ * _ * _ * 'iter * _) t -> 'iter -> 'chain -> 'iter

  (** Functors and modules implementing keychains. *)

  module Make (Impl : Impl) :
    S
    with module Key = Impl.Key
     and module Iterator = Impl.Iterator
     and type keychain_witness = Impl.keychain_witness

  module Of_string :
    S
    with type Key.t = char
     and type Key.comparator_witness = Char.comparator_witness
     and module Iterator = Iterator.Of_string

  module Of_list (Key : Comparator.S) :
    S
    with module Key = Key
     and module Iterator = Iterator.Monomorphic(Iterator.Of_list)(Key)

  module Of_listable (Key : Comparator.S) (Keychain : Listable with type elt = Key.t) :
    S with module Key = Key and module Iterator = Iterator.Of_listable0(Keychain)
end
