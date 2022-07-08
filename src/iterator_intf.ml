(** Implements iterators for arbitrary types, allowing sequential access to arbitrary
    containers. The interface is designed so that multiple kinds of iteration can be
    implemented efficiently. For example, both lists and strings have iterators
    implemented below that do not allocate. *)

open! Base

(** [Impl*] define the interface to the underlying implementation of an iterator. *)

module type Impl1 = sig
  (** Sequential collections. *)
  type 'a seq

  (** Elements of a collection. *)
  type 'a elt

  (** Iterators into a collection. *)
  type 'a t

  (** Creates an iterator at the first element of a collection. *)
  val start : 'a seq -> 'a t

  (** Reports [true] when the iterator is past all elements of a collection. *)
  val is_finished : 'a t -> 'a seq -> bool

  (** Gets the current element of a collection. May raise if [is_finished t]. *)
  val get_exn : 'a t -> 'a seq -> 'a elt

  (** Returns an iterator past the current element. May raise if [is_finished t]. *)
  val next_exn : 'a t -> 'a seq -> 'a t
end

module type Impl0 = sig
  type seq
  type elt
  type t

  include Impl1 with type _ seq := seq and type _ elt := elt and type _ t := t
end

(** [S*] provide the interface to iterators with phantom types. An [Iterator.S0] is like a
    [Comparator.S], it carries an implementation of iteration for a specific type and a
    phantom type witnessing that specific implementation. *)

module type S1 = sig
  (** The type of an iterator implementation. See [t] in [Iterator], below. *)
  type ('iter, 'seq, 'elt, 'idx) iterator

  (** Sequential collections. *)
  type 'a seq

  (** Elements of a collection. *)
  type 'a elt

  (** Iterators into a collection. *)
  type 'a t

  (** The phantom type identifying this implementation. *)
  type iterator_witness

  val iterator : ('a t, 'a seq, 'a elt, iterator_witness) iterator
end

module type S0 = sig
  type seq
  type elt
  type t

  include S1 with type _ seq := seq and type _ elt := elt and type _ t := t
end

(** [Listable*] implement types that may be converted to lists. Used to construct
    iterators into these types. *)

module type Listable1 = sig
  type 'a elt
  type 'a t

  val to_list : 'a t -> 'a elt list
end

module type Listable0 = sig
  type elt
  type t

  include Listable1 with type _ elt := elt and type _ t := t
end

module type Iterator = sig
  module type Impl0 = Impl0
  module type Impl1 = Impl1
  module type Listable0 = Listable0
  module type Listable1 = Listable1

  (** Represents an iterator implementation. Iterators have type ['iter]. They iterate
      over sequential collections of type ['seq] containing elements of type ['elt]. The
      implementation is identified by the phantom type ['idx]. *)
  type ('iter, 'seq, 'elt, 'idx) t =
    private
    (module Impl0 with type t = 'iter and type seq = 'seq and type elt = 'elt)

  module type S0 =
    S0 with type ('iter, 'seq, 'elt, 'idx) iterator := ('iter, 'seq, 'elt, 'idx) t

  module type S1 =
    S1 with type ('iter, 'seq, 'elt, 'idx) iterator := ('iter, 'seq, 'elt, 'idx) t

  (** Accessors into the implementation of an iterator. *)

  val start : ('iter, 'seq, _, _) t -> 'seq -> 'iter
  val is_finished : ('iter, 'seq, _, _) t -> 'iter -> 'seq -> bool
  val get_exn : ('iter, 'seq, 'elt, _) t -> 'iter -> 'seq -> 'elt
  val next_exn : ('iter, 'seq, _, _) t -> 'iter -> 'seq -> 'iter

  (** Modules and functors for constructing iterators. *)

  module Make1 (Impl : Impl1) :
    S1
    with type 'a t = 'a Impl.t
     and type 'a seq = 'a Impl.seq
     and type 'a elt = 'a Impl.elt

  module Make0 (Impl : Impl0) :
    S0 with type t = Impl.t and type seq = Impl.seq and type elt = Impl.elt

  module Monomorphic (Iterator : S1) (Elt : T) :
    S0
    with type t = Elt.t Iterator.t
     and type seq = Elt.t Iterator.seq
     and type elt = Elt.t Iterator.elt
     and type iterator_witness = Iterator.iterator_witness

  module Of_string : S0 with type seq = string and type elt = char
  module Of_list : S1 with type 'a seq = 'a list and type 'a elt = 'a
  module Of_listable0 (Seq : Listable0) : S0 with type seq = Seq.t and type elt = Seq.elt

  module Of_listable1 (Seq : Listable1) :
    S1 with type 'a seq = 'a Seq.t and type 'a elt = 'a Seq.elt
end
