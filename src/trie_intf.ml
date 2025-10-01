(** Represents a trie: https://en.wikipedia.org/wiki/Trie

    Lookup is based on "chains" of keys; each node of the trie has children representing
    each potential next key in the chain. See [Keychainable].

    This interface is modeled after [Base.Map] for element lookup by keychains. It also
    provides trie-node lookup by keychains. *)

open! Base

(** The interface of tries sharing a keychain implementation. *)
module type S = sig
  (** The type of generic tries. See [t] in [Trie], below. *)
  type ('chain, +'data, 'desc) trie constraint 'desc = _ * _ * _ * _ * _

  module Keychain : Keychainable.S

  type 'a t = (Keychain.t, 'a, Keychain.keychain_description) trie [@@deriving sexp_of]
end

module type Trie = sig
  module Keychainable = Keychainable
  module Iterator = Iterator

  (** The type of a trie. Keychains have type ['chain]. The data associated with each
      keychain has type ['data]. The description of the keychain implementation is
      ['desc]; see [Keychainable.t] for details.

      We derive only [sexp_of] for this type. For stable and round-trippable
      serializations, see [Trie_stable]. *)
  type ('chain, +'data, 'desc) t constraint 'desc = _ * _ * _ * _ * _ [@@deriving sexp_of]

  module type S = S with type ('chain, 'data, 'desc) trie := ('chain, 'data, 'desc) t

  (** Unlike [Base.Map], we avoid polymorphic variants for [add*] functions. *)
  module Or_duplicate : sig
    type ('a, 'b) t =
      | Ok of 'a
      | Duplicate of 'b
    [@@deriving sexp_of]
  end

  (** Constructors via comparator. *)

  val empty : ('chain, 'desc) Keychainable.t -> ('chain, _, 'desc) t

  val of_alist
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) list
    -> (('chain, 'data, 'desc) t, 'chain) Or_duplicate.t

  val of_alist_or_error
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) list
    -> ('chain, 'data, 'desc) t Or_error.t

  val of_alist_exn
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) list
    -> ('chain, 'data, 'desc) t

  val of_alist_multi
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) list
    -> ('chain, 'data list, 'desc) t

  val of_sequence
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) Sequence.t
    -> (('chain, 'data, 'desc) t, 'chain) Or_duplicate.t

  val of_sequence_or_error
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) Sequence.t
    -> ('chain, 'data, 'desc) t Or_error.t

  val of_sequence_exn
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) Sequence.t
    -> ('chain, 'data, 'desc) t

  val of_sequence_multi
    :  ('chain, 'desc) Keychainable.t
    -> ('chain * 'data) Sequence.t
    -> ('chain, 'data list, 'desc) t

  (** Accessors at top layer of trie *)

  val create
    :  ('chain, (_ * 'key * 'cmp * _ * _ as 'desc)) Keychainable.t
    -> datum:'data option
    -> tries:('key, ('chain, 'data, 'desc) t, 'cmp) Map.t
    -> ('chain, 'data, 'desc) t

  val datum : ('chain, 'data, _) t -> 'data option

  val tries
    :  ('chain, 'data, (_ * 'key * 'cmp * _ * _ as 'desc)) t
    -> ('key, ('chain, 'data, 'desc) t, 'cmp) Map.t

  val find_child
    :  ('chain, 'data, (_ * 'key * _ * _ * _ as 'desc)) t
    -> 'key
    -> ('chain, 'data, 'desc) t option

  (** Length accessors *)

  val is_empty : _ t -> bool
  val length : _ t -> int

  (** Faster version of [t |> tries |> Map.length]. *)
  val num_children : _ t -> int

  (** Key metadata accessors *)

  val keychainable : ('chain, _, 'desc) t -> ('chain, 'desc) Keychainable.t
  val sexp_of_keychain : ('chain, _, _) t -> 'chain -> Sexp.t
  val sexp_of_key : ('chain, _, _ * 'key * _ * _ * _) t -> 'key -> Sexp.t

  (** Comparisons *)

  [%%template:
  [@@@mode.default m = (global, local)]

  val compare
    :  ('data @ m -> 'data @ m -> int)
    -> ('chain, 'data, 'desc) t @ m
    -> ('chain, 'data, 'desc) t @ m
    -> int

  val equal
    :  ('data @ m -> 'data @ m -> bool)
    -> ('chain, 'data, 'desc) t @ m
    -> ('chain, 'data, 'desc) t @ m
    -> bool]

  (** Serialization *)

  val to_sexp : ('data -> Sexp.t) -> (_, 'data, _) t -> Sexp.t

  (** Accessors for individual elements. *)

  val mem : ('chain, _, _) t -> 'chain -> bool
  val find : ('chain, 'data, _) t -> 'chain -> 'data option
  val find_exn : ('chain, 'data, _) t -> 'chain -> 'data

  val set
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> data:'data
    -> ('chain, 'data, 'desc) t

  val add
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> data:'data
    -> (('chain, 'data, 'desc) t, 'chain) Or_duplicate.t

  val add_or_error
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> data:'data
    -> ('chain, 'data, 'desc) t Or_error.t

  val add_exn
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> data:'data
    -> ('chain, 'data, 'desc) t

  val remove : ('chain, 'data, 'desc) t -> 'chain -> ('chain, 'data, 'desc) t

  val change
    :  ('chain, 'data, 'desc) t
    -> 'chain
    -> f:('data option -> 'data option)
    -> ('chain, 'data, 'desc) t

  val update
    :  ('chain, 'data, 'desc) t
    -> 'chain
    -> f:('data option -> 'data)
    -> ('chain, 'data, 'desc) t

  val add_multi
    :  ('chain, 'data list, 'desc) t
    -> keychain:'chain
    -> data:'data
    -> ('chain, 'data list, 'desc) t

  val remove_multi
    :  ('chain, 'data list, 'desc) t
    -> 'chain
    -> ('chain, 'data list, 'desc) t

  val find_multi : ('chain, 'data list, _) t -> 'chain -> 'data list

  (** Accessors for trie nodes *)

  (** Reports whether there is a non-empty trie at the position of the given chain.
      Equivalently, reports whether the given key is a (non-strict) prefix of any
      element's key. *)
  val mem_trie : ('chain, 'data, 'desc) t -> 'chain -> bool

  (** {v
 Produces the trie at the given position. If [not (mem_trie t keychain)]], returns an
      empty trie.
      v} *)
  val find_trie : ('chain, 'data, 'desc) t -> 'chain -> ('chain, 'data, 'desc) t

  (** Replaces the trie node at the given position, if any, with the given trie. *)
  val set_trie
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> trie:('chain, 'data, 'desc) t
    -> ('chain, 'data, 'desc) t

  (** Adds the given trie node at the given position, or reports a duplicate if there is
      already a non-empty trie there. *)
  val add_trie
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> trie:('chain, 'data, 'desc) t
    -> (('chain, 'data, 'desc) t, 'chain) Or_duplicate.t

  (** Like [add_trie]; returns an [Or_error.t]. *)
  val add_trie_or_error
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> trie:('chain, 'data, 'desc) t
    -> ('chain, 'data, 'desc) t Or_error.t

  (** Like [add_trie]; raises on failure. *)
  val add_trie_exn
    :  ('chain, 'data, 'desc) t
    -> keychain:'chain
    -> trie:('chain, 'data, 'desc) t
    -> ('chain, 'data, 'desc) t

  (** Modifies the trie node at the position of the given chain. *)
  val update_trie
    :  ('chain, 'data, 'desc) t
    -> 'chain
    -> f:(('chain, 'data, 'desc) t -> ('chain, 'data, 'desc) t)
    -> ('chain, 'data, 'desc) t

  (** Traversals *)

  val invariant
    :  'chain Invariant.t
    -> 'data Invariant.t
    -> ('chain, 'data, _) t Invariant.t

  val keychains : ('chain, _, _) t -> 'chain list
  val data : (_, 'data, _) t -> 'data list
  val to_alist : ('chain, 'data, _) t -> ('chain * 'data) list
  val to_sequence : ('chain, 'data, _) t -> ('chain * 'data) Sequence.t
  val iter : (_, 'data, _) t -> f:('data -> unit) -> unit
  val iter_keychains : ('chain, _, _) t -> f:('chain -> unit) -> unit
  val iteri : ('chain, 'data, _) t -> f:(keychain:'chain -> data:'data -> unit) -> unit

  (** Unlike [Base.Map], we distinguish [fold] from [foldi]. For consistency with
      [Container.fold], we take the accumulator argument first in both cases. *)

  val fold : ('chain, 'data, _) t -> init:'acc -> f:('acc -> 'data -> 'acc) -> 'acc

  val foldi
    :  ('chain, 'data, 'desc) t
    -> init:'acc
    -> f:('acc -> keychain:'chain -> data:'data -> 'acc)
    -> 'acc

  (** Calls [f] with [t], plus recursively on descendent [trie]s of [t] where
      [not (is_empty trie)]. Passes [keychain] where [find_trie t keychain = trie]. *)
  val foldi_tries
    :  ('chain, 'data, 'desc) t
    -> init:'acc
    -> f:('acc -> keychain:'chain -> trie:('chain, 'data, 'desc) t -> 'acc)
    -> 'acc

  val map : ('chain, 'a, 'desc) t -> f:('a -> 'b) -> ('chain, 'b, 'desc) t

  val mapi
    :  ('chain, 'a, 'desc) t
    -> f:(keychain:'chain -> data:'a -> 'b)
    -> ('chain, 'b, 'desc) t

  val filter : ('chain, 'data, 'desc) t -> f:('data -> bool) -> ('chain, 'data, 'desc) t

  val filter_keychains
    :  ('chain, 'data, 'desc) t
    -> f:('chain -> bool)
    -> ('chain, 'data, 'desc) t

  val filteri
    :  ('chain, 'data, 'desc) t
    -> f:(keychain:'chain -> data:'data -> bool)
    -> ('chain, 'data, 'desc) t

  val filter_map : ('chain, 'a, 'desc) t -> f:('a -> 'b option) -> ('chain, 'b, 'desc) t

  val filter_mapi
    :  ('chain, 'a, 'desc) t
    -> f:(keychain:'chain -> data:'a -> 'b option)
    -> ('chain, 'b, 'desc) t

  val for_all : (_, 'data, _) t -> f:('data -> bool) -> bool
  val for_alli : ('chain, 'data, _) t -> f:(keychain:'chain -> data:'data -> bool) -> bool
  val exists : (_, 'data, _) t -> f:('data -> bool) -> bool
  val existsi : ('chain, 'data, _) t -> f:(keychain:'chain -> data:'data -> bool) -> bool
  val count : (_, 'data, _) t -> f:('data -> bool) -> int
  val counti : ('chain, 'data, _) t -> f:(keychain:'chain -> data:'data -> bool) -> int

  val partition_tf
    :  ('chain, 'data, 'desc) t
    -> f:('data -> bool)
    -> ('chain, 'data, 'desc) t * ('chain, 'data, 'desc) t

  val partitioni_tf
    :  ('chain, 'data, 'desc) t
    -> f:(keychain:'chain -> data:'data -> bool)
    -> ('chain, 'data, 'desc) t * ('chain, 'data, 'desc) t

  val partition_map
    :  ('chain, 'a, 'desc) t
    -> f:('a -> ('b, 'c) Either.t)
    -> ('chain, 'b, 'desc) t * ('chain, 'c, 'desc) t

  val partition_mapi
    :  ('chain, 'a, 'desc) t
    -> f:(keychain:'chain -> data:'a -> ('b, 'c) Either.t)
    -> ('chain, 'b, 'desc) t * ('chain, 'c, 'desc) t

  val merge
    :  ('chain, 'a, 'desc) t
    -> ('chain, 'b, 'desc) t
    -> f:
         (keychain:'chain
          -> [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ]
          -> 'c option)
    -> ('chain, 'c, 'desc) t

  val merge_skewed
    :  ('chain, 'data, 'desc) t
    -> ('chain, 'data, 'desc) t
    -> combine:(keychain:'chain -> 'data -> 'data -> 'data)
    -> ('chain, 'data, 'desc) t

  (** Modules and functors specializing tries to a keychain implementation. *)

  module Make (Keychain : Keychainable.S) : S with module Keychain = Keychain
  module Of_string : S with module Keychain = Keychainable.Of_string
  module Of_list (Key : Comparator.S) : S with module Keychain = Keychainable.Of_list(Key)

  module Of_listable
      (Key : Comparator.S)
      (Keychain : Keychainable.Listable with type elt = Key.t) :
    S with module Keychain = Keychainable.Of_listable(Key)(Keychain)
end
