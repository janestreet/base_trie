open! Base
include Keychainable_intf

type ('chain, 'desc) t =
  (module Impl
     with type t = 'chain
      and type Key.t = 'key
      and type Key.comparator_witness = 'cmp
      and type Iterator.t = 'iter
      and type Iterator.iterator_witness = 'idx
      and type keychain_witness = 'wit)
  constraint 'desc = 'wit * 'key * 'cmp * 'iter * 'idx

module type S = S with type ('chain, 'desc) keychainable := ('chain, 'desc) t

let comparator_m
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  (module Chain.Key : Comparator.S with type t = key and type comparator_witness = cmp)
;;

let comparator
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  Chain.Key.comparator
;;

let sexp_of_key t = Comparator.sexp_of_t (comparator t)

let sexp_of_keychain
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  Chain.sexp_of_t
;;

let keychain_of_rev_keys
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  Chain.of_rev_keys
;;

let iterator_m
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  (module Chain.Iterator : Iterator.S0
    with type t = iter
     and type seq = chain
     and type elt = key
     and type iterator_witness = idx)
;;

let iterator
  (type chain wit key cmp iter idx)
  ((module Chain) : (chain, wit * key * cmp * iter * idx) t)
  =
  Chain.Iterator.iterator
;;

let start t = Iterator.start (iterator t)
let is_finished t = Iterator.is_finished (iterator t)
let get_exn t = Iterator.get_exn (iterator t)
let next_exn t = Iterator.next_exn (iterator t)

module Make (Impl : Impl) :
  S
  with module Key = Impl.Key
   and module Iterator = Impl.Iterator
   and type keychain_witness = Impl.keychain_witness = struct
  let keychainable : _ t = (module Impl)

  include Impl

  type keychain_description =
    keychain_witness
    * Key.t
    * Key.comparator_witness
    * Iterator.t
    * Iterator.iterator_witness
end

module Of_string = Make (struct
    type keychain_witness

    module Key = Char

    type t = string [@@deriving sexp_of]

    let of_rev_keys rev_keys = String.of_char_list (List.rev rev_keys)

    module Iterator = Iterator.Of_string
  end)

module Of_list (Key : Comparator.S) = Make (struct
    type keychain_witness

    module Key = Key

    type t = Key.t list

    let sexp_of_t t = sexp_of_list (Comparator.sexp_of_t Key.comparator) t
    let of_rev_keys = List.rev

    module Iterator = Iterator.Monomorphic (Iterator.Of_list) (Key)
  end)

module Of_listable (Key : Comparator.S) (Keychain : Listable with type elt = Key.t) =
Make (struct
    type keychain_witness

    module Key = Key

    type t = Keychain.t [@@deriving sexp_of]

    let of_rev_keys rev_keys = Keychain.of_list (List.rev rev_keys)

    module Iterator = Iterator.Of_listable0 (Keychain)
  end)
