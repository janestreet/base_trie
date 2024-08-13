open! Base
include Iterator_intf

type ('iter, 'seq, 'elt, 'idx) t =
  (module Impl0 with type t = 'iter and type seq = 'seq and type elt = 'elt)

module type S0 =
  S0 with type ('iter, 'seq, 'elt, 'idx) iterator := ('iter, 'seq, 'elt, 'idx) t

module type S1 =
  S1 with type ('iter, 'seq, 'elt, 'idx) iterator := ('iter, 'seq, 'elt, 'idx) t

let start (type iter seq elt idx) t =
  let (module Iterate) = (t : (iter, seq, elt, idx) t) in
  Iterate.start
;;

let is_finished (type iter seq elt idx) t =
  let (module Iterate) = (t : (iter, seq, elt, idx) t) in
  Iterate.is_finished
;;

let get_exn (type iter seq elt idx) t =
  let (module Iterate) = (t : (iter, seq, elt, idx) t) in
  Iterate.get_exn
;;

let next_exn (type iter seq elt idx) t =
  let (module Iterate) = (t : (iter, seq, elt, idx) t) in
  Iterate.next_exn
;;

module Make1 (Impl : Impl1) = struct
  type iterator_witness

  let iterator : type a. (a Impl.t, a Impl.seq, a Impl.elt, iterator_witness) t =
    (module struct
      include Impl

      type seq = a Impl.seq
      type elt = a Impl.elt
      type t = a Impl.t
    end)
  ;;

  include Impl
end

module Make0 (Impl : Impl0) = struct
  include Make1 (struct
      include Impl

      type _ seq = Impl.seq
      type _ elt = Impl.elt
      type _ t = Impl.t
    end)

  include Impl
end

module Monomorphic (Iterator : S1) (Elt : T) = struct
  include Iterator

  type seq = Elt.t Iterator.seq
  type elt = Elt.t Iterator.elt
  type t = Elt.t Iterator.t
end

module Of_string = Make0 (struct
    type seq = string
    type elt = char
    type t = int

    let start _ = 0
    let is_finished pos string = pos >= String.length string
    let get_exn pos string = string.[pos]
    let next_exn pos _ = pos + 1
  end)

module Of_listable1 (Seq : Listable1) = Make1 (struct
    type 'a seq = 'a Seq.t
    type 'a elt = 'a Seq.elt
    type 'a t = 'a elt list

    let start seq = Seq.to_list seq
    let is_finished list _ = List.is_empty list
    let get_exn list _ = List.hd_exn list
    let next_exn list _ = List.tl_exn list
  end)

module Of_listable0 (Seq : Listable0) = struct
  include Of_listable1 (struct
      include Seq

      type _ t = Seq.t
      type _ elt = Seq.elt
    end)

  type seq = Seq.t
  type elt = Seq.elt
  type t = Seq.elt list
end

module Of_list = Of_listable1 (struct
    include List

    type 'a elt = 'a
  end)
