open! Base
open Base_quickcheck.Export
open Expect_test_helpers_base
module Iterator = Trie.Iterator

let test iter_m ~f:seq_of_string =
  require_does_not_raise (fun () ->
    Base_quickcheck.Test.run_exn
      ~config:{ Base_quickcheck.Test.default_config with test_count = 100 }
      (module struct
        type t = string [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun string ->
        let seq = seq_of_string string in
        let round_trip_string =
          let rec loop iter acc =
            if Iterator.is_finished iter_m iter seq
            then String.of_char_list (List.rev acc)
            else
              loop
                (Iterator.next_exn iter_m iter seq)
                (Iterator.get_exn iter_m iter seq :: acc)
          in
          loop (Iterator.start iter_m seq) []
        in
        require_equal (module String) string round_trip_string))
;;

module%test _ : module type of Iterator = struct
  module type Impl0 = Iterator.Impl0
  module type Impl1 = Iterator.Impl1
  module type Listable0 = Iterator.Listable0
  module type Listable1 = Iterator.Listable1
  module type S0 = Iterator.S0
  module type S1 = Iterator.S1

  type ('iter, 'seq, 'elt, 'idx) t = ('iter, 'seq, 'elt, 'idx) Iterator.t

  (** accessors are uninteresting *)

  let start = Iterator.start
  let is_finished = Iterator.is_finished
  let get_exn = Iterator.get_exn
  let next_exn = Iterator.next_exn

  (** type conversion is uninteresting **)
  module Monomorphic = Iterator.Monomorphic

  module Make1 = Iterator.Make1

  let%expect_test "Make1" =
    let module M =
      Make1 (struct
        type 'a seq = 'a array
        type 'a elt = 'a
        type _ t = int

        let start _ = 0
        let is_finished pos array = pos >= Array.length array
        let get_exn pos array = array.(pos)
        let next_exn pos _ = pos + 1
      end)
    in
    test M.iterator ~f:String.to_array;
    [%expect {| |}]
  ;;

  module Make0 = Iterator.Make0

  let%expect_test "Make0" =
    let module M =
      Make0 (struct
        type seq = bytes
        type elt = char
        type t = int

        let start _ = 0
        let is_finished pos array = pos >= Bytes.length array
        let get_exn pos array = Bytes.get array pos
        let next_exn pos _ = pos + 1
      end)
    in
    test M.iterator ~f:Bytes.of_string;
    [%expect {| |}]
  ;;

  module Of_listable1 = Iterator.Of_listable1

  let%expect_test "Of_listable1" =
    let module M =
      Of_listable1 (struct
        include Array

        type 'a elt = 'a
      end)
    in
    test M.iterator ~f:String.to_array;
    [%expect {| |}]
  ;;

  module Of_listable0 = Iterator.Of_listable0

  let%expect_test "Of_listable0" =
    let module M =
      Of_listable0 (struct
        include Bytes

        type elt = char
      end)
    in
    test M.iterator ~f:Bytes.of_string;
    [%expect {| |}]
  ;;

  module Of_list = Iterator.Of_list

  let%expect_test "Of_list" =
    test Of_list.iterator ~f:String.to_list;
    [%expect {| |}]
  ;;

  module Of_string = Iterator.Of_string

  let%expect_test "Of_string" =
    test Of_string.iterator ~f:Fn.id;
    [%expect {| |}]
  ;;
end
