open! Base
open Expect_test_helpers_base

(* We make sure to test every function in Trie by mirroring its signature.

   We choose a set of examples that is not very large, but demonstrates a lot of cases
   (e.g. cases that differentiate [for_all] from [exists], [exists] from [existsi], etc.).

   Our tests then print the results of our functions on these examples. We do not do a lot
   of [require]-based testing. If something fails, the test output will change but it
   probably won't signal failure.

   We therefore need to read expect test output for correctness. We make that clearer,
   albeit more verbose, by generally printing out the inputs in each test as well as the
   outputs, even if that means our examples are repeated many times. *)

module Examples = struct
  module T = Trie.Of_list (Int)

  let keychainable = T.Keychain.keychainable
  let comparator_m = Trie.Keychainable.comparator_m keychainable

  type keychain = int list [@@deriving compare, sexp_of]
  type key = int [@@deriving compare, sexp_of]
  type data = string [@@deriving compare ~localize, equal ~localize, sexp_of]

  (* We simulate alist constructors here so that Trie constructors, other than the
     primitive [Trie.create], are not used to test themselves. *)
  let rec simulate_of_alist_generic alist ~f =
    let datum_list, tries_list =
      List.partition_map alist ~f:(fun (keys, data) ->
        match keys with
        | [] -> First data
        | key :: keys -> Second (key, (keys, data)))
    in
    let datum = f datum_list in
    let tries =
      Map.of_alist_multi comparator_m tries_list
      |> Map.map ~f:(simulate_of_alist_generic ~f)
    in
    Trie.create keychainable ~datum ~tries
  ;;

  let simulate_of_alist_exn alist =
    simulate_of_alist_generic alist ~f:(function
      | [] -> None
      | [ data ] -> Some data
      | _ :: _ :: _ -> assert false)
  ;;

  let simulate_of_alist_multi alist =
    simulate_of_alist_generic alist ~f:(fun list ->
      if List.is_empty list then None else Some list)
  ;;

  let small_alist_without_duplicates = [ [], "greetings" ]
  let medium_alist_without_duplicates = [ [ 1 ], "yo"; [ 2; 3 ], "hello" ]

  let large_alist_without_duplicates =
    [ [ 1 ], "hi"; [ 2 ], "hello"; [ 2; 3; 4 ], "hello, world" ]
  ;;

  let alists_without_duplicates =
    [ []
    ; small_alist_without_duplicates
    ; medium_alist_without_duplicates
    ; large_alist_without_duplicates
    ]
  ;;

  let small_alist_with_duplicates = [ [ 1 ], "hi"; [ 1 ], "hello" ]

  let large_alist_with_duplicates =
    let alist = large_alist_without_duplicates in
    alist @ List.Assoc.map alist ~f:String.uppercase
  ;;

  let alists_with_duplicates =
    List.concat
      [ alists_without_duplicates
      ; [ small_alist_with_duplicates; large_alist_with_duplicates ]
      ]
  ;;

  let small_example_trie = simulate_of_alist_exn small_alist_without_duplicates
  let medium_example_trie = simulate_of_alist_exn medium_alist_without_duplicates
  let large_example_trie = simulate_of_alist_exn large_alist_without_duplicates
  let example_tries = List.map alists_without_duplicates ~f:simulate_of_alist_exn
  let large_example_trie_multi = simulate_of_alist_multi large_alist_with_duplicates

  let all_prefixes list =
    List.init (List.length list + 1) ~f:(fun length -> List.take list length)
  ;;

  let example_key_lists =
    let present_keys =
      List.concat_map alists_with_duplicates ~f:(fun alist -> List.map alist ~f:fst)
    in
    let absent_keys = [ [ 100; 200 ] ] in
    let all_keys = present_keys @ absent_keys in
    all_keys
    |> List.concat_map ~f:all_prefixes
    |> List.dedup_and_sort ~compare:[%compare: keychain]
  ;;
end

module%test Trie : module type of struct
  include Trie
end = struct
  (* Start off with definitions we don't bother to test. *)

  module type S = Trie.S

  module Keychainable = Trie.Keychainable
  module Iterator = Trie.Iterator
  module Or_duplicate = Trie.Or_duplicate

  type ('chain, 'data, 'desc) t = ('chain, 'data, 'desc) Trie.t

  let keychainable = Trie.keychainable
  let sexp_of_key = Trie.sexp_of_key
  let sexp_of_keychain = Trie.sexp_of_keychain

  (* These functors are thin wrappers around [Keychainable] functors, which are themselves
     thin wrappers around [Iterator] functors. See [test_iterator.ml]. *)

  module Make = Trie.Make
  module Of_string = Trie.Of_string
  module Of_list = Trie.Of_list
  module Of_listable = Trie.Of_listable

  (* We open Examples here to shadow [sexp_of_key] with the sexp of our key type. *)
  open Examples

  (* Check invariant every time we print a trie. *)

  let sexp_of_t sexp_of_chain sexp_of_data sexp_of_desc trie =
    require_does_not_raise (fun () -> Trie.invariant ignore ignore trie);
    Trie.sexp_of_t sexp_of_chain sexp_of_data sexp_of_desc trie
  ;;

  let to_sexp sexp_of_data trie =
    require_does_not_raise (fun () -> Trie.invariant ignore ignore trie);
    Trie.to_sexp sexp_of_data trie
  ;;

  (* Test every function below. *)

  let invariant = Trie.invariant

  let%expect_test "invariant" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test data_invariant key_invariant =
      show_raise ~hide_positions:true (fun () ->
        invariant data_invariant key_invariant trie)
    in
    test ignore ignore;
    [%expect {| "did not raise" |}];
    test (List.invariant (fun key -> assert (key < 3))) ignore;
    [%expect
      {|
      (raised (
        "invariant failed"
        lib/trie/src/trie.ml:LINE:COL
        (exn (
          "invariant failed"
          lib/trie/src/trie.ml:LINE:COL
          (exn "Assert_failure test_trie.ml:LINE:COL")
          ("problem with trie" ((keychain (2 3 4))))))
        (((1) _)
         ((2) _)
         ((2 3 4) _))))
      |}];
    test ignore (fun data -> assert (not (String.equal data "hello")));
    [%expect
      {|
      (raised (
        "invariant failed"
        lib/trie/src/trie.ml:LINE:COL
        (exn (
          "invariant failed"
          lib/trie/src/trie.ml:LINE:COL
          (exn "Assert_failure test_trie.ml:LINE:COL")
          ("problem with trie" ((keychain (2))))))
        (((1) _)
         ((2) _)
         ((2 3 4) _))))
      |}]
  ;;

  let empty = Trie.empty

  let%expect_test "empty" =
    print_s [%sexp (empty keychainable : data T.t)];
    [%expect {| () |}]
  ;;

  let create = Trie.create

  let%expect_test "create" =
    let t =
      create
        keychainable
        ~datum:(Some "hello")
        ~tries:(Map.singleton comparator_m 0 (empty keychainable))
    in
    print_s [%sexp (t : data T.t)];
    [%expect {| ((() hello)) |}]
  ;;

  let of_alist = Trie.of_alist

  let%expect_test "of_alist" =
    let test alist =
      let trie = of_alist keychainable alist in
      print_s
        [%sexp
          { alist : (keychain * data) list; trie : (data T.t, keychain) Or_duplicate.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((alist ()) (trie (Ok ())))
      ((alist ((() greetings))) (trie (Ok ((() greetings)))))
      ((alist (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((alist (
         ((1) hi)
         ((1) hello)))
       (trie (Duplicate (1))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Duplicate (1))))
      |}]
  ;;

  let of_alist_or_error = Trie.of_alist_or_error

  let%expect_test "of_alist_or_error" =
    let test alist =
      let trie = of_alist_or_error keychainable alist in
      print_s [%sexp { alist : (keychain * data) list; trie : data T.t Or_error.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((alist ()) (trie (Ok ())))
      ((alist ((() greetings))) (trie (Ok ((() greetings)))))
      ((alist (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((alist (
         ((1) hi)
         ((1) hello)))
       (trie (Error ("Trie.of_alist_exn: duplicate keychain" ((keychain (1)))))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Error ("Trie.of_alist_exn: duplicate keychain" ((keychain (1)))))))
      |}]
  ;;

  let of_alist_exn = Trie.of_alist_exn

  let%expect_test "of_alist_exn" =
    let test alist =
      let trie = Or_error.try_with (fun () -> of_alist_exn keychainable alist) in
      print_s [%sexp { alist : (keychain * data) list; trie : data T.t Or_error.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((alist ()) (trie (Ok ())))
      ((alist ((() greetings))) (trie (Ok ((() greetings)))))
      ((alist (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((alist (
         ((1) hi)
         ((1) hello)))
       (trie (Error ("Trie.of_alist_exn: duplicate keychain" ((keychain (1)))))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Error ("Trie.of_alist_exn: duplicate keychain" ((keychain (1)))))))
      |}]
  ;;

  let of_alist_multi = Trie.of_alist_multi

  let%expect_test "of_alist_multi" =
    let test alist =
      let trie = of_alist_multi keychainable alist in
      print_s [%sexp { alist : (keychain * data) list; trie : data list T.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((alist ())
       (trie  ()))
      ((alist ((() greetings))) (trie ((() (greetings)))))
      ((alist (((1) yo) ((2 3) hello)))
       (trie (
         ((1)
          (yo))
         ((2 3) (hello)))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         ((1) (hi))
         ((2) (hello))
         ((2 3 4) ("hello, world")))))
      ((alist (
         ((1) hi)
         ((1) hello)))
       (trie (((1) (hi hello)))))
      ((alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      |}]
  ;;

  let of_sequence = Trie.of_sequence

  let%expect_test "of_sequence" =
    let test alist =
      let sequence = Sequence.of_list alist in
      let trie = of_sequence keychainable sequence in
      print_s
        [%sexp
          { sequence : (keychain * data) Sequence.t
          ; trie : (data T.t, keychain) Or_duplicate.t
          }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((sequence ()) (trie (Ok ())))
      ((sequence ((() greetings))) (trie (Ok ((() greetings)))))
      ((sequence (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((sequence (
         ((1) hi)
         ((1) hello)))
       (trie (Duplicate (1))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Duplicate (1))))
      |}]
  ;;

  let of_sequence_or_error = Trie.of_sequence_or_error

  let%expect_test "of_sequence_or_error" =
    let test alist =
      let sequence = Sequence.of_list alist in
      let trie = of_sequence_or_error keychainable sequence in
      print_s
        [%sexp { sequence : (keychain * data) Sequence.t; trie : data T.t Or_error.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((sequence ()) (trie (Ok ())))
      ((sequence ((() greetings))) (trie (Ok ((() greetings)))))
      ((sequence (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((sequence (
         ((1) hi)
         ((1) hello)))
       (trie (Error ("Trie.of_sequence_exn: duplicate keychain" ((keychain (1)))))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Error ("Trie.of_sequence_exn: duplicate keychain" ((keychain (1)))))))
      |}]
  ;;

  let of_sequence_exn = Trie.of_sequence_exn

  let%expect_test "of_sequence_exn" =
    let test alist =
      let sequence = Sequence.of_list alist in
      let trie = Or_error.try_with (fun () -> of_sequence_exn keychainable sequence) in
      print_s
        [%sexp { sequence : (keychain * data) Sequence.t; trie : data T.t Or_error.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((sequence ()) (trie (Ok ())))
      ((sequence ((() greetings))) (trie (Ok ((() greetings)))))
      ((sequence (((1) yo) ((2 3) hello))) (trie (Ok (((1) yo) ((2 3) hello)))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")))))
      ((sequence (
         ((1) hi)
         ((1) hello)))
       (trie (Error ("Trie.of_sequence_exn: duplicate keychain" ((keychain (1)))))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (Error ("Trie.of_sequence_exn: duplicate keychain" ((keychain (1)))))))
      |}]
  ;;

  let of_sequence_multi = Trie.of_sequence_multi

  let%expect_test "of_sequence_multi" =
    let test alist =
      let sequence = Sequence.of_list alist in
      let trie = of_sequence_multi keychainable sequence in
      print_s [%sexp { sequence : (keychain * data) Sequence.t; trie : data list T.t }]
    in
    List.iter alists_with_duplicates ~f:test;
    [%expect
      {|
      ((sequence ())
       (trie     ()))
      ((sequence ((() greetings))) (trie ((() (greetings)))))
      ((sequence (((1) yo) ((2 3) hello)))
       (trie (
         ((1)
          (yo))
         ((2 3) (hello)))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (trie (
         ((1) (hi))
         ((2) (hello))
         ((2 3 4) ("hello, world")))))
      ((sequence (
         ((1) hi)
         ((1) hello)))
       (trie (((1) (hi hello)))))
      ((sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD")))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      |}]
  ;;

  [%%template
  [@@@mode.default m = (local, global)]

  let equal = (Trie.equal [@mode m])

  let%expect_test "equal" =
    let sexp_of_pair (fst, snd) = [%sexp { fst : data T.t; snd : data T.t }] in
    let test tries =
      let equal, different =
        List.partition_tf (List.cartesian_product tries tries) ~f:(fun (trie1, trie2) ->
          (equal [@mode m]) (equal_data [@mode m]) trie1 trie2)
      in
      print_s [%sexp { equal : pair list; different : pair list }]
    in
    test example_tries;
    [%expect
      {|
      ((equal (
         ((fst ())
          (snd ()))
         ((fst ((() greetings)))
          (snd ((() greetings))))
         ((fst (((1) yo) ((2 3) hello))) (snd (((1) yo) ((2 3) hello))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))))
       (different (
         ((fst ()) (snd ((() greetings))))
         ((fst ()) (snd (((1) yo) ((2 3) hello))))
         ((fst ())
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst ((() greetings))) (snd ()))
         ((fst ((() greetings))) (snd (((1) yo) ((2 3) hello))))
         ((fst ((() greetings)))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst (((1) yo) ((2 3) hello))) (snd ()))
         ((fst (((1) yo) ((2 3) hello))) (snd ((() greetings))))
         ((fst (((1) yo) ((2 3) hello)))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd ()))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd ((() greetings))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd (((1) yo) ((2 3) hello)))))))
      |}]
  ;;

  let compare = (Trie.compare [@mode m])

  let%expect_test "compare" =
    let sexp_of_pair (fst, snd) = [%sexp { fst : data T.t; snd : data T.t }] in
    let test tries =
      let less_than, equal_to, greater_than =
        List.partition3_map
          (List.cartesian_product tries tries)
          ~f:(fun ((trie1, trie2) as pair) ->
            let c = (compare [@mode m]) (compare_data [@mode m]) trie1 trie2 in
            if c < 0 then `Fst pair else if c = 0 then `Snd pair else `Trd pair)
      in
      print_s
        [%sexp { less_than : pair list; equal_to : pair list; greater_than : pair list }]
    in
    test example_tries;
    [%expect
      {|
      ((less_than (
         ((fst ()) (snd ((() greetings))))
         ((fst ()) (snd (((1) yo) ((2 3) hello))))
         ((fst ())
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst (((1) yo) ((2 3) hello))) (snd ((() greetings))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd ((() greetings))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd (((1) yo) ((2 3) hello))))))
       (equal_to (
         ((fst ())
          (snd ()))
         ((fst ((() greetings)))
          (snd ((() greetings))))
         ((fst (((1) yo) ((2 3) hello))) (snd (((1) yo) ((2 3) hello))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))))
       (greater_than (
         ((fst ((() greetings))) (snd ()))
         ((fst ((() greetings))) (snd (((1) yo) ((2 3) hello))))
         ((fst ((() greetings)))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst (((1) yo) ((2 3) hello))) (snd ()))
         ((fst (((1) yo) ((2 3) hello)))
          (snd (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world"))))
         ((fst (
            ((1) hi)
            ((2) hello)
            ((2 3 4) "hello, world")))
          (snd ())))))
      |}]
  ;;]

  let is_empty = Trie.is_empty

  let%expect_test "is_empty" =
    let test trie =
      let is_empty = is_empty trie in
      print_s [%sexp { is_empty : bool; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((is_empty true) (trie ()))
      ((is_empty false) (trie ((() greetings))))
      ((is_empty false) (trie (((1) yo) ((2 3) hello))))
      ((is_empty false)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let length = Trie.length

  let%expect_test "length" =
    let test trie =
      let length = length trie in
      print_s [%sexp { length : int; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((length 0) (trie ()))
      ((length 1) (trie ((() greetings))))
      ((length 2) (trie (((1) yo) ((2 3) hello))))
      ((length 3)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let datum = Trie.datum

  let%expect_test "datum" =
    let test trie =
      let datum = datum trie in
      print_s [%sexp { datum : data option; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((datum ())
       (trie  ()))
      ((datum (greetings)) (trie ((() greetings))))
      ((datum ()) (trie (((1) yo) ((2 3) hello))))
      ((datum ())
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let tries = Trie.tries

  let%expect_test "tries" =
    let test trie =
      let tries = tries trie in
      print_s [%sexp { tries : data T.t Map.M(Int).t; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((tries ())
       (trie  ()))
      ((tries ()) (trie ((() greetings))))
      ((tries ((1 ((() yo))) (2 (((3) hello))))) (trie (((1) yo) ((2 3) hello))))
      ((tries ((1 ((() hi))) (2 ((() hello) ((3 4) "hello, world")))))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let num_children = Trie.num_children

  let%expect_test "num_children" =
    let test trie =
      let num_children = num_children trie in
      print_s [%sexp { num_children : int; trie : data T.t }];
      require_equal (module Int) num_children (Map.length (tries trie))
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((num_children 0) (trie ()))
      ((num_children 0) (trie ((() greetings))))
      ((num_children 2) (trie (((1) yo) ((2 3) hello))))
      ((num_children 2)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let find_child = Trie.find_child

  let%expect_test "find_child" =
    let test trie =
      let child = find_child trie 2 in
      print_s [%sexp { child : data T.t option; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((child ())
       (trie  ()))
      ((child ()) (trie ((() greetings))))
      ((child ((((3) hello)))) (trie (((1) yo) ((2 3) hello))))
      ((child (((() hello) ((3 4) "hello, world"))))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let mem = Trie.mem

  let%expect_test "mem" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let present = mem trie keychain in
      print_s [%sexp { keychain : keychain; present : bool }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ()) (present false))
      ((keychain (1)) (present true))
      ((keychain (2)) (present true))
      ((keychain (2 3)) (present false))
      ((keychain (2 3 4)) (present true))
      ((keychain (100)) (present false))
      ((keychain (100 200)) (present false))
      |}]
  ;;

  let find = Trie.find

  let%expect_test "find" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let data = find trie keychain in
      print_s [%sexp { keychain : keychain; data : data option }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (data     ()))
      ((keychain (1))
       (data     (hi)))
      ((keychain (2))
       (data     (hello)))
      ((keychain (2 3)) (data ()))
      ((keychain (2 3 4)) (data ("hello, world")))
      ((keychain (100)) (data ()))
      ((keychain (100 200)) (data ()))
      |}]
  ;;

  let find_exn = Trie.find_exn

  let%expect_test "find_exn" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let data = Or_error.try_with (fun () -> find_exn trie keychain) in
      print_s [%sexp { keychain : keychain; data : data Or_error.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (data (Error ("Trie.find_exn: keychain not found" ((keychain ()))))))
      ((keychain (1)) (data (Ok hi)))
      ((keychain (2)) (data (Ok hello)))
      ((keychain (2 3))
       (data (Error ("Trie.find_exn: keychain not found" ((keychain (2 3)))))))
      ((keychain (2 3 4)) (data (Ok "hello, world")))
      ((keychain (100))
       (data (Error ("Trie.find_exn: keychain not found" ((keychain (100)))))))
      ((keychain (100 200))
       (data (Error ("Trie.find_exn: keychain not found" ((keychain (100 200)))))))
      |}]
  ;;

  let set = Trie.set

  let%expect_test "set" =
    let trie = medium_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect {| (((1) yo) ((2 3) hello)) |}];
    let test ~keychain ~data =
      let trie = set trie ~keychain ~data in
      print_s [%sexp { keychain : keychain; data : data; trie : data T.t }]
    in
    List.iter example_key_lists ~f:(fun keychain -> test ~keychain ~data:"salutations");
    [%expect
      {|
      ((keychain ())
       (data salutations)
       (trie ((() salutations) ((1) yo) ((2 3) hello))))
      ((keychain (1)) (data salutations) (trie (((1) salutations) ((2 3) hello))))
      ((keychain (2))
       (data salutations)
       (trie (
         ((1) yo)
         ((2) salutations)
         ((2 3) hello))))
      ((keychain (2 3)) (data salutations) (trie (((1) yo) ((2 3) salutations))))
      ((keychain (2 3 4))
       (data salutations)
       (trie (((1) yo) ((2 3) hello) ((2 3 4) salutations))))
      ((keychain (100))
       (data salutations)
       (trie (((1) yo) ((2 3) hello) ((100) salutations))))
      ((keychain (100 200))
       (data salutations)
       (trie (
         ((1) yo)
         ((2   3)   hello)
         ((100 200) salutations))))
      |}]
  ;;

  let add = Trie.add

  let%expect_test "add" =
    let trie = medium_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect {| (((1) yo) ((2 3) hello)) |}];
    let test ~keychain ~data =
      let trie = add trie ~keychain ~data in
      print_s
        [%sexp
          { keychain : keychain; data : data; trie : (data T.t, keychain) Or_duplicate.t }]
    in
    List.iter example_key_lists ~f:(fun keychain -> test ~keychain ~data:"salutations");
    [%expect
      {|
      ((keychain ())
       (data salutations)
       (trie (Ok ((() salutations) ((1) yo) ((2 3) hello)))))
      ((keychain (1)) (data salutations) (trie (Duplicate (1))))
      ((keychain (2))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2) salutations)
           ((2 3) hello)))))
      ((keychain (2 3)) (data salutations) (trie (Duplicate (2 3))))
      ((keychain (2 3 4))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((2 3 4) salutations)))))
      ((keychain (100))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((100) salutations)))))
      ((keychain (100 200))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2   3)   hello)
           ((100 200) salutations)))))
      |}]
  ;;

  let add_or_error = Trie.add_or_error

  let%expect_test "add_or_error" =
    let trie = medium_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect {| (((1) yo) ((2 3) hello)) |}];
    let test ~keychain ~data =
      let trie = add_or_error trie ~keychain ~data in
      print_s [%sexp { keychain : keychain; data : data; trie : data T.t Or_error.t }]
    in
    List.iter example_key_lists ~f:(fun keychain -> test ~keychain ~data:"salutations");
    [%expect
      {|
      ((keychain ())
       (data salutations)
       (trie (Ok ((() salutations) ((1) yo) ((2 3) hello)))))
      ((keychain (1))
       (data salutations)
       (trie (Error ("Trie.add_exn: duplicate keychain" ((keychain (1)))))))
      ((keychain (2))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2) salutations)
           ((2 3) hello)))))
      ((keychain (2 3))
       (data salutations)
       (trie (Error ("Trie.add_exn: duplicate keychain" ((keychain (2 3)))))))
      ((keychain (2 3 4))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((2 3 4) salutations)))))
      ((keychain (100))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((100) salutations)))))
      ((keychain (100 200))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2   3)   hello)
           ((100 200) salutations)))))
      |}]
  ;;

  let add_exn = Trie.add_exn

  let%expect_test "add_exn" =
    let trie = medium_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect {| (((1) yo) ((2 3) hello)) |}];
    let test ~keychain ~data =
      let trie = Or_error.try_with (fun () -> add_exn trie ~keychain ~data) in
      print_s [%sexp { keychain : keychain; data : data; trie : data T.t Or_error.t }]
    in
    List.iter example_key_lists ~f:(fun keychain -> test ~keychain ~data:"salutations");
    [%expect
      {|
      ((keychain ())
       (data salutations)
       (trie (Ok ((() salutations) ((1) yo) ((2 3) hello)))))
      ((keychain (1))
       (data salutations)
       (trie (Error ("Trie.add_exn: duplicate keychain" ((keychain (1)))))))
      ((keychain (2))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2) salutations)
           ((2 3) hello)))))
      ((keychain (2 3))
       (data salutations)
       (trie (Error ("Trie.add_exn: duplicate keychain" ((keychain (2 3)))))))
      ((keychain (2 3 4))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((2 3 4) salutations)))))
      ((keychain (100))
       (data salutations)
       (trie (Ok (((1) yo) ((2 3) hello) ((100) salutations)))))
      ((keychain (100 200))
       (data salutations)
       (trie (
         Ok (
           ((1) yo)
           ((2   3)   hello)
           ((100 200) salutations)))))
      |}]
  ;;

  let remove = Trie.remove

  let%expect_test "remove" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let trie = remove trie keychain in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (1)) (trie (((2) hello) ((2 3 4) "hello, world"))))
      ((keychain (2)) (trie (((1) hi) ((2 3 4) "hello, world"))))
      ((keychain (2 3))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (2 3 4))
       (trie (
         ((1) hi)
         ((2) hello))))
      ((keychain (100))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (100 200))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let change = Trie.change

  let%expect_test "change" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let trie =
        change trie keychain ~f:(function
          | None -> Some "Salutations!"
          | Some "hi" -> None
          | Some other -> Some (String.capitalize other ^ "!"))
      in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         (() Salutations!)
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (1)) (trie (((2) hello) ((2 3 4) "hello, world"))))
      ((keychain (2))
       (trie (
         ((1) hi)
         ((2) Hello!)
         ((2 3 4) "hello, world"))))
      ((keychain (2 3))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3) Salutations!)
         ((2 3 4) "hello, world"))))
      ((keychain (2 3 4))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "Hello, world!"))))
      ((keychain (100))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100) Salutations!))))
      ((keychain (100 200))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100 200) Salutations!))))
      |}]
  ;;

  let update = Trie.update

  let%expect_test "update" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let trie =
        update trie keychain ~f:(function
          | None -> "Salutations!"
          | Some other -> String.capitalize other ^ "!")
      in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         (() Salutations!)
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (1))
       (trie (
         ((1) Hi!)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (2))
       (trie (
         ((1) hi)
         ((2) Hello!)
         ((2 3 4) "hello, world"))))
      ((keychain (2 3))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3) Salutations!)
         ((2 3 4) "hello, world"))))
      ((keychain (2 3 4))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "Hello, world!"))))
      ((keychain (100))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100) Salutations!))))
      ((keychain (100 200))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100 200) Salutations!))))
      |}]
  ;;

  let add_multi = Trie.add_multi

  let%expect_test "add_multi" =
    let trie = large_example_trie_multi in
    print_s [%sexp (trie : data list T.t)];
    [%expect
      {|
      (((1) (hi    HI))
       ((2) (hello HELLO))
       ((2 3 4) ("hello, world" "HELLO, WORLD")))
      |}];
    let test keychain =
      let trie = add_multi trie ~keychain ~data:"Salutations" in
      print_s [%sexp { keychain : keychain; trie : data list T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         (() (Salutations))
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (1))
       (trie (
         ((1) (Salutations hi HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2))
       (trie (
         ((1) (hi HI))
         ((2) (Salutations hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2 3))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3) (Salutations))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2 3 4))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2           3              4)
          (Salutations "hello, world" "HELLO, WORLD")))))
      ((keychain (100))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD"))
         ((100)
          (Salutations)))))
      ((keychain (100 200))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD"))
         ((100 200) (Salutations)))))
      |}]
  ;;

  let remove_multi = Trie.remove_multi

  let%expect_test "remove_multi" =
    let trie = large_example_trie_multi in
    print_s [%sexp (trie : data list T.t)];
    [%expect
      {|
      (((1) (hi    HI))
       ((2) (hello HELLO))
       ((2 3 4) ("hello, world" "HELLO, WORLD")))
      |}];
    let test keychain =
      let trie = remove_multi trie keychain in
      print_s [%sexp { keychain : keychain; trie : data list T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (1))
       (trie (
         ((1)
          (HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2))
       (trie (
         ((1) (hi HI))
         ((2)
          (HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2 3))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (2 3 4))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("HELLO, WORLD")))))
      ((keychain (100))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      ((keychain (100 200))
       (trie (
         ((1) (hi    HI))
         ((2) (hello HELLO))
         ((2 3 4) ("hello, world" "HELLO, WORLD")))))
      |}]
  ;;

  let find_multi = Trie.find_multi

  let%expect_test "find_multi" =
    let trie = large_example_trie_multi in
    print_s [%sexp (trie : data list T.t)];
    [%expect
      {|
      (((1) (hi    HI))
       ((2) (hello HELLO))
       ((2 3 4) ("hello, world" "HELLO, WORLD")))
      |}];
    let test keychain =
      let data = find_multi trie keychain in
      print_s [%sexp { keychain : keychain; data : data list }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (data     ()))
      ((keychain (1)) (data (hi HI)))
      ((keychain (2)) (data (hello HELLO)))
      ((keychain (2 3)) (data ()))
      ((keychain (2 3 4)) (data ("hello, world" "HELLO, WORLD")))
      ((keychain (100)) (data ()))
      ((keychain (100 200)) (data ()))
      |}]
  ;;

  let mem_trie = Trie.mem_trie

  let%expect_test "mem_trie" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let present = mem_trie trie keychain in
      print_s [%sexp { keychain : keychain; present : bool }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ()) (present true))
      ((keychain (1)) (present true))
      ((keychain (2)) (present true))
      ((keychain (2 3)) (present true))
      ((keychain (2 3 4)) (present true))
      ((keychain (100)) (present false))
      ((keychain (100 200)) (present false))
      |}]
  ;;

  let find_trie = Trie.find_trie

  let%expect_test "find_trie" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let trie = find_trie trie keychain in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (1)) (trie ((() hi))))
      ((keychain (2)) (trie ((() hello) ((3 4) "hello, world"))))
      ((keychain (2 3)) (trie (((4) "hello, world"))))
      ((keychain (2 3 4)) (trie ((() "hello, world"))))
      ((keychain (100)) (trie ()))
      ((keychain (100 200)) (trie ()))
      |}]
  ;;

  let set_trie = Trie.set_trie

  let%expect_test "set_trie" =
    print_s [%sexp (large_example_trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    print_s [%sexp (small_example_trie : data T.t)];
    [%expect {| ((() greetings)) |}];
    let test keychain =
      let trie = set_trie large_example_trie ~keychain ~trie:small_example_trie in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ()) (trie ((() greetings))))
      ((keychain (1))
       (trie (
         ((1) greetings)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((keychain (2))
       (trie (
         ((1) hi)
         ((2) greetings))))
      ((keychain (2 3))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3) greetings))))
      ((keychain (2 3 4))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) greetings))))
      ((keychain (100))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100) greetings))))
      ((keychain (100 200))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100 200) greetings))))
      |}]
  ;;

  let add_trie = Trie.add_trie

  let%expect_test "add_trie" =
    print_s [%sexp (large_example_trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    print_s [%sexp (small_example_trie : data T.t)];
    [%expect {| ((() greetings)) |}];
    let test keychain =
      let trie = add_trie large_example_trie ~keychain ~trie:small_example_trie in
      print_s [%sexp { keychain : keychain; trie : (data T.t, keychain) Or_duplicate.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ()) (trie (Duplicate ())))
      ((keychain (1)) (trie (Duplicate (1))))
      ((keychain (2)) (trie (Duplicate (2))))
      ((keychain (2 3)) (trie (Duplicate (2 3))))
      ((keychain (2 3 4)) (trie (Duplicate (2 3 4))))
      ((keychain (100))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100) greetings)))))
      ((keychain (100 200))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100 200) greetings)))))
      |}]
  ;;

  let add_trie_or_error = Trie.add_trie_or_error

  let%expect_test "add_trie_or_error" =
    print_s [%sexp (large_example_trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    print_s [%sexp (small_example_trie : data T.t)];
    [%expect {| ((() greetings)) |}];
    let test keychain =
      let trie =
        add_trie_or_error large_example_trie ~keychain ~trie:small_example_trie
      in
      print_s [%sexp { keychain : keychain; trie : data T.t Or_error.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain ()))))))
      ((keychain (1))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (1)))))))
      ((keychain (2))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2)))))))
      ((keychain (2 3))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2 3)))))))
      ((keychain (2 3 4))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2 3 4)))))))
      ((keychain (100))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100) greetings)))))
      ((keychain (100 200))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100 200) greetings)))))
      |}]
  ;;

  let add_trie_exn = Trie.add_trie_exn

  let%expect_test "add_trie_exn" =
    print_s [%sexp (large_example_trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    print_s [%sexp (small_example_trie : data T.t)];
    [%expect {| ((() greetings)) |}];
    let test keychain =
      let trie =
        Or_error.try_with (fun () ->
          add_trie_exn large_example_trie ~keychain ~trie:small_example_trie)
      in
      print_s [%sexp { keychain : keychain; trie : data T.t Or_error.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain ()))))))
      ((keychain (1))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (1)))))))
      ((keychain (2))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2)))))))
      ((keychain (2 3))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2 3)))))))
      ((keychain (2 3 4))
       (trie (Error ("Trie.add_trie_exn: duplicate keychain" ((keychain (2 3 4)))))))
      ((keychain (100))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100) greetings)))))
      ((keychain (100 200))
       (trie (
         Ok (
           ((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world")
           ((100 200) greetings)))))
      |}]
  ;;

  let update_trie = Trie.update_trie

  let%expect_test "update_trie" =
    let trie = large_example_trie in
    print_s [%sexp (trie : data T.t)];
    [%expect
      {|
      (((1) hi)
       ((2) hello)
       ((2 3 4) "hello, world"))
      |}];
    let test keychain =
      let trie =
        update_trie trie keychain ~f:(fun trie ->
          create
            keychainable
            ~datum:(Some "update")
            ~tries:(Map.singleton comparator_m 0 trie))
      in
      print_s [%sexp { keychain : keychain; trie : data T.t }]
    in
    List.iter example_key_lists ~f:test;
    [%expect
      {|
      ((keychain ())
       (trie (
         (() update)
         ((0 1) hi)
         ((0 2) hello)
         ((0 2 3 4) "hello, world"))))
      ((keychain (1))
       (trie (((1) update) ((1 0) hi) ((2) hello) ((2 3 4) "hello, world"))))
      ((keychain (2))
       (trie (
         ((1) hi)
         ((2) update)
         ((2 0) hello)
         ((2 0 3 4) "hello, world"))))
      ((keychain (2 3))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3) update)
         ((2 3 0 4) "hello, world"))))
      ((keychain (2 3 4))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) update)
         ((2 3 4 0) "hello, world"))))
      ((keychain (100))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100) update))))
      ((keychain (100 200))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")
         ((100 200) update))))
      |}]
  ;;

  let keychains = Trie.keychains

  let%expect_test "keys" =
    let test trie =
      let keychains = keychains trie in
      print_s [%sexp { keychains : keychain list; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((keychains ())
       (trie      ()))
      ((keychains (())) (trie ((() greetings))))
      ((keychains ((1) (2 3))) (trie (((1) yo) ((2 3) hello))))
      ((keychains (
         (1)
         (2)
         (2 3 4)))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let data = Trie.data

  let%expect_test "data" =
    let test trie =
      let data = data trie in
      print_s [%sexp { data : data list; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((data ())
       (trie ()))
      ((data (greetings)) (trie ((() greetings))))
      ((data (yo hello)) (trie (((1) yo) ((2 3) hello))))
      ((data (hi hello "hello, world"))
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let to_alist = Trie.to_alist

  let%expect_test "to_alist" =
    let test trie =
      let alist = to_alist trie in
      print_s [%sexp { trie : data T.t; alist : (keychain * data) list }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie  ())
       (alist ()))
      ((trie  ((() greetings)))
       (alist ((() greetings))))
      ((trie (((1) yo) ((2 3) hello))) (alist (((1) yo) ((2 3) hello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (alist (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let to_sequence = Trie.to_sequence

  let%expect_test "to_sequence" =
    let test trie =
      let sequence = to_sequence trie in
      print_s [%sexp { trie : data T.t; sequence : (keychain * data) Sequence.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (sequence ()))
      ((trie     ((() greetings)))
       (sequence ((() greetings))))
      ((trie (((1) yo) ((2 3) hello))) (sequence (((1) yo) ((2 3) hello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (sequence (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let iter = Trie.iter

  let%expect_test "iter" =
    let test i trie =
      if i > 0 then print_endline "--";
      print_s [%sexp { trie : data T.t }];
      iter trie ~f:(fun data -> print_s [%sexp (data : data)])
    in
    List.iteri example_tries ~f:test;
    [%expect
      {|
      ((trie ()))
      --
      ((trie ((() greetings))))
      greetings
      --
      ((trie (((1) yo) ((2 3) hello))))
      yo
      hello
      --
      ((
        trie (
          ((1) hi)
          ((2) hello)
          ((2 3 4) "hello, world"))))
      hi
      hello
      "hello, world"
      |}]
  ;;

  let iter_keychains = Trie.iter_keychains

  let%expect_test "iter_keychains" =
    let test i trie =
      if i > 0 then print_endline "--";
      print_s [%sexp { trie : data T.t }];
      iter_keychains trie ~f:(fun keychain -> print_s [%sexp (keychain : keychain)])
    in
    List.iteri example_tries ~f:test;
    [%expect
      {|
      ((trie ()))
      --
      ((trie ((() greetings))))
      ()
      --
      ((trie (((1) yo) ((2 3) hello))))
      (1)
      (2 3)
      --
      ((
        trie (
          ((1) hi)
          ((2) hello)
          ((2 3 4) "hello, world"))))
      (1)
      (2)
      (2 3 4)
      |}]
  ;;

  let iteri = Trie.iteri

  let%expect_test "iteri" =
    let test i trie =
      if i > 0 then print_endline "--";
      print_s [%sexp { trie : data T.t }];
      iteri trie ~f:(fun ~keychain ~data ->
        print_s [%sexp { keychain : keychain; data : data }])
    in
    List.iteri example_tries ~f:test;
    [%expect
      {|
      ((trie ()))
      --
      ((trie ((() greetings))))
      ((keychain ()) (data greetings))
      --
      ((trie (((1) yo) ((2 3) hello))))
      ((keychain (1)) (data yo))
      ((keychain (2 3)) (data hello))
      --
      ((
        trie (
          ((1) hi)
          ((2) hello)
          ((2 3 4) "hello, world"))))
      ((keychain (1)) (data hi))
      ((keychain (2)) (data hello))
      ((keychain (2 3 4)) (data "hello, world"))
      |}]
  ;;

  let fold = Trie.fold

  let%expect_test "fold" =
    let test trie =
      let string = fold trie ~init:"..." ~f:(fun acc data -> data ^ "; " ^ acc) in
      print_s [%sexp { trie : data T.t; string : string }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie ()) (string ...))
      ((trie ((() greetings))) (string "greetings; ..."))
      ((trie (((1) yo) ((2 3) hello))) (string "hello; yo; ..."))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (string "hello, world; hello; hi; ..."))
      |}]
  ;;

  let foldi = Trie.foldi

  let%expect_test "foldi" =
    let test trie =
      let string =
        foldi trie ~init:"..." ~f:(fun acc ~keychain ~data ->
          Printf.sprintf
            "%s=%s; %s"
            (String.concat ~sep:"." (List.map keychain ~f:Int.to_string))
            data
            acc)
      in
      print_s [%sexp { trie : data T.t; string : string }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie ()) (string ...))
      ((trie ((() greetings))) (string "=greetings; ..."))
      ((trie (((1) yo) ((2 3) hello))) (string "2.3=hello; 1=yo; ..."))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (string "2.3.4=hello, world; 2=hello; 1=hi; ..."))
      |}]
  ;;

  let foldi_tries = Trie.foldi_tries

  let%expect_test "foldi_tries" =
    let test trie =
      let alist =
        foldi_tries trie ~init:[] ~f:(fun acc ~keychain ~trie -> (keychain, trie) :: acc)
      in
      print_s [%sexp { trie : data T.t; alist : (keychain * data T.t) list }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie ())
       (alist ((
         ()
         ()))))
      ((trie ((() greetings))) (alist ((() ((() greetings))))))
      ((trie (((1) yo) ((2 3) hello)))
       (alist (
         ((2 3) ((() hello)))
         ((2) (((3) hello)))
         ((1) ((() yo)))
         (() (((1) yo) ((2 3) hello))))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (alist (
         ((2 3 4) ((() "hello, world")))
         ((2 3) (((4) "hello, world")))
         ((2) ((() hello) ((3 4) "hello, world")))
         ((1) ((() hi)))
         (()
          (((1) hi)
           ((2) hello)
           ((2 3 4) "hello, world"))))))
      |}]
  ;;

  let map = Trie.map

  let%expect_test "map" =
    let test trie =
      let mapped = map trie ~f:String.uppercase in
      print_s [%sexp { trie : data T.t; mapped : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie   ())
       (mapped ()))
      ((trie   ((() greetings)))
       (mapped ((() GREETINGS))))
      ((trie (((1) yo) ((2 3) hello))) (mapped (((1) YO) ((2 3) HELLO))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (mapped (
         ((1) HI)
         ((2) HELLO)
         ((2 3 4) "HELLO, WORLD"))))
      |}]
  ;;

  let mapi = Trie.mapi

  let%expect_test "mapi" =
    let test trie =
      let mapped =
        mapi trie ~f:(fun ~keychain ~data ->
          Printf.sprintf "%s @ depth %d" data (List.length keychain))
      in
      print_s [%sexp { trie : data T.t; mapped : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie   ())
       (mapped ()))
      ((trie   ((() greetings)))
       (mapped ((() "greetings @ depth 0"))))
      ((trie (((1) yo) ((2 3) hello)))
       (mapped (((1) "yo @ depth 1") ((2 3) "hello @ depth 2"))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (mapped (
         ((1) "hi @ depth 1")
         ((2) "hello @ depth 1")
         ((2 3 4) "hello, world @ depth 3"))))
      |}]
  ;;

  let filter = Trie.filter

  let%expect_test "filter" =
    let test trie =
      let filtered = filter trie ~f:(fun string -> String.length string % 2 = 0) in
      print_s [%sexp { trie : data T.t; filtered : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (filtered ()))
      ((trie ((() greetings))) (filtered ()))
      ((trie (((1) yo) ((2 3) hello))) (filtered (((1) yo))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (filtered (((1) hi) ((2 3 4) "hello, world"))))
      |}]
  ;;

  let filter_keychains = Trie.filter_keychains

  let%expect_test "filter_keychains" =
    let test trie =
      let filtered =
        filter_keychains trie ~f:(fun keychain -> List.length keychain % 2 = 0)
      in
      print_s [%sexp { trie : data T.t; filtered : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (filtered ()))
      ((trie     ((() greetings)))
       (filtered ((() greetings))))
      ((trie (((1) yo) ((2 3) hello))) (filtered (((2 3) hello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (filtered ()))
      |}]
  ;;

  let filteri = Trie.filteri

  let%expect_test "filteri" =
    let test trie =
      let filtered =
        filteri trie ~f:(fun ~keychain ~data ->
          List.length keychain % 2 = 0 || String.length data % 5 = 0)
      in
      print_s [%sexp { trie : data T.t; filtered : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (filtered ()))
      ((trie     ((() greetings)))
       (filtered ((() greetings))))
      ((trie (((1) yo) ((2 3) hello))) (filtered (((2 3) hello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (filtered (((2) hello))))
      |}]
  ;;

  let filter_map = Trie.filter_map

  let%expect_test "filter_map" =
    let test trie =
      let filtered =
        filter_map trie ~f:(fun data -> String.chop_prefix data ~prefix:"h")
      in
      print_s [%sexp { trie : data T.t; filtered : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (filtered ()))
      ((trie ((() greetings))) (filtered ()))
      ((trie (((1) yo) ((2 3) hello))) (filtered (((2 3) ello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (filtered (
         ((1) i)
         ((2) ello)
         ((2 3 4) "ello, world"))))
      |}]
  ;;

  let filter_mapi = Trie.filter_mapi

  let%expect_test "filter_mapi" =
    let test trie =
      let filtered =
        filter_mapi trie ~f:(fun ~keychain ~data ->
          if List.length keychain % 2 = 0
          then String.chop_prefix data ~prefix:"h"
          else None)
      in
      print_s [%sexp { trie : data T.t; filtered : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie     ())
       (filtered ()))
      ((trie ((() greetings))) (filtered ()))
      ((trie (((1) yo) ((2 3) hello))) (filtered (((2 3) ello))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (filtered ()))
      |}]
  ;;

  let for_all = Trie.for_all

  let%expect_test "for_all" =
    let test trie =
      let success = for_all trie ~f:(fun data -> String.is_prefix data ~prefix:"h") in
      print_s [%sexp { success : bool; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((success true) (trie ()))
      ((success false) (trie ((() greetings))))
      ((success false) (trie (((1) yo) ((2 3) hello))))
      ((success true)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let for_alli = Trie.for_alli

  let%expect_test "for_alli" =
    let test trie =
      let success =
        for_alli trie ~f:(fun ~keychain ~data ->
          List.length keychain % 2 = 0 && String.is_prefix data ~prefix:"h")
      in
      print_s [%sexp { success : bool; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((success true) (trie ()))
      ((success false) (trie ((() greetings))))
      ((success false) (trie (((1) yo) ((2 3) hello))))
      ((success false)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let exists = Trie.exists

  let%expect_test "exists" =
    let test trie =
      let success = exists trie ~f:(fun data -> String.is_prefix data ~prefix:"h") in
      print_s [%sexp { success : bool; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((success false) (trie ()))
      ((success false) (trie ((() greetings))))
      ((success true) (trie (((1) yo) ((2 3) hello))))
      ((success true)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let existsi = Trie.existsi

  let%expect_test "existsi" =
    let test trie =
      let success =
        existsi trie ~f:(fun ~keychain ~data ->
          List.length keychain % 2 = 0 && String.is_prefix data ~prefix:"h")
      in
      print_s [%sexp { success : bool; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((success false) (trie ()))
      ((success false) (trie ((() greetings))))
      ((success true) (trie (((1) yo) ((2 3) hello))))
      ((success false)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let count = Trie.count

  let%expect_test "count" =
    let test trie =
      let count = count trie ~f:(fun data -> String.is_prefix data ~prefix:"h") in
      print_s [%sexp { count : int; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((count 0) (trie ()))
      ((count 0) (trie ((() greetings))))
      ((count 1) (trie (((1) yo) ((2 3) hello))))
      ((count 3)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let counti = Trie.counti

  let%expect_test "counti" =
    let test trie =
      let count =
        counti trie ~f:(fun ~keychain ~data ->
          List.length keychain % 2 = 0 && String.is_prefix data ~prefix:"h")
      in
      print_s [%sexp { count : int; trie : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((count 0) (trie ()))
      ((count 0) (trie ((() greetings))))
      ((count 1) (trie (((1) yo) ((2 3) hello))))
      ((count 0)
       (trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let partition_tf = Trie.partition_tf

  let%expect_test "partition_tf" =
    let test trie =
      let success, failure =
        partition_tf trie ~f:(fun data -> String.is_prefix data ~prefix:"h")
      in
      print_s [%sexp { trie : data T.t; success : data T.t; failure : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie    ())
       (success ())
       (failure ()))
      ((trie ((() greetings))) (success ()) (failure ((() greetings))))
      ((trie (((1) yo) ((2 3) hello)))
       (success (((2 3) hello)))
       (failure (((1) yo))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (success (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (failure ()))
      |}]
  ;;

  let partitioni_tf = Trie.partitioni_tf

  let%expect_test "partitioni_tf" =
    let test trie =
      let success, failure =
        partitioni_tf trie ~f:(fun ~keychain ~data ->
          List.length keychain % 2 = 0 && String.is_prefix data ~prefix:"h")
      in
      print_s [%sexp { trie : data T.t; success : data T.t; failure : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie    ())
       (success ())
       (failure ()))
      ((trie ((() greetings))) (success ()) (failure ((() greetings))))
      ((trie (((1) yo) ((2 3) hello)))
       (success (((2 3) hello)))
       (failure (((1) yo))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (success ())
       (failure (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let partition_map = Trie.partition_map

  let%expect_test "partition_map" =
    let test trie =
      let success, failure =
        partition_map trie ~f:(fun data ->
          match String.chop_prefix data ~prefix:"h" with
          | Some suffix -> First suffix
          | None -> Second data)
      in
      print_s [%sexp { trie : data T.t; success : data T.t; failure : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie    ())
       (success ())
       (failure ()))
      ((trie ((() greetings))) (success ()) (failure ((() greetings))))
      ((trie (((1) yo) ((2 3) hello)))
       (success (((2 3) ello)))
       (failure (((1) yo))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (success (
         ((1) i)
         ((2) ello)
         ((2 3 4) "ello, world")))
       (failure ()))
      |}]
  ;;

  let partition_mapi = Trie.partition_mapi

  let%expect_test "partition_mapi" =
    let test trie =
      let success, failure =
        partition_mapi trie ~f:(fun ~keychain ~data ->
          match String.chop_prefix data ~prefix:"h" with
          | Some suffix when List.length keychain % 2 = 0 -> First suffix
          | Some _ | None -> Second data)
      in
      print_s [%sexp { trie : data T.t; success : data T.t; failure : data T.t }]
    in
    List.iter example_tries ~f:test;
    [%expect
      {|
      ((trie    ())
       (success ())
       (failure ()))
      ((trie ((() greetings))) (success ()) (failure ((() greetings))))
      ((trie (((1) yo) ((2 3) hello)))
       (success (((2 3) ello)))
       (failure (((1) yo))))
      ((trie (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (success ())
       (failure (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      |}]
  ;;

  let merge = Trie.merge

  let%expect_test "merge" =
    let test_pair (left, right) =
      let merged =
        merge left right ~f:(fun ~keychain variant -> Some (keychain, variant))
      in
      print_s
        [%sexp
          { left : data T.t
          ; right : data T.t
          ; merged : ( keychain
                       , keychain
                         * [ `Left of data | `Right of data | `Both of data * data ]
                       , _ )
                       t
          }]
    in
    let test tries = List.iter (List.cartesian_product tries tries) ~f:test_pair in
    test example_tries;
    [%expect
      {|
      ((left   ())
       (right  ())
       (merged ()))
      ((left ()) (right ((() greetings))) (merged ((() (() (Right greetings))))))
      ((left ())
       (right (((1) yo) ((2 3) hello)))
       (merged (
         ((1) ((1) (Right yo)))
         ((2 3)
          ((2     3)
           (Right hello))))))
      ((left ())
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) ((1) (Right hi)))
         ((2) ((2) (Right hello)))
         ((2 3 4) ((2 3 4) (Right "hello, world"))))))
      ((left ((() greetings))) (right ()) (merged ((() (() (Left greetings))))))
      ((left  ((() greetings)))
       (right ((() greetings)))
       (merged ((() (() (Both (greetings greetings)))))))
      ((left ((() greetings)))
       (right (((1) yo) ((2 3) hello)))
       (merged (
         (() (() (Left greetings)))
         ((1) ((1) (Right yo)))
         ((2 3)
          ((2     3)
           (Right hello))))))
      ((left ((() greetings)))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         (() (() (Left greetings)))
         ((1) ((1) (Right hi)))
         ((2) ((2) (Right hello)))
         ((2 3 4) ((2 3 4) (Right "hello, world"))))))
      ((left (((1) yo) ((2 3) hello)))
       (right ())
       (merged (
         ((1) ((1) (Left yo)))
         ((2 3)
          ((2    3)
           (Left hello))))))
      ((left (((1) yo) ((2 3) hello)))
       (right ((() greetings)))
       (merged (
         (() (() (Right greetings)))
         ((1) ((1) (Left yo)))
         ((2 3)
          ((2    3)
           (Left hello))))))
      ((left (((1) yo) ((2 3) hello)))
       (right (((1) yo) ((2 3) hello)))
       (merged (((1) ((1) (Both (yo yo)))) ((2 3) ((2 3) (Both (hello hello)))))))
      ((left (((1) yo) ((2 3) hello)))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) ((1) (Both (yo hi))))
         ((2) ((2) (Right hello)))
         ((2 3)
          ((2    3)
           (Left hello)))
         ((2 3 4) ((2 3 4) (Right "hello, world"))))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right ())
       (merged (
         ((1) ((1) (Left hi)))
         ((2) ((2) (Left hello)))
         ((2 3 4) ((2 3 4) (Left "hello, world"))))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right ((() greetings)))
       (merged (
         (() (() (Right greetings)))
         ((1) ((1) (Left hi)))
         ((2) ((2) (Left hello)))
         ((2 3 4) ((2 3 4) (Left "hello, world"))))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right (((1) yo) ((2 3) hello)))
       (merged (
         ((1) ((1) (Both (hi yo))))
         ((2) ((2) (Left hello)))
         ((2 3)
          ((2     3)
           (Right hello)))
         ((2 3 4) ((2 3 4) (Left "hello, world"))))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) ((1) (Both (hi hi))))
         ((2) ((2) (Both (hello hello))))
         ((2 3 4) ((2 3 4) (Both ("hello, world" "hello, world")))))))
      |}]
  ;;

  let merge_skewed = Trie.merge_skewed

  let%expect_test "merge_skewed" =
    let test_pair (left, right) =
      let merged =
        merge_skewed left right ~combine:(fun ~keychain data1 data2 ->
          let sep = if List.length keychain % 2 = 0 then ", " else "; " in
          data1 ^ sep ^ data2)
      in
      print_s [%sexp { left : data T.t; right : data T.t; merged : data T.t }]
    in
    let test tries = List.iter (List.cartesian_product tries tries) ~f:test_pair in
    test example_tries;
    [%expect
      {|
      ((left   ())
       (right  ())
       (merged ()))
      ((left ())
       (right  ((() greetings)))
       (merged ((() greetings))))
      ((left ()) (right (((1) yo) ((2 3) hello))) (merged (((1) yo) ((2 3) hello))))
      ((left ())
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((left ((() greetings))) (right ()) (merged ((() greetings))))
      ((left   ((() greetings)))
       (right  ((() greetings)))
       (merged ((() "greetings, greetings"))))
      ((left ((() greetings)))
       (right (((1) yo) ((2 3) hello)))
       (merged ((() greetings) ((1) yo) ((2 3) hello))))
      ((left ((() greetings)))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         (() greetings)
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((left (((1) yo) ((2 3) hello))) (right ()) (merged (((1) yo) ((2 3) hello))))
      ((left (((1) yo) ((2 3) hello)))
       (right ((() greetings)))
       (merged ((() greetings) ((1) yo) ((2 3) hello))))
      ((left (((1) yo) ((2 3) hello)))
       (right (((1) yo) ((2 3) hello)))
       (merged (((1) "yo; yo") ((2 3) "hello, hello"))))
      ((left (((1) yo) ((2 3) hello)))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) "yo; hi")
         ((2) hello)
         ((2 3) hello)
         ((2 3 4) "hello, world"))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right ())
       (merged (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right ((() greetings)))
       (merged (
         (() greetings)
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world"))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right (((1) yo) ((2 3) hello)))
       (merged (
         ((1) "hi; yo")
         ((2) hello)
         ((2 3) hello)
         ((2 3 4) "hello, world"))))
      ((left (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (right (
         ((1) hi)
         ((2) hello)
         ((2 3 4) "hello, world")))
       (merged (
         ((1) "hi; hi")
         ((2) "hello; hello")
         ((2 3 4) "hello, world; hello, world"))))
      |}]
  ;;
end
