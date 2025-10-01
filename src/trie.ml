open! Base
module Keychainable = Keychainable
module Iterator = Iterator
include Trie_intf

module Or_duplicate = struct
  type ('a, 'b) t =
    | Ok of 'a
    | Duplicate of 'b
  [@@deriving sexp_of]

  let map t ~f =
    match t with
    | Ok x -> Ok (f x)
    | Duplicate _ as t -> t
  ;;

  let of_result = function
    | Error x -> Duplicate x
    | Ok x -> Ok x
  ;;
end

let always_ok = function
  | Ok x -> x
  | Error (_ : Nothing.t) -> .
;;

let or_keychain_error ~keychainable result ~message =
  Result.map_error result ~f:(fun keychain ->
    let sexp_of_keychain = Keychainable.sexp_of_keychain keychainable in
    Error.create_s [%sexp (message : string), { keychain : keychain }])
;;

module Node0 : sig
  type ('chain, +'data, 'desc) t constraint 'desc = _ * _ * _ * _ * _

  val empty : (_, _, _) t
  val is_empty : _ t -> bool

  val%template datum : (_, 'data, _) t @ m -> 'data option @ m
  [@@mode m = (global, local)]

  val num_children : _ t -> int

  val%template tries
    :  ('chain, 'data, (_ * 'key * 'cmp * _ * _ as 'desc)) t @ m
    -> ('key, ('chain, 'data, 'desc) t, 'cmp) Map.Using_comparator.Tree.t @ m
  [@@mode m = (global, local)]

  val create
    :  datum:'data option
    -> tries:('key, ('chain, 'data, 'desc) t, 'cmp) Map.Using_comparator.Tree.t
    -> ('chain, 'data, (_ * 'key * 'cmp * _ * _ as 'desc)) t

  (** Defines the data structure invariant prior to serialization definitions. *)
  val invariant
    :  keychainable:('chain, (_ * 'key * 'cmp * _ * _ as 'desc)) Keychainable.t
    -> 'chain Invariant.t
    -> 'data Invariant.t
    -> ('chain, 'data, 'desc) t
    -> sexp_of_t:(('chain, 'data, 'desc) t -> Sexp.t)
    -> unit
end = struct
  type ('chain, 'data, 'desc) t =
    { datum : 'data option
    ; tries :
        ( 'key
          , ('chain, 'data, 'wit * 'key * 'cmp * 'iter * 'idx) t
          , 'cmp )
          Map.Using_comparator.Tree.t
    }
    constraint 'desc = 'wit * 'key * 'cmp * 'iter * 'idx
  [@@deriving fields ~getters ~local_getters]

  let is_empty { datum; tries } =
    Option.is_none datum && Map.Using_comparator.Tree.is_empty tries
  ;;

  let empty =
    { datum = None; tries = Map.Using_comparator.Tree.empty_without_value_restriction }
  ;;

  (* [create] maintains the invariant that nodes have no empty children. *)
  let create ~datum ~tries =
    let tries =
      Map.Using_comparator.Tree.filter tries ~f:(fun trie -> not (is_empty trie))
    in
    { datum; tries }
  ;;

  let num_children t = Map.Using_comparator.Tree.length t.tries

  let invariant_context ~keychainable ~rev_keys =
    let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
    let sexp_of_keychain = Keychainable.sexp_of_keychain keychainable in
    [%sexp "problem with trie", { keychain : keychain }]
  ;;

  let rec invariant_at ~keychainable chain_invariant data_invariant t ~rev_keys =
    (* We check each layer in a context that adds the current keychain to the error
       message. We exit that context before checking children so we don't wind up with
       many layers of nesting. *)
    Invariant.invariant
      rev_keys
      (fun rev_keys -> invariant_context ~keychainable ~rev_keys)
      (fun () ->
        Option.iter (datum t) ~f:(fun data ->
          (* We only construct a keychain and check its invariant at nodes that have
              data. There might be a keychain invariant such as no empty chains being
              stored in the trie. *)
          let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
          chain_invariant keychain;
          data_invariant data);
        if is_empty t && not (List.is_empty rev_keys)
        then raise_s [%sexp "trie contains empty node"]);
    Map.Using_comparator.Tree.iteri (tries t) ~f:(fun ~key ~data:trie ->
      invariant_at
        ~keychainable
        chain_invariant
        data_invariant
        trie
        ~rev_keys:(key :: rev_keys))
  ;;

  let invariant ~keychainable chain_invariant data_invariant t ~sexp_of_t =
    Invariant.invariant t sexp_of_t (fun () ->
      invariant_at ~keychainable chain_invariant data_invariant t ~rev_keys:[])
  ;;
end

module Node = struct
  include Node0

  [%%template
  [@@@mode.default m = (global, local)]

  let rec equal ~keychainable equal_data trie1 trie2 =
    (Option.equal [@mode m])
      equal_data
      ((datum [@mode m]) trie1)
      ((datum [@mode m]) trie2)
    && ((Map.Using_comparator.Tree.equal [@mode m])
          ~comparator:(Keychainable.comparator keychainable)
          ((equal [@mode m]) ~keychainable equal_data)
          ((tries [@mode m]) trie1)
          ((tries [@mode m]) trie2) [@nontail])
  ;;

  let compare ~keychainable compare_data =
    let rec compare_trie x y = force lazy_compare_trie x y
    and lazy_compare_trie =
      (* use laziness to avoid calling the [Comparable.*] functions more than once *)
      lazy
        (fun a_1 b_1 ->
          (Comparable.lexicographic [@mode m])
            [ (fun a b ->
                (Comparable.lift [@mode m])
                  ~f:(datum [@mode m])
                  ((Option.compare [@mode m]) compare_data)
                  a
                  b)
            ; (fun a b ->
                (Comparable.lift [@mode m])
                  ~f:(tries [@mode m])
                  ((Map.Using_comparator.Tree.compare_direct [@mode m])
                     ~comparator:(Keychainable.comparator keychainable)
                     compare_trie)
                  a
                  b)
            ]
            a_1
            b_1)
    in
    compare_trie
  ;;]

  let rec find_trie_and_call_at ~keychainable t keychain ~iter ~if_found ~if_not_found =
    if Keychainable.is_finished keychainable iter keychain
    then if_found t
    else (
      let key = Keychainable.get_exn keychainable iter keychain in
      let iter = Keychainable.next_exn keychainable iter keychain in
      let tries = tries t in
      match
        Map.Using_comparator.Tree.find
          ~comparator:(Keychainable.comparator keychainable)
          tries
          key
      with
      | None -> if_not_found
      | Some trie ->
        find_trie_and_call_at ~keychainable trie keychain ~iter ~if_found ~if_not_found)
  ;;

  let find_trie_and_call ~keychainable t keychain ~if_found ~if_not_found =
    let iter = Keychainable.start keychainable keychain in
    find_trie_and_call_at ~keychainable t keychain ~iter ~if_found ~if_not_found
  ;;

  let find_trie ~keychainable t keychain =
    if is_empty t
    then t
    else find_trie_and_call ~keychainable t keychain ~if_found:Fn.id ~if_not_found:empty
  ;;

  let mem_trie ~keychainable t keychain =
    find_trie_and_call
      ~keychainable
      t
      keychain
      ~if_found:(fun trie -> not (is_empty trie))
      ~if_not_found:false
  ;;

  let find ~keychainable t keychain =
    find_trie_and_call ~keychainable t keychain ~if_found:datum ~if_not_found:None
  ;;

  let mem ~keychainable t keychain = Option.is_some (find ~keychainable t keychain)

  let find_multi ~keychainable t keychain =
    find ~keychainable t keychain |> Option.value ~default:[]
  ;;

  let find_result ~keychainable t keychain =
    match find ~keychainable t keychain with
    | Some data -> Ok data
    | None -> Error keychain
  ;;

  let find_or_error ~keychainable t keychain =
    find_result ~keychainable t keychain
    |> or_keychain_error ~keychainable ~message:"Trie.find_exn: keychain not found"
  ;;

  let find_exn ~keychainable t keychain =
    find_or_error ~keychainable t keychain |> Or_error.ok_exn
  ;;

  let rec update_trie_result_at ~keychainable t keychain ~iter ~f ~stack =
    if Keychainable.is_finished keychainable iter keychain
    then (
      let%map.Result trie = f t in
      List.fold stack ~init:trie ~f:(fun acc (key, context) ->
        create
          ~datum:(datum context)
          ~tries:
            (Map.Using_comparator.Tree.set
               ~comparator:(Keychainable.comparator keychainable)
               (tries context)
               ~key
               ~data:acc)))
    else (
      let key = Keychainable.get_exn keychainable iter keychain in
      let iter = Keychainable.next_exn keychainable iter keychain in
      let trie =
        match
          Map.Using_comparator.Tree.find
            ~comparator:(Keychainable.comparator keychainable)
            (tries t)
            key
        with
        | Some trie -> trie
        | None -> empty
      in
      update_trie_result_at ~keychainable trie keychain ~iter ~f ~stack:((key, t) :: stack))
  ;;

  let update_trie_result ~keychainable t keychain ~f =
    let iter = Keychainable.start keychainable keychain in
    update_trie_result_at ~keychainable t keychain ~iter ~f ~stack:[]
  ;;

  let update_trie ~keychainable t keychain ~f =
    update_trie_result ~keychainable t keychain ~f:(fun x -> Ok (f x)) |> always_ok
  ;;

  let set_trie ~keychainable t ~keychain ~trie =
    update_trie ~keychainable t keychain ~f:(fun _ -> trie)
  ;;

  let add_trie_result ~keychainable t ~keychain ~trie =
    update_trie_result ~keychainable t keychain ~f:(fun inner ->
      if is_empty inner then Ok trie else Error keychain)
  ;;

  let add_trie ~keychainable t ~keychain ~trie =
    add_trie_result ~keychainable t ~keychain ~trie |> Or_duplicate.of_result
  ;;

  let add_trie_or_error ~keychainable t ~keychain ~trie =
    add_trie_result ~keychainable t ~keychain ~trie
    |> or_keychain_error ~keychainable ~message:"Trie.add_trie_exn: duplicate keychain"
  ;;

  let add_trie_exn ~keychainable t ~keychain ~trie =
    add_trie_or_error ~keychainable t ~keychain ~trie |> Or_error.ok_exn
  ;;

  let set ~keychainable t ~keychain ~data =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      create ~datum:(Some data) ~tries:(tries trie))
  ;;

  let remove ~keychainable t keychain =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      create ~datum:None ~tries:(tries trie))
  ;;

  let change ~keychainable t keychain ~f =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      create ~datum:(f (datum trie)) ~tries:(tries trie))
  ;;

  let update ~keychainable t keychain ~f =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      create ~datum:(Some (f (datum trie))) ~tries:(tries trie))
  ;;

  let add_result ~keychainable t ~keychain ~data =
    update_trie_result ~keychainable t keychain ~f:(fun trie ->
      match datum trie with
      | Some _ -> Error keychain
      | None -> Ok (create ~datum:(Some data) ~tries:(tries trie)))
  ;;

  let add ~keychainable t ~keychain ~data =
    add_result ~keychainable t ~keychain ~data |> Or_duplicate.of_result
  ;;

  let add_or_error ~keychainable t ~keychain ~data =
    add_result ~keychainable t ~keychain ~data
    |> or_keychain_error ~keychainable ~message:"Trie.add_exn: duplicate keychain"
  ;;

  let add_exn ~keychainable t ~keychain ~data =
    add_or_error ~keychainable t ~keychain ~data |> Or_error.ok_exn
  ;;

  let add_multi ~keychainable t ~keychain ~data =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      create
        ~datum:(Some (data :: Option.value (datum trie) ~default:[]))
        ~tries:(tries trie))
  ;;

  let remove_multi ~keychainable t keychain =
    update_trie ~keychainable t keychain ~f:(fun trie ->
      let datum =
        match datum trie with
        | Some (_ :: (_ :: _ as data)) -> Some data
        | Some [ _ ] | Some [] | None -> None
      in
      create ~datum ~tries:(tries trie))
  ;;

  let of_alist_result ~keychainable alist =
    List.fold_result alist ~init:empty ~f:(fun t (keychain, data) ->
      add_result ~keychainable t ~keychain ~data)
  ;;

  let of_alist ~keychainable alist =
    of_alist_result ~keychainable alist |> Or_duplicate.of_result
  ;;

  let of_alist_or_error ~keychainable alist =
    of_alist_result ~keychainable alist
    |> or_keychain_error ~keychainable ~message:"Trie.of_alist_exn: duplicate keychain"
  ;;

  let of_alist_exn ~keychainable alist =
    of_alist_or_error ~keychainable alist |> Or_error.ok_exn
  ;;

  let of_alist_multi_rev ~keychainable alist =
    List.fold alist ~init:empty ~f:(fun t (keychain, data) ->
      add_multi ~keychainable t ~keychain ~data)
  ;;

  let of_alist_multi ~keychainable alist =
    of_alist_multi_rev ~keychainable (List.rev alist)
  ;;

  let of_sequence_result ~keychainable seq =
    Sequence.fold_result seq ~init:empty ~f:(fun t (keychain, data) ->
      add_result ~keychainable t ~keychain ~data)
  ;;

  let of_sequence ~keychainable seq =
    of_sequence_result ~keychainable seq |> Or_duplicate.of_result
  ;;

  let of_sequence_or_error ~keychainable seq =
    of_sequence_result ~keychainable seq
    |> or_keychain_error ~keychainable ~message:"Trie.of_sequence_exn: duplicate keychain"
  ;;

  let of_sequence_exn ~keychainable seq =
    of_sequence_or_error ~keychainable seq |> Or_error.ok_exn
  ;;

  let of_sequence_multi ~keychainable seq =
    of_alist_multi_rev ~keychainable (Sequence.to_list_rev seq)
  ;;

  let rec filter_mapi_at ~keychainable t ~f ~rev_keys =
    let datum =
      let%bind.Option data = datum t in
      f ~rev_keys ~data
    in
    let tries =
      Map.Using_comparator.Tree.mapi (tries t) ~f:(fun ~key ~data:trie ->
        filter_mapi_at ~keychainable trie ~f ~rev_keys:(key :: rev_keys))
    in
    create ~datum ~tries
  ;;

  let filter_mapi ~keychainable t ~f =
    let f ~rev_keys ~data =
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      f ~keychain ~data
    in
    filter_mapi_at ~keychainable t ~f ~rev_keys:[]
  ;;

  let filteri ~keychainable t ~f =
    filter_mapi ~keychainable t ~f:(fun ~keychain ~data ->
      if f ~keychain ~data then Some data else None)
  ;;

  let filter_keychains ~keychainable t ~f =
    filteri ~keychainable t ~f:(fun ~keychain ~data:_ -> f keychain)
  ;;

  let mapi ~keychainable t ~f =
    filter_mapi ~keychainable t ~f:(fun ~keychain ~data -> Some (f ~keychain ~data))
  ;;

  (* We don't use [filter_mapi] here; we don't want to allocate key lists. *)
  let rec filter_map t ~f =
    let datum = Option.bind (datum t) ~f in
    let tries =
      Map.Using_comparator.Tree.map (tries t) ~f:(fun trie -> filter_map trie ~f)
    in
    create ~datum ~tries
  ;;

  let filter t ~f = filter_map t ~f:(fun data -> if f data then Some data else None)
  let map t ~f = filter_map t ~f:(fun data -> Some (f data))

  let rec foldi_nodes_at ~keychainable t ~init ~f ~rev_keys =
    Map.Using_comparator.Tree.fold
      (tries t)
      ~init:(f init ~rev_keys ~node:t)
      ~f:(fun ~key ~data:node acc ->
        foldi_nodes_at ~keychainable node ~init:acc ~f ~rev_keys:(key :: rev_keys))
  ;;

  let foldi_nodes ~keychainable t ~init ~f =
    let f acc ~rev_keys ~node =
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      f acc ~keychain ~node
    in
    foldi_nodes_at ~keychainable t ~init ~f ~rev_keys:[]
  ;;

  let rec foldi_at ~keychainable t ~init ~f ~rev_keys =
    Map.Using_comparator.Tree.fold
      (tries t)
      ~init:(Option.fold (datum t) ~init ~f:(fun acc data -> f acc ~rev_keys ~data))
      ~f:(fun ~key ~data:trie acc ->
        foldi_at ~keychainable trie ~init:acc ~f ~rev_keys:(key :: rev_keys))
  ;;

  let foldi ~keychainable t ~init ~f =
    let f acc ~rev_keys ~data =
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      f acc ~keychain ~data
    in
    foldi_at ~keychainable t ~init ~f ~rev_keys:[]
  ;;

  let partition_mapi ~keychainable t ~f =
    let init = empty, empty in
    foldi ~keychainable t ~init ~f:(fun (first, second) ~keychain ~data ->
      match (f ~keychain ~data : _ Either.t) with
      | First data -> add_exn ~keychainable first ~keychain ~data, second
      | Second data -> first, add_exn ~keychainable second ~keychain ~data)
  ;;

  let partitioni_tf ~keychainable t ~f =
    partition_mapi ~keychainable t ~f:(fun ~keychain ~data ->
      if f ~keychain ~data then First data else Second data)
  ;;

  let to_alist ~keychainable t =
    foldi ~keychainable t ~init:[] ~f:(fun acc ~keychain ~data -> (keychain, data) :: acc)
    |> List.rev
  ;;

  let to_sexp ~keychainable sexp_of_chain sexp_of_data t =
    [%sexp (to_alist ~keychainable t : (chain * data) list)]
  ;;

  (* We don't use [foldi] here; we don't want to allocate key lists. *)
  let rec fold t ~init ~f =
    Map.Using_comparator.Tree.fold
      (tries t)
      ~init:(Option.fold (datum t) ~init ~f:(fun acc data -> f acc data))
      ~f:(fun ~key:_ ~data:trie acc -> fold trie ~init:acc ~f)
  ;;

  (* We don't use [partition_mapi] here; we don't want to allocate key lists. *)
  let partition_map ~keychainable t ~f =
    let init = empty, empty in
    foldi ~keychainable t ~init ~f:(fun (first, second) ~keychain ~data ->
      match (f data : _ Either.t) with
      | First data -> add_exn ~keychainable first ~keychain ~data, second
      | Second data -> first, add_exn ~keychainable second ~keychain ~data)
  ;;

  let partition_tf ~keychainable t ~f =
    partition_map ~keychainable t ~f:(fun data ->
      if f data then First data else Second data)
  ;;

  let length t = fold t ~init:0 ~f:(fun n _ -> n + 1)

  let keychains ~keychainable t =
    foldi ~keychainable t ~init:[] ~f:(fun list ~keychain ~data:_ -> keychain :: list)
    |> List.rev
  ;;

  let data t = fold t ~init:[] ~f:(fun list data -> data :: list) |> List.rev
  let count t ~f = fold t ~init:0 ~f:(fun n data -> if f data then n + 1 else n)

  let counti ~keychainable t ~f =
    foldi ~keychainable t ~init:0 ~f:(fun n ~keychain ~data ->
      if f ~keychain ~data then n + 1 else n)
  ;;

  let iter t ~f = fold t ~init:() ~f:(fun () data -> f data)

  let iter_keychains ~keychainable t ~f =
    foldi ~keychainable t ~init:() ~f:(fun () ~keychain ~data:_ -> f keychain)
  ;;

  let iteri ~keychainable t ~f =
    foldi ~keychainable t ~init:() ~f:(fun () ~keychain ~data -> f ~keychain ~data)
  ;;

  let exists t ~f =
    With_return.with_return (fun escape ->
      iter t ~f:(fun data -> if f data then escape.return true);
      false)
  ;;

  let for_all t ~f = not (exists t ~f:(fun data -> not (f data)))

  let existsi ~keychainable t ~f =
    With_return.with_return (fun escape ->
      iteri ~keychainable t ~f:(fun ~keychain ~data ->
        if f ~keychain ~data then escape.return true);
      false)
  ;;

  let for_alli ~keychainable t ~f =
    not (existsi ~keychainable t ~f:(fun ~keychain ~data -> not (f ~keychain ~data)))
  ;;

  let rec merge_skewed_at ~keychainable trie1 trie2 ~combine ~rev_keys =
    let datum =
      Option.merge (datum trie1) (datum trie2) ~f:(fun data1 data2 ->
        combine ~rev_keys data1 data2)
    in
    let tries =
      (* There is no [merge_skewed] for [Map.Using_comparator.Tree]. They do not store
         length, so it cannot be implemented efficiently. Our "skew" comes from not
         needing to recur on asymmetric sub-tries. *)
      Map.Using_comparator.Tree.merge
        ~comparator:(Keychainable.comparator keychainable)
        (tries trie1)
        (tries trie2)
        ~f:(fun ~key variant ->
          match variant with
          | `Left trie | `Right trie -> Some trie
          | `Both (trie1, trie2) ->
            Some
              (merge_skewed_at
                 ~keychainable
                 trie1
                 trie2
                 ~combine
                 ~rev_keys:(key :: rev_keys)))
    in
    create ~datum ~tries
  ;;

  let merge_skewed ~keychainable trie1 trie2 ~combine =
    let combine ~rev_keys x y =
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      combine ~keychain x y
    in
    merge_skewed_at ~keychainable trie1 trie2 ~combine ~rev_keys:[]
  ;;

  let rec merge_at ~keychainable trie1 trie2 ~f ~rev_keys =
    let datum =
      let%bind.Option variant =
        match datum trie1, datum trie2 with
        | None, None -> None
        | Some data1, None -> Some (`Left data1)
        | None, Some data2 -> Some (`Right data2)
        | Some data1, Some data2 -> Some (`Both (data1, data2))
      in
      f ~rev_keys variant
    in
    let tries =
      Map.Using_comparator.Tree.merge
        ~comparator:(Keychainable.comparator keychainable)
        (tries trie1)
        (tries trie2)
        ~f:(fun ~key variant ->
          let rev_keys = key :: rev_keys in
          let trie =
            (* By re-bindings [rev_keys] at each clause, we cannot forget to re-add them to
               the list of keychain when recurring. *)
            match rev_keys, variant with
            | rev_keys, `Both (trie1, trie2) ->
              merge_at ~keychainable trie1 trie2 ~f ~rev_keys
            | rev_keys, `Left trie1 ->
              filter_mapi_at ~keychainable trie1 ~rev_keys ~f:(fun ~rev_keys ~data ->
                f ~rev_keys (`Left data))
            | rev_keys, `Right trie2 ->
              filter_mapi_at ~keychainable trie2 ~rev_keys ~f:(fun ~rev_keys ~data ->
                f ~rev_keys (`Right data))
          in
          Some trie)
    in
    create ~datum ~tries
  ;;

  let merge ~keychainable trie1 trie2 ~f =
    let f ~rev_keys variant =
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      f ~keychain variant
    in
    merge_at ~keychainable trie1 trie2 ~f ~rev_keys:[]
  ;;

  let rec to_sequence_at ~keychainable t ~rev_keys =
    let tries_sequence =
      let%bind.Sequence key, trie =
        Map.Using_comparator.Tree.to_sequence
          ~comparator:(Keychainable.comparator keychainable)
          (tries t)
      in
      to_sequence_at ~keychainable trie ~rev_keys:(key :: rev_keys)
    in
    match datum t with
    | None -> tries_sequence
    | Some data -> Sequence.shift_right tries_sequence (rev_keys, data)
  ;;

  let to_sequence ~keychainable t =
    to_sequence_at ~keychainable t ~rev_keys:[]
    |> Sequence.map ~f:(fun (rev_keys, data) ->
      let keychain = Keychainable.keychain_of_rev_keys keychainable rev_keys in
      keychain, data)
  ;;
end

type ('chain, 'data, 'desc) t =
  { keychainable : ('chain, 'desc) Keychainable.t @@ global
  ; root : ('chain, 'data, 'desc) Node.t
  }
[@@deriving fields ~getters ~local_getters]

let sexp_of_keychain t = Keychainable.sexp_of_keychain (keychainable t)
let sexp_of_key t = Keychainable.sexp_of_key (keychainable t)

let sexp_of_t sexp_of_chain sexp_of_data _ trie =
  Node.to_sexp ~keychainable:trie.keychainable sexp_of_chain sexp_of_data trie.root
;;

let to_sexp sexp_of_data t = sexp_of_t (sexp_of_keychain t) sexp_of_data [%sexp_of: _] t
let make root ~keychainable = { root; keychainable }

let like t node =
  if phys_equal node (root t) then t else make node ~keychainable:(keychainable t)
;;

let like2 t (x, y) = like t x, like t y

let like_poly t node =
  (* when the type may have changed, we cannot do a [phys_equal] check *)
  make node ~keychainable:(keychainable t)
;;

let like2_poly t (x, y) = like_poly t x, like_poly t y
let empty keychainable = make Node.empty ~keychainable

let of_alist keychainable alist =
  Node.of_alist ~keychainable alist |> Or_duplicate.map ~f:(make ~keychainable)
;;

let of_alist_or_error keychainable alist =
  Node.of_alist_or_error ~keychainable alist |> Or_error.map ~f:(make ~keychainable)
;;

let of_alist_exn keychainable alist =
  Node.of_alist_exn ~keychainable alist |> make ~keychainable
;;

let of_alist_multi keychainable alist =
  Node.of_alist_multi ~keychainable alist |> make ~keychainable
;;

let of_sequence keychainable sequence =
  Node.of_sequence ~keychainable sequence |> Or_duplicate.map ~f:(make ~keychainable)
;;

let of_sequence_or_error keychainable sequence =
  Node.of_sequence_or_error ~keychainable sequence |> Or_error.map ~f:(make ~keychainable)
;;

let of_sequence_exn keychainable sequence =
  Node.of_sequence_exn ~keychainable sequence |> make ~keychainable
;;

let of_sequence_multi keychainable sequence =
  Node.of_sequence_multi ~keychainable sequence |> make ~keychainable
;;

let create keychainable ~datum ~tries =
  Node.create
    ~datum
    ~tries:(tries |> Map.Using_comparator.to_tree |> Map.Using_comparator.Tree.map ~f:root)
  |> make ~keychainable
;;

let datum t = Node.datum (root t)

let tries t =
  let keychainable = keychainable t in
  root t
  |> Node.tries
  |> Map.Using_comparator.Tree.map ~f:(like t)
  |> Map.Using_comparator.of_tree ~comparator:(Keychainable.comparator keychainable)
;;

let find_child t key =
  let comparator = keychainable t |> Keychainable.comparator in
  let tries = root t |> Node.tries in
  Map.Using_comparator.Tree.find tries key ~comparator |> Option.map ~f:(like t)
;;

let is_empty t = Node.is_empty (root t)

[%%template
[@@@mode.default m = (global, local)]

let compare compare_data x y =
  let keychainable = keychainable x in
  (Node.compare [@mode m])
    ~keychainable
    compare_data
    ((root [@mode m]) x)
    ((root [@mode m]) y) [@nontail]
;;

let equal equal_data x y =
  let keychainable = keychainable x in
  (Node.equal [@mode m])
    ~keychainable
    equal_data
    ((root [@mode m]) x)
    ((root [@mode m]) y) [@nontail]
;;]

let mem t keychain = Node.mem ~keychainable:(keychainable t) (root t) keychain
let find t keychain = Node.find ~keychainable:(keychainable t) (root t) keychain
let find_exn t keychain = Node.find_exn ~keychainable:(keychainable t) (root t) keychain

let set t ~keychain ~data =
  Node.set ~keychainable:(keychainable t) (root t) ~keychain ~data |> like t
;;

let add t ~keychain ~data =
  Node.add ~keychainable:(keychainable t) (root t) ~keychain ~data
  |> Or_duplicate.map ~f:(like t)
;;

let add_or_error t ~keychain ~data =
  Node.add_or_error ~keychainable:(keychainable t) (root t) ~keychain ~data
  |> Or_error.map ~f:(like t)
;;

let add_exn t ~keychain ~data =
  Node.add_exn ~keychainable:(keychainable t) (root t) ~keychain ~data |> like t
;;

let remove t keychain =
  Node.remove ~keychainable:(keychainable t) (root t) keychain |> like t
;;

let change t keychain ~f =
  Node.change ~keychainable:(keychainable t) (root t) keychain ~f |> like t
;;

let update t keychain ~f =
  Node.update ~keychainable:(keychainable t) (root t) keychain ~f |> like t
;;

let add_multi t ~keychain ~data =
  Node.add_multi ~keychainable:(keychainable t) (root t) ~keychain ~data |> like t
;;

let remove_multi t keychain =
  Node.remove_multi ~keychainable:(keychainable t) (root t) keychain |> like t
;;

let find_multi t keychain =
  Node.find_multi ~keychainable:(keychainable t) (root t) keychain
;;

let mem_trie t keychain = Node.mem_trie ~keychainable:(keychainable t) (root t) keychain

let find_trie t keychain =
  Node.find_trie ~keychainable:(keychainable t) (root t) keychain |> like t
;;

let set_trie t ~keychain ~trie =
  Node.set_trie ~keychainable:(keychainable t) (root t) ~keychain ~trie:(root trie)
  |> like t
;;

let add_trie t ~keychain ~trie =
  Node.add_trie ~keychainable:(keychainable t) (root t) ~keychain ~trie:(root trie)
  |> Or_duplicate.map ~f:(like t)
;;

let add_trie_or_error t ~keychain ~trie =
  Node.add_trie_or_error
    ~keychainable:(keychainable t)
    (root t)
    ~keychain
    ~trie:(root trie)
  |> Or_error.map ~f:(like t)
;;

let add_trie_exn t ~keychain ~trie =
  Node.add_trie_exn ~keychainable:(keychainable t) (root t) ~keychain ~trie:(root trie)
  |> like t
;;

let update_trie t keychain ~f =
  Node.update_trie ~keychainable:(keychainable t) (root t) keychain ~f:(fun node ->
    root (f (like t node)))
  |> like t
;;

let invariant chain_invariant data_invariant t =
  Node.invariant
    ~keychainable:(keychainable t)
    chain_invariant
    data_invariant
    (root t)
    ~sexp_of_t:(fun node -> to_sexp [%sexp_of: _] (like t node))
;;

let length t = Node.length (root t)
let num_children t = Node.num_children (root t)
let keychains t = Node.keychains ~keychainable:(keychainable t) (root t)
let data t = Node.data (root t)
let to_alist t = Node.to_alist ~keychainable:(keychainable t) (root t)
let to_sequence t = Node.to_sequence ~keychainable:(keychainable t) (root t)
let iter t ~f = Node.iter (root t) ~f
let iter_keychains t ~f = Node.iter_keychains ~keychainable:(keychainable t) (root t) ~f
let iteri t ~f = Node.iteri ~keychainable:(keychainable t) (root t) ~f
let fold t ~init ~f = Node.fold (root t) ~init ~f
let foldi t ~init ~f = Node.foldi ~keychainable:(keychainable t) (root t) ~init ~f

let foldi_tries t ~init ~f =
  Node.foldi_nodes
    ~keychainable:(keychainable t)
    (root t)
    ~init
    ~f:(fun acc ~keychain ~node -> f acc ~keychain ~trie:(like t node))
;;

let map t ~f = Node.map (root t) ~f |> like_poly t
let mapi t ~f = Node.mapi ~keychainable:(keychainable t) (root t) ~f |> like_poly t
let filter t ~f = Node.filter (root t) ~f |> like t

let filter_keychains t ~f =
  Node.filter_keychains ~keychainable:(keychainable t) (root t) ~f |> like t
;;

let filteri t ~f = Node.filteri ~keychainable:(keychainable t) (root t) ~f |> like t
let filter_map t ~f = Node.filter_map (root t) ~f |> like_poly t

let filter_mapi t ~f =
  Node.filter_mapi ~keychainable:(keychainable t) (root t) ~f |> like_poly t
;;

let for_all t ~f = Node.for_all (root t) ~f
let for_alli t ~f = Node.for_alli ~keychainable:(keychainable t) (root t) ~f
let exists t ~f = Node.exists (root t) ~f
let existsi t ~f = Node.existsi ~keychainable:(keychainable t) (root t) ~f
let count t ~f = Node.count (root t) ~f
let counti t ~f = Node.counti ~keychainable:(keychainable t) (root t) ~f

let partition_tf t ~f =
  Node.partition_tf ~keychainable:(keychainable t) (root t) ~f |> like2 t
;;

let partitioni_tf t ~f =
  Node.partitioni_tf ~keychainable:(keychainable t) (root t) ~f |> like2 t
;;

let partition_map t ~f =
  Node.partition_map ~keychainable:(keychainable t) (root t) ~f |> like2_poly t
;;

let partition_mapi t ~f =
  Node.partition_mapi ~keychainable:(keychainable t) (root t) ~f |> like2_poly t
;;

let merge x y ~f =
  Node.merge ~keychainable:(keychainable x) (root x) (root y) ~f |> like_poly x
;;

let merge_skewed x y ~combine =
  Node.merge_skewed ~keychainable:(keychainable x) (root x) (root y) ~combine |> like x
;;

module type S = S with type ('chain, 'data, 'desc) trie := ('chain, 'data, 'desc) t

module Make (Keychain : Keychainable.S) : S with module Keychain = Keychain = struct
  module Keychain = Keychain

  type nonrec 'a t = (Keychain.t, 'a, Keychain.keychain_description) t

  let sexp_of_t = to_sexp
end

module Of_string = Make (Keychainable.Of_string)
module Of_list (Key : Comparator.S) = Make (Keychainable.Of_list (Key))

module Of_listable
    (Key : Comparator.S)
    (Keychain : Keychainable.Listable with type elt = Key.t) =
  Make (Keychainable.Of_listable (Key) (Keychain))
