open! Core.Core_stable

module V1
    (Keychain_unstable : Trie.Keychainable.S)
    (Keychain_stable : Stable_with_witness with type t = Keychain_unstable.t) =
struct
  module T = Trie.Make (Keychain_unstable)

  type 'a t = 'a T.t

  let%template compare = (Trie.compare [@mode m]) [@@mode m = (global, local)]
  let map = Trie.map

  module Alist_stable = struct
    type 'a t = (Keychain_stable.t * 'a) List.V1.t
    [@@deriving bin_io, sexp, stable_witness]
  end

  let to_alist = Trie.to_alist
  let of_alist alist = Trie.of_alist_exn T.Keychain.keychainable alist

  include
    Sexpable.Of_sexpable1.V1
      (Alist_stable)
      (struct
        type 'a t = 'a T.t

        let to_sexpable = to_alist
        let of_sexpable = of_alist
      end)

  include
    Binable.Of_binable1.V2
      (Alist_stable)
      (struct
        type 'a t = 'a T.t

        let to_binable = to_alist
        let of_binable = of_alist

        let caller_identity =
          Bin_shape.Uuid.of_string "ce72f8fc-7f27-4a96-be76-5ce83184d82f"
        ;;
      end)

  let stable_witness data_witness =
    Stable_witness.of_serializable
      (Alist_stable.stable_witness data_witness)
      of_alist
      to_alist
  ;;
end
