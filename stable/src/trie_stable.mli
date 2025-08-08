open! Core.Core_stable

(** Takes an unstable keychain implementation to define the in-memory representation, and
    a stable serialization for keychain values to define the serialization. *)
module V1
    (Keychain_unstable : Trie.Keychainable.S)
    (Keychain_stable : Stable_with_witness with type t = Keychain_unstable.t) : sig
  type 'a t = 'a Trie.Make(Keychain_unstable).t [@@deriving compare ~localize]

  include Stable1_with_witness with type 'a t := 'a t
end
