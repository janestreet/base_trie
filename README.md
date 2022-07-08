Trie data structure library
===========================

This library provides an implementation of a
[trie](https://en.wikipedia.org/wiki/Trie) data structure.

Lookup is based on "chains" of keys; each node of the trie has
children representing each potential next key in the chain.  See
`Keychainable`.

This interface is modeled after `Base.Map` for element lookup by
keychains.  It also provides trie-node lookup by keychains.
