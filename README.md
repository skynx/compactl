# COMPACTL
### _Sky Hester_

Lisp-native compact data structures.

This is a lisp system which WILL SOON implement (non-naive) WT and,
more importantly, the Fonseca-Silva algorithm for single-pass
construction of WT with an unknown alphabet.

## Note about :compactl-debug

Some functions include explanation forms which print information to
the `*standard-output*` stream, but these are not enabled by default.
To be provided with these explanation statements when those functions
are called (e.g., to better understand how they work), manually place
`:compactl-debug` on the `*features*` list,

```
(push :compactl-debug *features*)
```

then reload the system.  To get quicklisp to do this, update the
access time for the file you care about, `wavelet-trees.lisp` for
example:

```
touch compact/sequences/wavelet-tree.lisp
```

then, load the system as usual.

```
(ql:quickload :compactl)
```

To disable debug printing, issue the following form at the REPL,
`touch` the requisite files, and reload again.

```
(setq *features* (remove :compactl-debug *features*))
```

## wavelet-tree

Wavelet trees (WT), as described by Gonzalo Navarro, are a compact
data structure over sequences having length n, of symbols from an
alphabet Σ with σ symbols, where σ is a positive integer, supporting
three standard query operations in O(log σ) time and space \(n log σ +
o(n log σ) + O(σw) \) bits, where w is the length in bits of the
machine word.

## License

No license yet.

