# COMPACTL
### _Sky Hester_

Lisp-native compact data structures.

(A WORK IN PROGRESS.)

This is a lisp system which is intended to provide a variety of data
structures with theoretically good space and time performance
guarantees.

# Design premise

`COMPACTL` intends to be a fairly faithful rendering into Common Lisp
of the design sketch of a compact data structures library described in
Compact Data Structures: A Practical Approach (G. Navarro, 2016).

Following Navarro's presentation, the primary system definition
`:compactl` in the file `compactl.asd` has been organized into modules
corresponding to related data structures and their various compact
implementations: Arrays, bitvectors, (parentheses, permutations,)
sequences(, trees, graphs, grids, and texts).  Dependencies among the
modules have been initially informed by dependencies among chapters in
the book, although others may be added if needed.  Additionally, I'm
including the compression capabilites needed as they come up, and the
generic protocol for inspecting and manipulating compact data
structures is defined and documented in accord with the book.  Tests
and examples are contained in separate systems, `:compactl.t` and
`:compactl.ex`, respectively.

One fortunate (and unexpected) consequence of approaching the project
this way has to do with that book's habit of explaining how a new data
structure works by showing how to obtain it through modifying an
instance of a data structure defined earlier.  In CLOS, this
pedagogical device is very naturally modeled using
`update-instance-for-different-class`.  Speaking from an admittedly
subjective position, these transforms between fundamentally similar
classes seem to give the system a sense of cohesion for me -- even
moreso than the class structure or generic functions and their
methods/combinations.  Maybe in some sense, though, such
transformations are only easy to present clearly because of a well
conceived object-oriented design.  I won't try to take credit for
that.

# Supported data structures

## wavelet-tree

Wavelet trees (WT) are a compact data structure over sequences having
length n, of symbols from an alphabet Σ with σ symbols, where σ is a
positive integer, supporting three standard query operations in O(log
σ) time and space \(n log σ + o(n log σ) + O(σw) \) bits, where w is
the length in bits of the machine word.

The system WILL SOON implement (non-naive) WT and, more importantly,
the Fonseca-Silva algorithm for single-pass construction of WT with an
unknown alphabet.

# Tips

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

## License

No license yet.

