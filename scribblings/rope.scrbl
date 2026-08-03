#lang scribble/manual

@(require scribble/example
          racket/sandbox
          @for-label[racket/base
                     racket/contract
                     racket/generic
                     rope
                     rope/rope
                     rope/define-rope-type
                     rope/string
                     rope/bytes])

@(define rope-eval (make-base-eval))
@(rope-eval '(require rope))

@title{Ropes: An Alternative to Strings}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[rope]

A @deftech{rope} is a tree of small string (or byte, or vector, or other) chunks.
It supports the same operations as a string, but where a string of length
@math{n} takes @math{O(n)} time to split, splice, or concatenate, a balanced rope
does the same work in @math{O(log n)}. This library provides the rope data
structure, a generic protocol for building ropes out of any chunkable
data, and ready-made instances for strings and byte strings.

Based on the paper by @hyperlink["https://doi.org/10.1002/spe.4380251203"]{Boehm, Atkinson, and Plass}.

@table-of-contents[]

@; -------------------------------------------------------------------------------------------------

@section{Introduction}

Racket strings are immutable, fixed-length vectors of characters. That makes them
cheap to read and share, but expensive to edit. Inserting a character in the
middle of a string of length @math{n} means allocating and copying a new string
of length @math{n}, because @racket[substring] and @racket[string-append] both
have to walk the whole thing. A text editor that repeatedly inserts and deletes
characters in a large buffer this way pays @math{O(n)} per keystroke, and
@math{O(n^2)} (or worse) over the course of editing the whole document.

Ropes solve this by representing a long sequence as a balanced binary tree of
short chunks rather than as a flat array. Each leaf holds a chunk small enough
to slice or copy cheaply. Each internal node caches the combined length of its
subtrees, so that length queries are @math{O(1)} and a split or concatenation only
has to touch @math{O(log n)} nodes along a single root-to-leaf path. Hence, splicing
new content into a rope costs @math{O(log n)} to find the split points plus the cost
of building a subtree for the inserted text, rather than @math{O(n)} to rebuild the
whole sequence.

Asymptotic complexity is only half the story---real-world performance matters, too.
On the benchmark suite bundled with this package (see
@secref{Measured_Performance}), a splice or slice into a rope of 32,768
characters completes in microseconds, regardless of where in the rope it happens.
Appending two ropes typically costs about 100 nanoseconds. Because ropes compare whole
chunks at once rather than one element at a time, @racket[equal?] on two content-identical
ropes completes in a few hundred microseconds. Recomputing @racket[equal-hash-code]
on a rope that has already been hashed costs under 60 nanoseconds @emph{independent
 of its size}, because hashes are memoized as a composable polynomial hash.

This library separates the tree structure from what it holds. The @racket[rope]
type itself only knows about leaves and nodes---it has no idea whether a leaf's
payload is a string, a byte string, or something else entirely. A generic
interface, @racket[gen:ropeable], describes the handful of operations a chunk type
needs to support (@italic{slice}, @italic{append}, @italic{count}, and so on), and
the @racket[define-rope-type] macro turns any such description into a complete,
efficient, type-specific rope implementation. The library includes support for string
ropes and byte string ropes out of the box, and @secref{Defining_New_Rope_Types} walks
through building a new rope type from scratch.

@; -------------------------------------------------------------------------------------------------

@section{Quick Start}

The most common way to use this library is through @racketmodname[rope/string],
which treats a rope as a persistent, efficiently editable stand-in for a
@racket[string]. You can build one with @racket[string->rope] and get an ordinary
string back with @racket[rope->string]:

@examples[#:eval rope-eval #:label #f
 (define greeting (string->rope "Hello!"))
 (rope-length greeting)
 (rope->string greeting)]

Editing a rope never touches its argument; every operation returns a new rope that
shares structure with the old one. @racket[string-rope-splice] replaces a run of
characters with a new string, and inserting text is just the special case where
nothing is replaced:

@examples[#:eval rope-eval #:label #f
 (define with-name (string-rope-splice greeting 5 0 " World"))
 (rope->string with-name)
 (code:comment "the original is untouched")
 (rope->string greeting)]

Deleting text is just splicing in the empty string.

@examples[#:eval rope-eval #:label #f
 (define bare (string-rope-splice with-name 5 7 ""))
 (rope->string bare)]

Building up a large document out of pieces is a matter of appending ropes, which
is also @math{O(log n)}:

@examples[#:eval rope-eval #:label #f
 (define doc
   (string-rope-append
    (string->rope "It was the best of times, ")
    (string->rope "it was the worst of times.")))
 (rope->string doc)]

Ropes are also sequences of their elements, so a rope of characters works with
the various @racket[for] forms via @racket[in-string-rope]:

@examples[#:eval rope-eval #:label #f
 (for/list ([ch (in-string-rope doc)]
            #:when (char-upper-case? ch))
   ch)]

Everything above works identically for byte strings with @racketmodname[rope/bytes]
in place of @racketmodname[rope/string].

@; -------------------------------------------------------------------------------------------------

@section{Concepts}

@subsection{Leaves, Nodes, and Balance}

A @racket[rope] is either a @racket[rope-leaf], holding one chunk of raw data
directly, or a @racket[rope-node], holding a left and a right sub-rope. Both
variants cache their element @racket[count] and @racket[width] (see below), which
is what makes @racket[rope-length] an @math{O(1)} operation instead of a tree
walk.

Without careful management, repeated splitting and appending can produce a tree that
degenerates into a linked list, at which point operations that ought to be
@math{O(log n)} would become @math{O(n)} again. Following Boehm, Atkinson, and Plass,
this library keeps every rope balanced by maintaining the invariant that a rope of
depth @math{d} always has at least @math{Fib(d + 2)} leaves, where
@math{Fib} is the Fibonacci sequence. @racket[rope-append1] checks this
invariant after every naive concatenation and, when it is violated, rebuilds the
offending subtree from scratch in @math{O(n)} time.
Because a rebuild is only triggered when the tree has drifted measurably out of
balance, this happens at most @math{O(log n)} times over the course of
@math{O(n)} edits, making @racket[rope-append1] amortized @math{O(log n)}.

@subsection[#:tag "Count_and_Width"]{Count and Width}

Every rope tracks three numbers: its @deftech{count}, the number of elements it
holds, its @deftech{width}, the total extent of those elements along some
other axis, and its @deftech{depth}, the distance from it to the furthest leaf.
For the string and byte-string ropes in this library, every element
occupies exactly one unit of width---a character is one character wide, and a byte is
one byte wide---so @racket[rope-count] and @racket[rope-width] always agree.
@racket[rope-length] is simply an alias for the latter.

The two numbers diverge for a chunk type whose elements have variable extent. For
example, in a rope over a sequence of on-screen glyphs, each glyph is a single element
that may occupy a variable number of display columns. @racket[rope-offset-index] exists
for exactly this situation: given a rope and a @tech{width}, such as a column
number, it locates the @tech{count}-based element index at that position in
@math{O(log n)} time. For the built-in types this is a straightforward binary
search, but the machinery is there for chunk types that need it.

@subsection{Cursors}

A @racket[cursor] is a position within a rope, represented as the raw chunk
currently being read, an offset within that chunk, and a rope of everything after
it. Cursors support @math{O(1)} @racket[cursor-peek] and @racket[cursor-advance]
except when crossing from one leaf into the next, which is itself amortized
@math{O(1)} over the length of a leaf. @racket[cursor-drop] skips
@math{k} elements, and @racket[cursor-take] extracts the next @math{k}
elements as a rope. Both run in @math{O(log n)} time, independent of
@math{k}, by splitting the rope rather than visiting each element.

Cursors are the mechanism behind @racket[rope-foldl] and @racket[rope-foldr],
which fold over one or more ropes in lockstep, and behind sequence constructors
such as @racket[in-string-rope].

@subsection[#:tag "The_gen_ropeable_Interface"]{The @racket[gen:ropeable] Interface}

Everything above is implemented once, generically, in terms of a small interface,
@racket[gen:ropeable], that describes how to work with a raw chunk. A chunk type
implements @racket[gen:ropeable] by supplying:

@itemlist[
 @item{a predicate identifying values of that chunk type (@racket[raw?]);}
 @item{a maximum chunk size for a leaf (@racket[raw-limit]) and a way to construct
   an empty chunk (@racket[raw-empty]);}
 @item{a chunk's @tech{count} and @tech{width} (@racket[raw-count],
   @racket[raw-width]);}
 @item{ways to slice, append, and index into a chunk (@racket[raw-slice],
   @racket[raw-append], @racket[raw-ref]); and}
 @item{constructors for type-specific leaf and node structs
   (@racket[rope-leaf-ctor], @racket[rope-node-ctor]).}]

Every other operation in @secref{Generic_Rope_Operations}---splitting, splicing,
slicing, balancing, cursors, folds---comes for free as a fallback implementation
once those pieces are in place. You will rarely call these generic operations
directly;
@racket[define-rope-type] (@secref{Defining_New_Rope_Types}) uses them to generate
a complete, type-specific API like the one @racketmodname[rope/string] exposes for
strings.

Alongside the type-specific API, @racket[define-rope-type] also binds  the
@racket[gen:ropeable] instance itself, e.g. @racket[string-rope-ropeable] and
@racket[bytes-rope-ropeable]. And, with
@racket[rope-type-out]/@racket[rope-type-out/contract], these bindings can be
exported automatically. This witness value enables code written
directly against @racketmodname[rope/rope] generically across any chunk
type---for example, operations on either @racket[string-rope]s or @racket[bytes-rope]s
can be written without going through the type-specific wrappers at all:

@examples[#:eval rope-eval #:label #f
 (rope-append1 string-rope-ropeable (string->rope "abc") (string->rope "def"))]

@subsection{Content-Based Equality and Hashing}

Every rope produced by @racket[define-rope-type] implements
@racket[gen:equal+hash]---@racket[equal?] compares the @emph{sequence of elements}
two ropes denote, not their tree shape, so a rope built by one sequence of
appends/splices is @racket[equal?] to a differently-shaped rope holding the
same content. @racket[equal-hash-code] and
@racket[equal-secondary-hash-code] agree accordingly.

Internally, this is a composable polynomial (Rabin–Karp style) rolling hash.
Each node caches ⟨hash, baseⁿ⟩ for its subtree in a weak, @racket[eq?]-keyed
table, so combining two already-hashed subtrees into a parent is
@math{O(1)}. Hashing a freshly built rope of @math{n} elements is
@math{O(n)}; hashing it again, or hashing any rope sharing structure with
one already hashed, is @math{O(1)} amortized. @racket[equal?] itself takes
an @racket[eq?] fast path; on mismatch, it falls back to an @math{O(n)}
cursor-based walk that never requires the two ropes' leaf boundaries to align.

@examples[#:eval rope-eval #:label #f
 (define a (string->rope "hello world"))
 (define b (string-rope-append (string->rope "hello ") (string->rope "world")))
 (equal? a b)
 (equal? (equal-hash-code a) (equal-hash-code b))]

@; -------------------------------------------------------------------------------------------------

@section[#:tag "Generic_Rope_Operations"]{Generic Rope Operations}

@defmodule[rope/rope]

This module defines the @racket[rope] tree structure, the @racket[cursor] type,
and the @racket[gen:ropeable] interface that connects the two to a particular
chunk representation. Most programs will use a type-specific module such as
@racketmodname[rope/string] instead of calling these generic operations directly;
this reference is here for readers implementing a new chunk type, and to document
what the type-specific operations reduce to underneath.

@subsection{Ropes}

@defstruct*[rope ()]{

 The common supertype of @racket[rope-leaf] and @racket[rope-node]. A
 @racket[rope] value is always one or the other; there are no other instances.}

@defstruct*[(rope-leaf rope) ([count exact-nonnegative-integer?]
                              [width exact-nonnegative-integer?]
                              [raw any/c])]{

 A rope holding a single chunk of raw data. @racket[count] and @racket[width] are
 the chunk's @tech{count} and @tech{width} (the @tech{depth} of a leaf is always 0);
 @racket[raw] is the chunk itself, whose type depends on the @racket[gen:ropeable]
 implementation that produced it.}

@defstruct*[(rope-node rope) ([count exact-nonnegative-integer?]
                              [width exact-nonnegative-integer?]
                              [depth exact-nonnegative-integer?]
                              [left rope?]
                              [right rope?])]{

 A rope formed by concatenating @racket[left] and @racket[right]. @racket[count]
 and @racket[width] are the sums of the corresponding fields of @racket[left] and
 @racket[right], cached to avoid recomputation on every query. @racket[depth] is
 one more than the deeper of @racket[left] and @racket[right].}

@defproc[(rope-count [rope rope?]) exact-nonnegative-integer?]{

 Returns the total @tech{count} of @racket[rope]---the number of elements it
 holds, summed across every leaf.}

@defproc[(rope-width [rope rope?]) exact-nonnegative-integer?]{

 Returns the total @tech{width} of @racket[rope].}

@defproc[(rope-length [rope rope?]) exact-nonnegative-integer?]{

 An alias for @racket[rope-width], provided for symmetry with
 @racket[string-length] and @racket[bytes-length].}

@defproc[(rope-depth [rope rope?]) exact-nonnegative-integer?]{

 Returns the height of @racket[rope]'s tree: @racket[0] for a leaf, and one more
 than the deeper of its two children for a node. Constant time, since every node
 caches its own depth.}

@defproc[(rope-empty? [rope rope?]) boolean?]{

 Returns @racket[#t] if @racket[rope] holds no elements.

@examples[#:eval rope-eval
 (rope-empty? (string->rope ""))
 (rope-empty? (string->rope "x"))]}

@defproc[(rope-balanced? [rope rope?]) boolean?]{

 Returns @racket[#t] if @racket[rope]'s @racket[count] is at least the Fibonacci
 bound for its @racket[depth], per Boehm, Atkinson, and Plass. Ropes produced by
 this library's own operations are always balanced; this predicate exists mainly
 as an internal invariant check.}

@defproc[(rope-flatten [rope rope?]) list?]{

 Returns the list of raw chunks held in @racket[rope]'s leaves, in left-to-right
 order. Runs in time proportional to the number of leaves.}

@subsection[#:tag "gen_ropeable_reference"]{The @racket[gen:ropeable] Interface}

@defthing[gen:ropeable any/c]{

 A generic interface, in the sense of @racket[define-generics], that a chunk type
 implements in order to obtain a complete rope implementation. Most programs
 interact with this interface only indirectly, through @racket[define-rope-type];
 see @secref{Defining_New_Rope_Types}.}

@defproc[(ropeable? [v any/c]) boolean?]{

 Returns @racket[#t] if @racket[v] is an instance of some type implementing
 @racket[gen:ropeable].}

@defproc[(rope-leaf-ctor [ropeable ropeable?]) procedure?]{

 Returns the constructor @racket[ropeable] uses to build leaf nodes: a procedure
 of three arguments, @racket[count], @racket[width], and @racket[raw], returning a
 @racket[rope-leaf?].}

@defproc[(rope-node-ctor [ropeable ropeable?]) procedure?]{

 Returns the constructor @racket[ropeable] uses to build internal nodes: a
 procedure of four arguments, @racket[count], @racket[width], @racket[left], and
 @racket[right], returning a @racket[rope-node?].}

@subsubsection{Raw Chunk Operations}

These describe the raw, non-rope representation that a @racket[gen:ropeable]
instance is built around.

@defproc[(raw? [ropeable ropeable?] [v any/c]) boolean?]{

 Returns @racket[#t] if @racket[v] is a valid raw chunk for @racket[ropeable]'s
 type.}

@defproc[(raw-limit [ropeable ropeable?]) exact-nonnegative-integer?]{

 Returns the largest @tech{count} a single leaf is allowed to hold for
 @racket[ropeable]'s type. Both @racketmodname[rope/string] and
 @racketmodname[rope/bytes] use @racket[512].}

@defproc[(raw-empty [ropeable ropeable?]) any/c]{

 Constructs an empty raw chunk for @racket[ropeable]'s type, e.g., @racket[""] for
 strings and @racket[#""] for byte strings.}

@defproc[(raw-count [ropeable ropeable?] [raw any/c]) exact-nonnegative-integer?]{

 Returns the @tech{count} of @racket[raw], i.e, how many elements it holds.}

@defproc[(raw-width [ropeable ropeable?] [raw any/c]) exact-nonnegative-integer?]{

 Returns the @tech{width} of @racket[raw]. For strings and byte strings this
 coincides with @racket[raw-count]; see @secref["Count_and_Width"].}

@defproc[(raw-slice [ropeable ropeable?]
                     [raw any/c]
                     [start exact-nonnegative-integer?]
                     [end exact-nonnegative-integer?])
         any/c]{

 Returns the sub-chunk of @racket[raw] from element @racket[start] (inclusive) to
 @racket[end] (exclusive), analogous to @racket[substring].}

@defproc[(raw-append [ropeable ropeable?] [raw any/c] ...) any/c]{

 Concatenates any number of raw chunks into one, analogous to
 @racket[string-append].}

@defproc[(raw-ref [ropeable ropeable?]
                   [raw any/c]
                   [pos exact-nonnegative-integer?])
         any/c]{

 Returns the element of @racket[raw] at position @racket[pos], analogous to
 @racket[string-ref].}

@subsubsection{Rope Construction and Editing}

These are the same operations exposed under type-specific names by
@racketmodname[rope/string] and @racketmodname[rope/bytes]; see those sections for
usage examples. Each takes an explicit @racket[gen:ropeable] instance identifying
which chunk type's rules to follow.

@defproc[(make-rope-leaf [ropeable ropeable?] [raw any/c]) rope-leaf?]{

 Wraps @racket[raw] in a leaf, computing its @racket[count] and @racket[width]
 along the way.}

@defproc[(make-rope-node [ropeable ropeable?] [left rope?] [right rope?]) rope-node?]{

 Joins @racket[left] and @racket[right] into a node, without checking or repairing
 balance. Most callers want @racket[rope-append1] instead.}

@defproc[(make-empty-rope [ropeable ropeable?]) rope?]{

 Returns an empty rope: a leaf wrapping @racket[(raw-empty ropeable)].}

@defproc[(rope-concat [ropeable ropeable?] [left rope?] [right rope?]) rope?]{

 Naively joins @racket[left] and @racket[right] into a node in @math{O(1)} time,
 without checking balance. Repeated use can produce an unbalanced tree; see
 @racket[rope-append1].}

@defproc[(rope-append1 [ropeable ropeable?] [left rope?] [right rope?]) rope?]{

 Joins @racket[left] and @racket[right], rebuilding the result from scratch if
 doing so naively would violate the balance invariant. Amortized
 @math{O(log n)}.}

@defproc[(rope-append [ropeable ropeable?] [rope rope?] ...) rope?]{

 Joins any number of ropes left to right by repeated @racket[rope-append1].
 Amortized @math{O(log n)} per argument.}

@defproc[(rope-split [ropeable ropeable?]
                      [rope rope?]
                      [i exact-nonnegative-integer?])
         (values rope? rope?)]{

 Splits @racket[rope] into the elements before index @racket[i] and the elements
 from @racket[i] onward, returning both as ropes. Amortized @math{O(log n)}: one
 descent to the split point, plus one @racket[rope-append1] per level on the way
 back up.}

@defproc[(rope-offset-index [ropeable ropeable?]
                             [rope rope?]
                             [ofs exact-nonnegative-integer?])
         exact-nonnegative-integer?]{

 Returns the @tech{count}-based index of the element containing @tech{width}
 offset @racket[ofs]. See @secref["Count_and_Width"]. @math{O(log n)}.}

@defproc[(rope-splice [ropeable ropeable?]
                       [rope rope?]
                       [start exact-nonnegative-integer?]
                       [old-len exact-nonnegative-integer?]
                       [new-raw any/c])
         rope?]{

 Replaces the @racket[old-len] elements of @racket[rope] beginning at
 @racket[start] with the contents of @racket[new-raw], returning the result.
 @math{O(log n + m)}, where @racket[m] is the @tech{count} of @racket[new-raw].}

@defproc[(rope-slice [ropeable ropeable?]
                      [rope rope?]
                      [start exact-nonnegative-integer?]
                      [len exact-nonnegative-integer?])
         rope?]{

 Extracts the @racket[len] elements of @racket[rope] beginning at @racket[start].
 Amortized @math{O(log n + k)}, where @math{k} is the number of leaves spanned
 by the extracted range.}

@defproc[(raw->rope [ropeable ropeable?] [raw any/c]) rope?]{

 Builds a balanced rope out of @racket[raw] from the bottom up, splitting it into
 leaves no larger than @racket[(raw-limit ropeable)]. @math{O(n)}.}

@defproc[(rope->raw [ropeable ropeable?] [rope rope?]) any/c]{

 Flattens @racket[rope] back into a single raw chunk, by appending its leaves left
 to right. Time proportional to the number of leaves.}

@defproc[(raw-compare [ropeable ropeable?] [raw1 any/c] [raw2 any/c])
         (or/c '< '= '>)]{

 Returns the lexicographic ordering of @racket[raw1] against @racket[raw2].
 This operation is optional: if a @racket[gen:ropeable] instance omits this method,
 the comparison operations will raise an error when invoked on it.}

@defproc[(rope-compare-with [ropeable ropeable?] [cmp-proc procedure?]
                            [rope1 rope?] [rope2 rope?])
         (or/c '< '= '>)]{

 Like @racket[rope-compare], but uses @racket[cmp-proc] in place of
 @racket[raw-compare] to order corresponding chunks, enabling alternate
 orderings.}

@defproc[(rope-compare [ropeable ropeable?] [rope1 rope?] [rope2 rope?])
         (or/c '< '= '>)]{

 Compares @racket[rope1] and @racket[rope2] lexicographically by element,
 without flattening either. @math{O(log n + d)} amortized, where @math{d}
 is the number of elements scanned before the first difference.}

@defproc[(rope=? [ropeable ropeable?] [rope1 rope?] [rope2 rope?]) boolean?]{

 Returns @racket[#t] iff @racket[rope-compare] reports @racket['=] for
 @racket[rope1] and @racket[rope2]. @math{O(log n + d)} amortized, same as
 @racket[rope-compare].

 Note this is @emph{not} the same check as @racket[equal?] on two ropes: this
 comparator-based check requires the instantiating type to have supplied
 @racket[#:compare] to @racket[define-rope-type], and does not benefit from
 the memoized content-hash caching described in
 @secref{Content-Based_Equality_and_Hashing}. Prefer @racket[equal?] for a
 plain equality test; use @racket[rope=?] when you already need
 @racket[rope-compare]'s ordering (e.g. alongside @racket[rope<?]) and want
 the equal-case answer from the same family of operations.}

@defproc[(rope<?  [ropeable ropeable?] [rope1 rope?] [rope2 rope?]) boolean?]
@defproc[(rope<=? [ropeable ropeable?] [rope1 rope?] [rope2 rope?]) boolean?]
@defproc[(rope>?  [ropeable ropeable?] [rope1 rope?] [rope2 rope?]) boolean?]
@defproc[(rope>=? [ropeable ropeable?] [rope1 rope?] [rope2 rope?]) boolean?]{
 See @racket[rope-compare].}

@subsection[#:tag "Cursors-API"]{Cursors}

@defstruct*[cursor ([raw any/c]
                    [pos exact-nonnegative-integer?]
                    [after rope?])]{

 A position within a rope. @racket[raw] is the chunk currently being read,
 @racket[pos] is an index into it, and @racket[after] is a rope of everything
 following that chunk. Most programs use the type-specific cursor operations
 provided by @racketmodname[rope/string] or @racketmodname[rope/bytes] rather than
 constructing a @racket[cursor] directly.}

@defproc[(cursor-at-end? [ropeable ropeable?] [cursor cursor?]) boolean?]{

 Returns @racket[#t] if @racket[cursor] has no more elements to read. Constant
 time.}

@defproc[(cursor-peek [ropeable ropeable?] [cursor cursor?]) any/c]{

 Returns the element under @racket[cursor], or @racket[#f] if @racket[cursor] is
 at the end. Constant time, except when crossing from one leaf into the next,
 which is amortized constant time over the leaf.}

@defproc[(cursor-advance [ropeable ropeable?] [cursor cursor?]) cursor?]{

 Returns a cursor advanced one element past @racket[cursor]. Same complexity as
 @racket[cursor-peek].}

@defproc[(cursor-drop [ropeable ropeable?]
                      [cursor cursor?]
                      [k exact-nonnegative-integer?])
         cursor?]{

 Returns a cursor advanced @racket[k] elements past @racket[cursor], by splitting
 the remainder of the rope rather than stepping one element at a time.
 @math{O(log n)}, independent of @racket[k].}

@defproc[(cursor-take [ropeable ropeable?]
                      [cursor cursor?]
                      [k exact-nonnegative-integer?])
         rope?]{

 Returns a rope of the @racket[k] elements starting at @racket[cursor], by
 splitting the remainder of the rope rather than stepping one element at a
 time. @math{O(log n)}, independent of @racket[k].}

@defproc[(rope->cursor [ropeable ropeable?] [rope rope?]) cursor?]{

 Returns a cursor positioned at the first element of @racket[rope]. @math{O(log n)}.}

@defproc[(cursor->rope [ropeable ropeable?] [cursor cursor?]) rope?]{

 Reconstitutes @racket[cursor] and everything after it as a single rope. Used
 internally to implement @racket[cursor-drop] without visiting each skipped
 element.}

@subsection{Cursor-Based Rope Operations}

@defproc[(rope-foldl [ropeable ropeable?]
                      [proc procedure?]
                      [init any/c]
                      [rope rope?] ...+)
         any/c]{

 Folds @racket[proc] left to right over the elements of one or more ropes in
 lockstep, the way @racket[foldl] does over lists. Every @racket[rope] argument
 must have the same @racket[count]. @racket[proc] is applied as
 @racket[(proc acc elem ...)], where the @racket[elem]s are the corresponding
 elements of each rope at the current position and @racket[acc] is the
 accumulated result so far. @math{O(n)}.}

@defproc[(rope-foldr [ropeable ropeable?]
                      [proc procedure?]
                      [init any/c]
                      [rope rope?] ...+)
         any/c]{

 Like @racket[rope-foldl], but folds right to left.}

@; -------------------------------------------------------------------------------------------------

@section{String Ropes}

@defmodule[rope/string]

A rope specialized for @racket[string?] chunks, generated by
@racket[define-rope-type] (@secref{Defining_New_Rope_Types}). Every operation
below behaves exactly like its generic counterpart in @racketmodname[rope/rope],
specialized to strings and with no need to supply a @racket[ropeable] instance
explicitly.

@examples[#:eval rope-eval #:label "Example:"
 (define r (string->rope "supercalifragilisticexpialidocious"))
 (rope-length r)
 (rope-depth r)
 (rope->string (string-rope-slice r 5 4))]

@defstruct*[(string-rope-leaf rope-leaf) ([count exact-nonnegative-integer?]
                                          [width exact-nonnegative-integer?]
                                          [raw string?])]{

 A leaf of a string rope, holding one string of at most @racket[(string-raw-limit)]
 characters.}

@defstruct*[(string-rope-node rope-node) ([count exact-nonnegative-integer?]
                                          [width exact-nonnegative-integer?]
                                          [left string-rope?]
                                          [right string-rope?])]{

 An internal node of a string rope.}

@defproc[(string-rope? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @racket[string-rope-leaf?] or
 @racket[string-rope-node?].}

@defthing[string-rope-ropeable ropeable?]{

 The @racket[gen:ropeable] instance for @racket[string-rope]s. Passing this to
 an operation from @racketmodname[rope/rope], such as @racket[rope-append1] or
 @racket[rope->cursor], behaves exactly like the corresponding
 @racket[string-rope-*] operation documented below.

@examples[#:eval rope-eval
 (rope-count (string->rope "abc"))
 (rope-append1 string-rope-ropeable (string->rope "abc") (string->rope "def"))]}

@defproc[(string-raw? [v any/c]) boolean?]{ An alias for @racket[string?]. }

@defproc[(string-raw-limit) exact-nonnegative-integer?]{

 Returns the maximum number of characters a single leaf may hold: @racket[512].}

@defproc[(string-raw-empty) string?]{ Returns @racket[""]. }
@defproc[(string-raw-count [raw string?]) exact-nonnegative-integer?]{
 An alias for @racket[string-length].}
@defproc[(string-raw-width [raw string?]) exact-nonnegative-integer?]{
 An alias for @racket[string-length].}
@defproc[(string-raw-slice [raw string?]
                            [start exact-nonnegative-integer?]
                            [end exact-nonnegative-integer?])
         string?]{
 An alias for @racket[substring].}
@defproc[(string-raw-ref [raw string?] [pos exact-nonnegative-integer?]) char?]{
 An alias for @racket[string-ref].}
@defproc[(string-raw-append [raw string?] ...) string?]{
 An alias for @racket[string-append].}

@defproc[(make-string-rope-leaf [raw string?]) string-rope-leaf?]{

 Wraps @racket[raw] directly in a leaf, without checking it against
 @racket[(string-raw-limit)]. Prefer @racket[string->rope] unless you know
 @racket[raw] is already short enough.}

@defthing[empty-string-rope string-rope?]{ The empty string rope. }

@defproc[(string->rope [str string?]) string-rope?]{

 Builds a balanced rope out of @racket[str]. @math{O(n)}.

@examples[#:eval rope-eval
 (string->rope "a balanced tree of text")]}

@defproc[(rope->string [rope string-rope?]) string?]{

 Flattens @racket[rope] into an ordinary string. Time proportional to the number
 of leaves.}

@defproc[(string-rope-append1 [left string-rope?] [right string-rope?]) string-rope?]{

 Joins two string ropes, repairing balance if needed. Amortized @math{O(log n)}.}

@defproc[(string-rope-append [rope string-rope?] ...) string-rope?]{

 Joins any number of string ropes left to right.

@examples[#:eval rope-eval
 (rope->string
  (string-rope-append (string->rope "one ")
                      (string->rope "two ")
                      (string->rope "three")))]}

@defproc[(string-rope-split [rope string-rope?] [i exact-nonnegative-integer?])
         (values string-rope? string-rope?)]{

 Splits @racket[rope] at character index @racket[i]. Amortized @math{O(log n)}.}

@defproc[(string-rope-offset-index [rope string-rope?] [ofs exact-nonnegative-integer?])
         exact-nonnegative-integer?]{

 Returns the character index at offset @racket[ofs]. Since @racket[count] and
 @racket[width] coincide for strings, this is simply @racket[ofs] itself, modulo
 range checking; the operation exists chiefly for parity with
 @racket[rope-offset-index].}

@defproc[(string-rope-splice [rope string-rope?]
                              [start exact-nonnegative-integer?]
                              [old-len exact-nonnegative-integer?]
                              [new-str string?])
         string-rope?]{

 Replaces @racket[old-len] characters of @racket[rope] beginning at @racket[start]
 with @racket[new-str]. @math{O(log n + m)}, where @racket[m] is
 @racket[(string-length new-str)].

@examples[#:eval rope-eval #:label "Example:"
 (define r (string->rope "a tree of text"))
 (rope->string (string-rope-splice r 2 4 "rope"))]}

@defproc[(string-rope-slice [rope string-rope?]
                             [start exact-nonnegative-integer?]
                             [len exact-nonnegative-integer?])
         string-rope?]{

 Extracts @racket[len] characters of @racket[rope] beginning at @racket[start],
 analogous to @racket[substring].}

@defproc[(string-rope-compare [rope1 string-rope?] [rope2 string-rope?])
         (or/c '< '= '>)]{ See @racket[rope-compare]. }
@defproc[(string-rope=? [rope1 string-rope?] [rope2 string-rope?]) boolean?]{
 See @racket[rope=?].}
@defproc[(string-rope<?  [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope<=? [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope>?  [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope>=? [rope1 string-rope?] [rope2 string-rope?]) boolean?]{
 Versions of @racket[string<?]/@racket[string<=?]/@racket[string>?]/
 @racket[string>=?] for ropes, without flattening either argument.}

@defproc[(string-rope-ci-compare [rope1 string-rope?] [rope2 string-rope?])
         (or/c '< '= '>)]{
 Like @racket[string-rope-compare], but case-folds each comparison, as
 @racket[string-ci<?]/@racket[string-ci=?] do.}
@defproc[(string-rope-ci<?  [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope-ci<=? [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope-ci>?  [rope1 string-rope?] [rope2 string-rope?]) boolean?]
@defproc[(string-rope-ci>=? [rope1 string-rope?] [rope2 string-rope?]) boolean?]{
 Case-insensitive versions of the above.}

@defproc[(string-cursor-at-end? [cursor cursor?]) boolean?]{ See @racket[cursor-at-end?]. }
@defproc[(string-cursor-peek [cursor cursor?]) (or/c #f char?)]{ See @racket[cursor-peek]. }
@defproc[(string-cursor-advance [cursor cursor?]) cursor?]{ See @racket[cursor-advance]. }
@defproc[(string-cursor-drop [cursor cursor?] [k exact-nonnegative-integer?]) cursor?]{
 See @racket[cursor-drop].}
@defproc[(string-cursor-take [cursor cursor?] [k exact-nonnegative-integer?]) string-rope?]{
 See @racket[cursor-take].}
@defproc[(string-rope->cursor [rope string-rope?]) cursor?]{ See @racket[rope->cursor]. }
@defproc[(cursor->string-rope [cursor cursor?]) string-rope?]{ See @racket[cursor->rope]. }

@defproc[(string-rope-foldl [proc procedure?] [init any/c] [rope string-rope?] ...+) any/c]{
 See @racket[rope-foldl].

@examples[#:eval rope-eval
 (string-rope-foldl (λ (acc ch) (cons ch acc)) '() (string->rope "abc"))]}

@defproc[(string-rope-foldr [proc procedure?] [init any/c] [rope string-rope?] ...+) any/c]{
 See @racket[rope-foldr].}

@defproc[(in-string-rope [rope string-rope?]) sequence?]{

 Returns a sequence of the characters of @racket[rope], in order. Recognized by
 @racket[for] and related forms as a specially optimized sequence, but also usable
 as an ordinary sequence value.

@examples[#:eval rope-eval
 (for/list ([ch (in-string-rope (string->rope "rope"))]) ch)]}

@defproc[(open-input-string-rope [rope string-rope?]) input-port?]{

 Returns an input port that reads the characters of @racket[rope], UTF-8 encoded,
 without first flattening @racket[rope] into a single string. Each read of
 @math{k} bytes costs @math{O(k)}; reading the whole rope costs @math{O(n)}
 overall, since a character is a small, constant number of bytes.

@examples[#:eval rope-eval
 (define in (open-input-string-rope (string->rope "line one\nline two")))
 (read-line in)]}

@; -------------------------------------------------------------------------------------------------

@section{Byte Ropes}

@defmodule[rope/bytes]

A rope specialized for @racket[bytes?] chunks, generated by
@racket[define-rope-type] in exactly the same way as
@racketmodname[rope/string]. Every operation below is the byte-string version of
the corresponding string-rope operation described in the previous section.

@defstruct*[(bytes-rope-leaf rope-leaf) ([count exact-nonnegative-integer?]
                                         [width exact-nonnegative-integer?]
                                         [raw bytes?])]{

 A leaf of a byte rope, holding a byte string of at most @racket[(bytes-raw-limit)]
 bytes.}

@defstruct*[(bytes-rope-node rope-node) ([count exact-nonnegative-integer?]
                                         [width exact-nonnegative-integer?]
                                         [left bytes-rope?]
                                         [right bytes-rope?])]{

 An internal node of a byte rope.}

@defproc[(bytes-rope? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @racket[bytes-rope-leaf?] or
 @racket[bytes-rope-node?].}

@defthing[bytes-rope-ropeable ropeable?]{

 The @racket[gen:ropeable] instance for @racket[bytes-rope]s. Passing this to
 an operation from @racketmodname[rope/rope], such as @racket[rope-append1] or
 @racket[rope->cursor], behaves exactly like the corresponding
 @racket[bytes-rope-*] operation documented below.}

@defproc[(bytes-raw? [v any/c]) boolean?]{ An alias for @racket[bytes?]. }
@defproc[(bytes-raw-limit) exact-nonnegative-integer?]{
 Returns the maximum number of bytes a single leaf may hold: @racket[512].}
@defproc[(bytes-raw-empty) bytes?]{ Returns @racket[#""]. }
@defproc[(bytes-raw-count [raw bytes?]) exact-nonnegative-integer?]{
 An alias for @racket[bytes-length].}
@defproc[(bytes-raw-width [raw bytes?]) exact-nonnegative-integer?]{
 An alias for @racket[bytes-length].}
@defproc[(bytes-raw-slice [raw bytes?]
                           [start exact-nonnegative-integer?]
                           [end exact-nonnegative-integer?])
         bytes?]{
 An alias for @racket[subbytes].}
@defproc[(bytes-raw-ref [raw bytes?] [pos exact-nonnegative-integer?]) byte?]{
 An alias for @racket[bytes-ref].}
@defproc[(bytes-raw-append [raw bytes?] ...) bytes?]{
 An alias for @racket[bytes-append].}

@defproc[(make-bytes-rope-leaf [raw bytes?]) bytes-rope-leaf?]{

 Wraps @racket[raw] directly in a leaf, without checking it against
 @racket[(bytes-raw-limit)]. Prefer @racket[bytes->rope] unless you know
 @racket[raw] is already short enough.}

@defthing[empty-bytes-rope bytes-rope?]{ The empty byte rope. }

@defproc[(bytes->rope [bstr bytes?]) bytes-rope?]{

 Builds a balanced rope out of @racket[bstr]. @math{O(n)}.}

@defproc[(rope->bytes [rope bytes-rope?]) bytes?]{

 Flattens @racket[rope] into an ordinary byte string.}

@defproc[(bytes-rope-append1 [left bytes-rope?] [right bytes-rope?]) bytes-rope?]{

 Joins two byte ropes, repairing balance if needed.}

@defproc[(bytes-rope-append [rope bytes-rope?] ...) bytes-rope?]{

 Joins any number of byte ropes left to right.}

@defproc[(bytes-rope-split [rope bytes-rope?] [i exact-nonnegative-integer?])
         (values bytes-rope? bytes-rope?)]{

 Splits @racket[rope] at byte index @racket[i].}

@defproc[(bytes-rope-offset-index [rope bytes-rope?] [ofs exact-nonnegative-integer?])
         exact-nonnegative-integer?]{

 Returns the byte index at offset @racket[ofs]; as with
 @racket[string-rope-offset-index], @racket[count] and @racket[width] coincide for
 byte ropes.}

@defproc[(bytes-rope-splice [rope bytes-rope?]
                             [start exact-nonnegative-integer?]
                             [old-len exact-nonnegative-integer?]
                             [new-bstr bytes?])
         bytes-rope?]{

 Replaces @racket[old-len] bytes of @racket[rope] beginning at @racket[start] with
 @racket[new-bstr].

@examples[#:eval rope-eval #:label "Example:"
 (define r (bytes->rope #"hello world"))
 (rope->bytes (bytes-rope-splice r 6 5 #"racket"))]}

@defproc[(bytes-rope-slice [rope bytes-rope?]
                            [start exact-nonnegative-integer?]
                            [len exact-nonnegative-integer?])
         bytes-rope?]{

 Extracts @racket[len] bytes of @racket[rope] beginning at @racket[start],
 analogous to @racket[subbytes].}

@defproc[(bytes-rope-compare [rope1 bytes-rope?] [rope2 bytes-rope?])
         (or/c '< '= '>)]{ See @racket[rope-compare]. }
@defproc[(bytes-rope=?  [rope1 bytes-rope?] [rope2 bytes-rope?]) boolean?]{
 See @racket[rope=?].}
@defproc[(bytes-rope<?  [rope1 bytes-rope?] [rope2 bytes-rope?]) boolean?]
@defproc[(bytes-rope<=? [rope1 bytes-rope?] [rope2 bytes-rope?]) boolean?]
@defproc[(bytes-rope>?  [rope1 bytes-rope?] [rope2 bytes-rope?]) boolean?]
@defproc[(bytes-rope>=? [rope1 bytes-rope?] [rope2 bytes-rope?]) boolean?]{
 Versions of @racket[bytes<?]//@racket[bytes>?] for ropes, without flattening
 either argument.}

@defproc[(bytes-cursor-at-end? [cursor cursor?]) boolean?]{ See @racket[cursor-at-end?]. }
@defproc[(bytes-cursor-peek [cursor cursor?]) (or/c #f byte?)]{ See @racket[cursor-peek]. }
@defproc[(bytes-cursor-advance [cursor cursor?]) cursor?]{ See @racket[cursor-advance]. }
@defproc[(bytes-cursor-drop [cursor cursor?] [k exact-nonnegative-integer?]) cursor?]{
 See @racket[cursor-drop].}
@defproc[(bytes-cursor-take [cursor cursor?] [k exact-nonnegative-integer?]) bytes-rope?]{
 See @racket[cursor-take].}
@defproc[(bytes-rope->cursor [rope bytes-rope?]) cursor?]{ See @racket[rope->cursor]. }
@defproc[(cursor->bytes-rope [cursor cursor?]) bytes-rope?]{ See @racket[cursor->rope]. }

@defproc[(bytes-rope-foldl [proc procedure?] [init any/c] [rope bytes-rope?] ...+) any/c]{
 See @racket[rope-foldl].}
@defproc[(bytes-rope-foldr [proc procedure?] [init any/c] [rope bytes-rope?] ...+) any/c]{
 See @racket[rope-foldr].}

@defproc[(in-bytes-rope [rope bytes-rope?]) sequence?]{

 Returns a sequence of the bytes of @racket[rope], in order.}

@defproc[(open-input-bytes-rope [rope bytes-rope?]) input-port?]{

 Returns an input port that reads the bytes of @racket[rope] directly, without
 first flattening it into a single byte string.}

@; -------------------------------------------------------------------------------------------------

@section[#:tag "Defining_New_Rope_Types"]{Defining New Rope Types}

@defmodule[rope/define-rope-type]

@defform[#:literals ()
 (define-rope-type type raw? raw-limit raw-empty raw-count raw-width
                    raw-slice raw-append raw-ref)
 #:contracts ([raw? (any/c . -> . boolean?)]
              [raw-limit (-> exact-nonnegative-integer?)]
              [raw-empty (-> any/c)]
              [raw-count (any/c . -> . exact-nonnegative-integer?)]
              [raw-width (any/c . -> . exact-nonnegative-integer?)]
              [raw-slice (any/c exact-nonnegative-integer?
                                 exact-nonnegative-integer? . -> . any/c)]
              [raw-append (list? . -> . any/c)]
              [raw-ref (any/c exact-nonnegative-integer? . -> . any/c)])]{

 Generates a complete, efficient rope implementation specialized to a chunk type
 named @racket[type], analogous to the ones this library defines for
 @racket[string] and @racket[bytes]. @racket[type] is an identifier used as a
 prefix for every generated binding; the other arguments are expressions
 supplying the pieces of the @racket[gen:ropeable] interface described in
 @secref["The_gen_ropeable_Interface"]:

@itemlist[
 @item{@racket[raw?] is a one-argument predicate identifying valid chunks;}
 @item{@racket[raw-limit] is a zero-argument procedure returning the maximum
   @tech{count} of a single leaf;}
 @item{@racket[raw-empty] is a zero-argument procedure constructing an empty
   chunk;}
 @item{@racket[raw-count] and @racket[raw-width] are one-argument procedures
   returning a chunk's @tech{count} and @tech{width};}
 @item{@racket[raw-slice] is a three-argument procedure, taking a chunk followed
   by start and end indices, and returning a sub-chunk;}
 @item{@racket[raw-append] is a one-argument procedure that takes a
   @emph{list} of chunks and returns their concatenation; and}
 @item{@racket[raw-ref] is a two-argument procedure that takes a chunk and an index
   and returns a single element.}]

 Given these, @racket[define-rope-type] defines, among others, the following
 bindings, where @racket[_type] stands for the identifier bound to @racket[type]:

@itemlist[
 @item{structs @racket[_type-rope-leaf] and @racket[_type-rope-node], both
   subtypes of @racket[rope], and a predicate @racket[_type-rope?];}
 @item{@racket[_type-rope-ropeable], the @racket[gen:ropeable] instance itself---a
   first-class witness suitable for passing as the @racket[ropeable] argument to
   any operation in @secref{Generic_Rope_Operations}, for callers that want
   to work through the generic @racketmodname[rope/rope] API directly instead of
   the type-specific wrappers below;}
 @item{@racket[_type-raw?], @racket[_type-raw-limit], @racket[_type-raw-empty],
   @racket[_type-raw-count], @racket[_type-raw-width], @racket[_type-raw-slice],
   @racket[_type-raw-append], and @racket[_type-raw-ref], each specialized
   versions of the procedure supplied above;}
 @item{@racket[make-_type-rope-leaf], @racket[make-empty-_type-rope],
   @racket[_type-rope-append1], @racket[_type-rope-append],
   @racket[_type-rope-split], @racket[_type-rope-offset-index],
   @racket[_type-rope-splice], @racket[_type-rope-slice],
   @racket[_type->-rope], and @racket[rope->_type], the
   specialized construction and editing operations from
   @secref["Generic_Rope_Operations"];}
 @item{@racket[_type-rope-compare], @racket[_type-rope-compare-with],
   @racket[_type-rope=?], @racket[_type-rope<?], @racket[_type-rope<=?],
   @racket[_type-rope>?], and @racket[_type-rope>=?], the specialized
   comparison operations (raises at call time unless @racket[#:compare] was
   supplied to @racket[define-rope-type]);}
 @item{@racket[_type-cursor-at-end?], @racket[_type-cursor-peek],
   @racket[_type-cursor-advance], @racket[_type-cursor-drop],
   @racket[_type-cursor-take], @racket[_type-rope->cursor], and
   @racket[cursor->_type-rope], the specialized cursor operations; and}
 @item{@racket[_type-rope-foldl], @racket[_type-rope-foldr], and
   @racket[in-_type-rope], the specialized fold and sequence operations.}]

 @racket[define-rope-type] does not @racket[provide] any of these bindings; it
 only defines them in the enclosing module. A module that uses
 @racket[define-rope-type] is expected to re-export the bindings it wants to make
 public, typically via @racket[rope-type-out] or @racket[rope-type-out/contract]
 (below).}

@defform[(rope-type-out type)]{

 A @racket[provide] sub-form. Re-exports every public binding
 @racket[define-rope-type] introduces for @racket[type]---the structs,
 predicates, raw operations, rope operations, cursor operations, folds,
 @racket[in-_type-rope], and the @racket[gen:ropeable] witness
 @racket[_type-rope-ropeable]---under their generated names, with @emph{no}
 contracts attached. Implementation plumbing (@racket[_type-rope-gen], the
 leaf/node constructor selectors, and @racket[in-_type-rope-runtime]) is
 excluded; none of it is meant to be called directly.

 Use this inside a trusted module where paying for contract checks at every
 rope operation isn't worthwhile.}

@defform[
 (rope-type-out/contract type maybe-raw maybe-element)
 #:grammar
 ([maybe-raw     (code:line) (code:line #:raw raw-contract-expr)]
  [maybe-element (code:line) (code:line #:element element-contract-expr)])
 #:contracts ([raw-contract-expr contract?]
              [element-contract-expr contract?])]{

 Like @racket[rope-type-out], but wraps every re-exported procedure in
 @racket[contract-out]. Without @racket[#:raw] or @racket[#:element], raw values
 are checked against the generic @racket[_type-raw?] and individual elements
 against @racket[any/c], which is sound but as loose as the generic protocol allows.
 @racket[#:raw] and @racket[#:element] tighten those two positions to whatever
 concrete contract the instantiating module actually promises, .e.g., @racket[string?]
 and @racket[char?] for @racketmodname[rope/string], @racket[bytes?] and
 @racket[byte?] for @racketmodname[rope/bytes].

 @racket[in-_type-rope] is re-exported bare, since a sequence macro has no
 useful contract. Its runtime fallback @racket[in-_type-rope-runtime], which is used
 when the sequence is handed to a higher-order function like
 @racket[sequence-map] instead of appearing directly in a @racket[for] clause,
 @emph{is} contracted and re-exported alongside it.

 @racket[_type-rope-ropeable] is contracted with @racket[ropeable?]. It is a
 first-class @racket[gen:ropeable] witness, meant to be handed to those operations
 directly.}

 Note that @racket[_type-cursor-peek]'s contract always admits @racket[#f] in
 addition to @racket[element-contract-expr], since @racket[#f] signals that the
 cursor has run out of elements rather than being itself an element value.

As a complete example, here is a rope type over immutable vectors, exposing only
the operations needed to build, edit, and read one back:

@racketmod[
racket/base

(require racket/contract
         rope/define-rope-type
         rope/rope)

(provide
 (contract-out
  [vector-rope?         (any/c . -> . boolean?)]
  [vector->rope         (vector? . -> . vector-rope?)]
  [rope->vector         (vector-rope? . -> . vector?)]
  [vector-rope-append   (vector-rope? ... . -> . vector-rope?)]
  [vector-rope-splice   (vector-rope? exact-nonnegative-integer?
                                      exact-nonnegative-integer? vector? . -> . vector-rope?)]
  [vector-rope-slice    (vector-rope? exact-nonnegative-integer?
                                      exact-nonnegative-integer? . -> . vector-rope?)]))

(define-rope-type vector
  vector?
  (λ () 256)
  (λ () (vector))
  vector-length
  vector-length
  (λ (v start end) (vector-copy v start end))
  (λ (vs) (apply vector-append vs))
  vector-ref)
]

Every operation on the resulting @racket[vector-rope] type (appending, splicing,
slicing, folding, iterating with @racket[in-vector-rope]) behaves exactly like
its string and byte-string counterparts, at the same complexity, without any
further code.

The @racket[provide] clause above spells out each binding's contract by hand,
which is worth doing for a small, curated surface like this one. However, it omits
@racket[vector-rope-ropeable]. If a module wants callers to be able to drop
down to the generic @racketmodname[rope/rope] operations, it would add
@racket[(contract-out [vector-rope-ropeable ropeable?])] to its
@racket[provide] clause, or simply re-export it bare, as
@racket[rope-type-out]/@racket[rope-type-out/contract] do. A type that wants
to expose everything @racket[define-rope-type] generates can skip the
boilerplate entirely:

@racketblock[
 (provide (rope-type-out/contract vector #:raw vector?))
]

@; -------------------------------------------------------------------------------------------------

@section{Performance Summary}

The following table summarizes the running time of the principal operations, for
a rope of @math{n} elements. @italic{Amortized} entries can occasionally cost more
on a single call, when a rebalancing rebuild is triggered, but never more than
@math{O(log n)} on average over a sequence of edits.

@tabular[
 #:style 'boxed
 #:column-properties '(left center)
 #:row-properties '(bottom-border ())
 (list
  (list @bold{Operation}                  @bold{Time})
  (list @racket[rope-length]              "O(1)")
  (list @racket[rope-depth]                "O(1)")
  (list @racket[rope-balanced?]            "O(1)")
  (list @racket[rope-append]               "O(log n) amortized")
  (list @racket[rope-split]                "O(log n) amortized")
  (list @racket[rope-splice]               "O(log n + m) amortized")
  (list @racket[rope-slice]                "O(log n + k) amortized")
  (list @elem{@racket[rope-compare]/@racket[rope<?]/etc.} "O(log n + d) amortized")
  (list @racket[rope-offset-index]         "O(log n)")
  (list @elem{@racket[cursor-peek]/@racket[cursor-advance]} "O(1) amortized")
  (list @racket[cursor-drop]               "O(log n)")
  (list @racket[cursor-take]               "O(log n)")
  (list @elem{@racket[rope-foldl]/@racket[rope-foldr]}      "O(n)")
  (list @racket[raw->rope]                 "O(n)")
  (list @racket[rope->raw]                 "O(# leaves)")
  (list @elem{@racket[equal?]}             "O(n) worst case, O(1) if eq? or already hashed")
  (list @elem{@racket[equal-hash-code]}    "O(n) cold, O(1) amortized (cached)")
  )]

Here @math{m} is the count of the chunk being spliced in, @math{k} is the
number of leaves spanned by an extracted slice, and @math{d} is the position of
the first difference.

@; -------------------------------------------------------------------------------------------------

@section[#:tag "Measured_Performance"]{Measured Performance}

The numbers below come from this package's own benchmark suite
(documented in @tt{benchmark/README.md}), run against a freshly built
package so results reflect compiled (not interpreted) code.

@bold{Test machine:} benchmarks were performed on a desktop PC with a
@italic{12th Gen Intel(R) Core(TM) i7-12700K with 12 cores (8 P-cores,
4 E-cores) and 20 threads, 32GiB DDR5 DRAM at 4800 MT/s, on Arch linux
(July 2026) with Racket v9.2 [cs].}

@bold{Methodology:} default suite settings unless noted---sizes
@tt{0, 8, 64, 512, 4096, 32768}, 15 measured trials per benchmark after 5
untimed warmup calls, forced GC before each trial. Warm benchmarks are
batch-calibrated to at least 5ms per batch before timing (see
@tt{benchmark/README.md}'s @italic{Timer resolution and batching}
section); cold benchmarks are measured unbatched, one fresh fixture per
trial. Every number below is a mean over its trials, not a single
sample.

@bold{Reproducing these numbers:}
@verbatim{
 racco setup rope
 racket benchmark/main.rkt --save /tmp/run.json
 racket benchmark/visualize.rkt --in /tmp/run.json --order scenario
}

@image["images/benchmark-overview.svg" #:scale 1.0]{Every benchmarked
 operation, one row each, sorted fastest to slowest, one dot per input
 size on a shared log-scale time axis. Color indicates operation category.}

Here are some of the more impactful figures from that chart, at the largest
test size (32,768 elements):

@tabular[
 #:style 'boxed
 #:column-properties '(left center)
 #:row-properties '(bottom-border ())
 (list
  (list @bold{Operation}                        @bold{Time at n = 32,768})
  (list "append1 (leaf-level)"                   "~110-120ns")
  (list "split at midpoint"                      "~3us")
  (list "slice, one quarter of the rope"         "~5-6us")
  (list "splice 4 elements at the midpoint"       "~9us")
  (list "offset-index at the midpoint"             "~10-11us")
  (list "build from a flat raw chunk"               "~30-170us")
  (list "equal-hash-code, already hashed (warm)"      "<60ns, flat across all sizes")
  (list "equal-hash-code, never hashed before (cold)" "~5.8-6.0ms")
  (list "equal?, same content (post-optimization, see below)" "~200-550us"))]

The @tt{*-build} rows in the full chart (@tt{typed-build},
@tt{fragmented-build}, @tt{edited-build}) are the slowest operations
measured, at tens to hundreds of milliseconds. This is expected, since
they simulate building a rope by thousands of individual edits, which
dominates the elapsed real time cost of running the full suite.
Everything else in the table is a single operation on an already-built rope.

@subsection{Case study: optimizing @racket[equal?]}

As a concrete example of how the benchmarking suite is used, here are the
results of a real optimization made during development. The original
content-equality check compared two ropes element by element, and
it was changed to compare whole overlapping chunks at once. This was the
same strategy @racket[rope-compare] already used, and we hypothesized that
one @racket[equal?] call per chunk would beat many individual per-character
calls.

@image["images/benchmark-equal-optimization-delta.svg" #:scale 1.0]{
 Percent change in mean time, new implementation vs. old, one row per
 operation/scenario, one dot per input size. Green = a significant
 improvement, red = a significant regression, gray = within measurement
 noise (a shaded band around each dot spans roughly two standard
 deviations either side, scaled to percent of the baseline mean).}

The result was 74-98% reductions (roughly 8-13x) across every scenario
that requires walking real content, with no measurable effect on
unrelated operations. We also observed a regression---immediate-mismatch
cases became a few microseconds slower. This was expected, since the new
strategy pays for slicing a whole chunk up front. See @tt{benchmark/README.md}
for how to produce the same kind of before/after comparison for your own patches.

@(close-eval rope-eval)
