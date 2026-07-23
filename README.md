# rope

A [rope](https://en.wikipedia.org/wiki/Rope_(data_structure)) data structure
for Racket: a persistent, efficiently editable stand-in for strings, byte
strings, or any other chunkable sequence.

Splitting, splicing, or concatenating an ordinary Racket string of length `n`
takes `O(n)` steps&mdash;both `substring` and `string-append` have to walk
(and copy) the whole string. A rope represents the same sequence as a balanced
tree of small chunks, so the same operations takes `O(log n)` steps. A text
editor or language server that repeatedly edits a large buffer this way pays
`O(log n)` per edit instead of `O(n)`, so it stays fast as the buffer grows.

This library includes pre-defined rope types for `string?` and `bytes?`
chunks, and a `define-rope-type` macro for building new types over any other
chunk representation with a notion of slice, append, and index.

Full documentation is available via `raco docs rope`, on
[`https://docs.racket-lang.org/rope/`](docs.racket-lang.org/rope/), or in
[`rope.scrbl`](./scribblings/rope.scrbl).

## Why Ropes?

If you are building an editor, ropes are an efficient alternative to strings.

- **Plain text editors** need to insert, delete, and scroll through
  arbitrarily large buffers and at interactive speeds. With a rope, these
  operations will remain fast, even as the buffer grows.
- **Programming editors and language servers** require incremental lexing and
  parsing on top of the buffer. Ropes provide a stable and efficient medium
  for manipulating arbitrary spans and offsets in real time, so these
  processes can efficiently target only the portion of the buffer that
  actually changed.
- **Structured or semi-structured editors** such as rich text editors and
  tabular GUI widgets can leverage the `define-rope-type` macro to generate
  the same machinery over custom chunk types for managing runs of styled
  spans, or a row of cells.

## An Example

```racket
#lang racket/base

(require rope)

(define doc (string->rope "Hello!"))
(define greeting (string-rope-splice doc 5 0 " World"))
(rope->string greeting)                        ; => "Hello World!"

;; the original is untouched -- edits never mutate their argument
(rope->string doc)                             ; => "Hello!"

;; ropes are sequences too
(for/list ([ch (in-string-rope greeting)] #:when (char-upper-case? ch)) ch)
;; => '(#\H #\W)
```

## Installation

```sh
raco pkg install rope
```

or, to track the repository directly:

```sh
git clone https://github.com/dedbox/racket-rope.git
cd racket-rope
raco pkg install -n rope
```

## Running the Tests

```sh
raco test .
```

## Benchmarking

See [`benchmark/README.md`](./benchmark/README.md) for the microbenchmark
suite used to produce the performance numbers in the documentation, including
instructions for comparing before and after a change, and for rendering the
results as an SVG chart.

## Contributing / Reporting Bugs

Bug reports, feature requests, and pull requests are welcome via the
[GitHub issue tracker](https://github.com/dedbox/racket-rope/issues).

If you are reporting a bug, please include a minimal reproduction and your
Racket version (`racket --version`). If you are proposing a change to the core
`rope`/`define-rope-type` machinery, a before/after run of the benchmark suite
is a good way to show that the change don't regress performance.

For anything larger than a small fix, opening an issue to discuss the approach
first is appreciated before you put time into a pull request.

## License

Licensed under either of

- Apache License, Version 2.0 ([`LICENSE-APACHE`](./LICENSE-APACHE))
- MIT license ([`LICENSE-MIT`](./LICENSE-MIT))

at your option. In short: you can use, modify, and redistribute this software,
including in proprietary and commercial products, without any obligation to
share your own source code; you just need to keep the copyright and license
notice of whichever license you choose intact. The Apache 2.0 option
additionally includes an express grant of patent rights from contributors,
terminated for anyone who initiates patent litigation over the software; pick
that one if the patent grant matters to you, or MIT if you need the simpler,
more broadly-compatible text (for example, to combine this code with a
GPLv2-only project, which Apache 2.0 alone cannot do).

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this project, as defined in the Apache-2.0 license,
shall be dual-licensed as above, without any additional terms or conditions.
