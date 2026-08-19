#lang scribble/manual

@(require scribble/example
          racket/sandbox
          @for-label[racket/base
                     racket/contract
                     rope2])

@(define rope-eval (make-base-eval))
@(rope-eval '(require rope2))

@title{Ropes: An Alternative to Strings}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@defmodule[rope2]

Based on the paper by
@hyperlink["https://doi.org/10.1002/spe.4380251203"]{Boehm, Atkinson, and Plass}.

@table-of-contents[]

@; -----------------------------------------------------------------------------

@section{Introduction}

A rope is a balanced binary tree where each leaf contains a raw chunk of individual elements.
A chunk can be any sequential data structure, such as a string or a vector.

Although ropes are typically used as a drop-in replacement for strings or byte strings,
where the elements are likely to have a fixed size,
this is not a hard requirement.
For example, it is possible to create ropes whose elements are lexical tokens
that carry the text of the words they represent.
This extra precision allows parsers to determine not only the offset of a token,
but also the offset of an individual character in its underlying text.

@; -----------------------------------------------------------------------------

@section{Appendix}

Documentation To-Do
@itemlist[
 @item{contracted vs uncontracted}
 ]

Dev Documentation To-Do
@itemlist[
 @item{generic calls are macros (for performance)}
 @item{clean and precise error messages}
 @item{for overlap=?, default looping algorithem is generally faster
  on strings or collections of Racket-level (i.e., boxed) values,
  but can be astonishingly slower (e.g., than slicing) for special types
  (e.g., bytes - contiguous, unboxed C arrays).}
 @item{there will be no generic function API,
  since falling back to run-time dispatch defies the spirit of the library.
  We will need good docs on how to use the macros instead.}
 ]
@(close-eval rope-eval)
