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

Documentation To-Do
@itemlist[
 @item{contracted vs uncontracted}
 ]

@(close-eval rope-eval)
