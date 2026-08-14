#lang info

(define collection "rope2")
(define version "0.1")
(define pkg-authors '("Eric Griffis <dedbox@gmail.com>"))
(define pkg-desc "An alternative to strings.")
(define license '(MIT OR Apache-2.0))

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/rope.scrbl" ())))
