#lang info

(define collection "rope")
(define version "1.1")
(define pkg-authors '("Eric Griffis <dedbox@gmail.com>"))
(define pkg-desc "A high-performance generic rope library for real-time text processing.")
(define license '(MIT OR Apache-2.0))

(define deps
  '("base"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/rope.scrbl" ())))
