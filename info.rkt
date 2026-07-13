#lang info

(define collection "rope")
(define version "0.1")
(define pkg-authors '("Eric Griffis <dedbox@gmail.com>"))
(define pkg-desc "An alternative to strings.")

(define deps
  '("base"
    "reprovide-lang-lib"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings
  '(("scribblings/rope.scrbl" ())))
