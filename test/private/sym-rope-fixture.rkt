#lang racket/base

(module defined racket/base
  (require racket/vector
           rope/define-rope-type)

  (provide (rope-type-out sym))

  ;; raw = vector of symbols, mirroring define-rope-type-test.rkt's own fixture.
  (define-rope-type sym
    (λ (v) (and (vector? v) (for/and ([e (in-vector v)]) (symbol? e))))
    (λ () 4)
    (λ () (vector))
    vector-length
    vector-length
    (λ (v s e) (vector-copy v s e))
    (λ (raws) (apply vector-append raws))
    vector-ref))

(require 'defined)

(provide (all-from-out 'defined))

(module contracted racket/base
  (require rope/define-rope-type
           (submod ".." defined))
  (provide (rope-type-out/contract sym #:raw sym-raw? #:element symbol?)))

(require (prefix-in contracted- 'contracted))

(provide (all-from-out 'contracted))
