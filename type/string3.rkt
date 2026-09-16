#lang racket/base

(require rope2/type)

(provide (all-defined-out))

(define-rope-type string3
  #:chunk?       string?
  #:chunk-limit  3
  #:chunk-empty  ""
  #:chunk-length string-length
  #:chunk-ref    string-ref
  #:chunk-slice  (λ (c i k) (substring c i (+ i k)))
  #:chunk-append (λ (cs) (apply string-append cs))
  #:elem-width   1)

;; (define-rope-equality-instance string3)
