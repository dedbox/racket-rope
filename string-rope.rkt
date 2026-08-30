#lang racket/base

;; rope/string-rope.rkt

(require rope2/rope-type)

(provide (all-defined-out))

(define-rope-type string
  #:chunk?       string?
  #:chunk-limit  512
  #:chunk-empty  ""
  #:chunk-length string-length
  #:chunk-ref    string-ref
  #:chunk-slice  (λ (c i k) (substring c i (+ i k)))
  #:chunk-append (λ (cs) (apply string-append cs))
  #:elem-width   1
  #:elem-hash    char->integer)

(require rope2/generic-ops)
