#lang racket/base

(require rope/type)

(provide (all-defined-out))

(define-rope-type bytes
  #:chunk?       bytes?
  #:chunk-limit  512
  #:chunk-empty  #""
  #:chunk-length bytes-length
  #:chunk-ref    bytes-ref
  #:chunk-slice  (λ (c i k) (subbytes c i (+ i k)))
  #:chunk-append (λ (cs) (apply bytes-append cs))
  #:elem-width   1
  ;; content-based equality
  #:chunk=?      bytes=?
  #:elem=?       =
  #:elem-hash    values)
