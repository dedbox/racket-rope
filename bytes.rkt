#lang racket/base

(require rope/define-rope-type
         rope/rope)

(provide (all-defined-out))

(define BYTES-LEAF-LIMIT 512)

(define bytes-rope-ops
  (rope-ops BYTES-LEAF-LIMIT
            (λ () #"")
            bytes-length
            bytes-length
            subbytes
            (λ (raws) (apply bytes-append raws))
            bytes-ref))

(define-rope-type bytes bytes-rope-ops)
