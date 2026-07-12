#lang racket/base

(require rope/define-rope-type
         rope/rope)

(provide (all-defined-out))

(define STRING-LEAF-LIMIT 512)

(define string-rope-ops
  (rope-ops STRING-LEAF-LIMIT
            (λ () "")
            string-length
            string-length
            substring
            (λ (raws) (apply string-append raws))
            string-ref))

(define-rope-type string string-rope-ops)

