#lang racket/base

(require racket/syntax
         "./error.rkt")

(provide (all-defined-out))

(struct rope-type (bindings) #:transparent)

(define (describe-type τ-stx)
  (define desc-id (format-id τ-stx "rope:~a" (syntax-e τ-stx)))
  (or (syntax-local-value desc-id (λ () #f))
      (rope-error "expected a rope type name")))
