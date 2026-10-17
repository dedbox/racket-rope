#lang racket/base

;;; Lexically scoped type variable binding

(require (for-syntax racket/base
                     syntax/parse
                     "./type.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (with-type-scope (τ:id self:id) ([ref:id target:id] ...) body ...+)
  (let-syntax ([τ (make-rename-transformer #'self)]
               [ref (make-rename-transformer #'target)] ...)
    body ...))

(define-syntax-parse-rule (with-type-members (τ:id type-id:id ([ref:id key:id] ...)) body ...+)
  #:do [(define desc (lookup-type 'with-type-members this-syntax #'type-id))]
  #:with self (rope-type-self desc)
  #:with (target ...) (for/list ([k (in-list (attribute key))])
                        (or (rope-type-ref desc (syntax-e k))
                            (raise-syntax-error 'with-type-members
                              (format "type ~a has no member ~a" (rope-type-name desc) (syntax-e k))
                              this-syntax k)))
  (with-type-scope (τ self) ([ref target] ...) body ...))
