#lang racket/base

(require racket/syntax)

(provide (all-defined-out))

;; name         : name of this type (e.g., string)
;; self         : identifier bound to this descriptor (e.g., rope:string)
;; signature-id : indentifier bound to the signature this type realizes
;; members      : immutable hasheq, key → realized identifier
(struct rope-type (name self signature-id members) #:transparent)

(define (rope-type-ref desc key) (hash-ref (rope-type-members desc) key #f))

(define (lookup-type who stx id)
  (define (maybe v) (and (rope-type? v) v))
  (or (and (identifier? id)
           (or (maybe (syntax-local-value id (λ () #f)))
               (maybe (syntax-local-value (format-id id "rope:~a" (syntax-e id)) (λ () #f)))))
      (raise-syntax-error who "expected a rope type name" stx id)))
