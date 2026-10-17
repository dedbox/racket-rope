#lang racket/base

(require racket/set)

(provide (all-defined-out))

;; key     : symbol
;; formals : list of formal parameters (for docs)
;; pattern : string with exactly one "~a"
;; op      : name of generic op wrapper, or #f
;; kind    : function | struct | syntax
;; doc     : string or #f
(struct member-spec (key formals pattern op kind doc) #:transparent)

;; name  : symbol
;; specs : (listof member-spec) in order of declaration
;; index : immutable hasheq, key → member-spec
(struct signature (name specs index) #:transparent)

(define (build-signature name specs)
  (define index (for/hasheq ([spec (in-list specs)]) (values (member-spec-key spec) spec)))
  (signature name specs index))

(define (signature-ref sig key) (hash-ref (signature-index sig) key #f))
(define (signature-keys sig) (map member-spec-key (signature-specs sig)))

(define (valid-pattern? str) (and (string? str) (regexp-match? #px"^[^~]*~a[^~]*$" str)))
(define (default-pattern key) (string-append "~a-" (symbol->string key)))

(define (find-duplicate-key ids)
  (let loop ([ids ids] [seen (seteq)])
    (cond [(null? ids) #f]
          [(set-member? seen (syntax-e (car ids))) (car ids)]
          [else (loop (cdr ids) (set-add seen (syntax-e (car ids))))])))

(define (lookup-signature who id stx)
  (define x (and (identifier? id) (syntax-local-value id (λ () #f))))
  (if (signature? x) x
      (raise-syntax-error who "expected a rope signature" stx id)))
