#lang racket/base

(require (for-syntax racket/base
                     "../private/signature.rkt"
                     "../private/type.rkt")
         rackunit
         rope/signature
         rope/type
         "../private/testing.rkt")

(define-test-rope-type tty)

;; (pattern legacy realized-symbol) for each signature member of type ty
(define-syntax (realized-table stx)
  (syntax-case stx ()
    [(_ ty)
     (let ([desc (lookup-type 'realized-table stx #'ty)]
           [sig (syntax-local-value #'rope-signature)])
       #`'#,(for/list ([spec (in-list (signature-specs sig))])
              (list (member-spec-pattern spec)
                    (syntax-e (rope-type-ref desc (member-spec-key spec))))))]))

(define table (realized-table tty))

(test-case "every realized name follows its pattern"
  (for ([row (in-list table)])
    (check-equal? (symbol->string (cadr row))
                  (format (car row) "tty"))))

(test-case "type-member returns the realized identifier"
  (check-eq? (type-member tty chunk-ref) tty-chunk-ref)
  (check-eq? (type-member tty rope-concat) tty-rope-concat)
  (check-eq? (type-member tty make-rope-leaf) make-tty-rope-leaf))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define (check-expand-error rx datum)
  (check-exn rx (λ () (eval datum ns))))

(test-case "error messages"
  (check-expand-error #rx"unknown type member"
                      '(type-member tty chunk-reff))
  (check-expand-error #rx"expected a rope type name"
                      '(type-member nope chunk-ref))
  (check-expand-error #rx"declared in rope-signature but not defined"
                      '(realize-rope-type (t t2) rope-signature
                                          (define-member (t.chunk? x) #t)))
  (check-expand-error #rx"unknown type member"
                      '(realize-rope-type (t t3) rope-signature
                                          (define-member (t.chunk? x) (t.chunk-reff x))))
  (check-expand-error #rx"allowed only"
                      '(define-member (t.chunk? x) #t)))
