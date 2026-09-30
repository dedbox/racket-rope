#lang racket/base

(require (for-template racket/base)
         racket/syntax
         syntax/parse
         "./error.rkt")

(provide (all-defined-out))

(struct rope-class (params reqs body) #:transparent)

(define (describe-class κ-stx)
  (define desc-id
    (format-id κ-stx "rope:~a" (syntax-e κ-stx) #:source κ-stx #:props κ-stx))
  (or (syntax-local-value desc-id (λ () #f))
      (rope-error "expected a rope class name")))

(define (build-class-args κ-name params bindings)
  (for/list ([param (in-list params)])
    (define name (car param))
    (define dfl (syntax-e (class-param-default (cdr param))))
    (cond [(assoc name bindings) => values]
          [(not dfl) (rope-error "expected ~a argument ~a" κ-name name)]
          [(and (pair? dfl) (null? (cdr dfl))) (cons name (car dfl))]
          [else (rope-error "internal error: ~a / ~a" name κ-name)])))

(define (assert-syntax-class-match! ctx class-stx expr-stx)
  (define validate (generate-temporary 'validate))
  (define validator
    #`(let-syntax ([#,validate (λ (stx)
                                 (syntax-parse (quote-syntax #,expr-stx)
                                   #:context (quote-syntax #,ctx)
                                   [(~var _ #,class-stx) #''matched]))])
        (#,validate)))
  (local-expand validator 'expression null)
  (void))

(struct class-param (stxclass default) #:transparent)

;; `default` is optional: #f is None, list is Some
(define-syntax-class field-spec
  #:attributes (name stxclass default)
  (pattern [name:id stxclass:id]
           #:attr default #'#f)
  (pattern [name:id stxclass:id #:default dfl:expr]
           #:do [(assert-syntax-class-match!
                  this-syntax (attribute stxclass) (attribute dfl))]
           #:attr default #'(dfl)))

(define-syntax-class required-class
  #:attributes (id [onlys 1] [excepts 1] [rename-from 1] [rename-to 1])
  (pattern id:id
           #:attr [onlys 1] null
           #:attr [excepts 1] null
           #:attr [rename-from 1] null
           #:attr [rename-to 1] null)
  (pattern [id:id ~! (~alt (~optional (~seq #:only ~! (onlys:id ...))
                                      #:defaults ([(onlys 1) null]))
                           (~optional (~seq #:except ~! (excepts:id ...))
                                      #:defaults ([(excepts 1) null]))
                           (~optional (~seq #:rename ~! ([rename-from:id rename-to:id] ...))
                                      #:defaults ([(rename-from 1) null]
                                                  [(rename-to 1) null])))
                  ...]))

(struct require-spec (class onlys excepts renames) #:transparent)
