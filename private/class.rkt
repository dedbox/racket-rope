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
          [else
           ;; This branch should never fire on user input. We only ever reach
           ;; here if the internal representation of a parameter's
           ;; default-value declaration is mal-formed, i.e., it is not #f or a
           ;; singleton list.
           (rope-error "internal error: ~a / ~a" name κ-name)])))

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
  (pattern [name:id stxclass:id #:optional]
           #:attr default #'(#f))
  (pattern [name:id stxclass:id #:default dfl:expr]
           #:do [(assert-syntax-class-match!
                  this-syntax (attribute stxclass) (attribute dfl))]
           #:attr default #'(dfl)))

;; A dependency mask entry may contain only one of:
;;
;; - An inclusion mask: only these params are imported.
;; - An exclusion mask: all params excep these are imported.
;;
;; It may also contain:
;;
;; - A renaming mask: every param must be in scope and is bound to a new name
;;   when imported.
;;
;; It is not possible to rename included or excluded params.

(struct mask-spec (class has-include? includes excludes renames) #:transparent)

(define-syntax-class depmask
  #:attributes (class has-include? [include 1] [exclude 1] [from 1] [to 1])
  (pattern class:id
           #:attr has-include? #'#f
           #:attr [include 1] null
           #:attr [exclude 1] null
           #:attr [from 1] null
           #:attr [to 1] null)
  (pattern [class:id #:rename ~! ([from:id to:id] ...)]
           #:attr has-include? #'#f
           #:attr [include 1] null
           #:attr [exclude 1] null)
  (pattern [class:id #:include ~! (include:id ...)
                     (~optional (~seq #:rename ~! ([from:id to:id] ...))
                                #:defaults ([(from 1) null] [(to 1) null]))]
           #:attr has-include? #'#t
           #:attr [exclude 1] null)
  (pattern [class:id #:exclude ~! (exclude:id ...)
                     (~optional (~seq #:rename ~! ([from:id to:id] ...))
                                #:defaults ([(from 1) null] [(to 1) null]))]
           #:attr has-include? #'#f
           #:attr [include 1] null))
