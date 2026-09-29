#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "./private/class.rkt"
                     "./private/error.rkt"
                     "./private/instance.rkt"
                     "./private/star.rkt"
                     "./private/type.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-instance κ:id τ:id
                            (~optional ([key:id val:expr] ...)
                                       #:defaults ([(key 1) null] [(val 1) null])))
  #:do [(push-caller 'define-rope-instance this-syntax #f)

        (define type-desc (with-sub-expr #'τ (describe-type #'τ)))
        (define class-desc (with-sub-expr #'κ (describe-class #'κ)))

        (define reqs (rope-class-reqs class-desc))
        (define imports (resolve-imports #'τ reqs))

        (define instance-bindings
          (for/list ([k (in-list (attribute key))]
                     [v (in-list (attribute val))])
            (cons (syntax-e k) v)))

        (define params (rope-class-params class-desc))
        (define class-bindings
          (build-class-args (syntax-e #'κ) params instance-bindings))

        (define stxclasses
          (for/list ([param (in-list (rope-class-params class-desc))])
            (cons (car param) (class-param-stxclass (cdr param)))))

        (for ([instance-entry (in-list instance-bindings)]
              [k (in-list (attribute key))])
          (define name (car instance-entry))
          (define val-stx (cdr instance-entry))
          (define stxclass
            (cond
              [(assoc name stxclasses) => cdr]
              [else
               (with-sub-expr (combine-source-locations k val-stx)
                 (rope-error "unknown member ~a of ~a" name (syntax-e #'κ)))]))
          (assert-syntax-class-match! this-syntax stxclass val-stx))

        (define class-body (rope-class-body class-desc))
        (define type-bindings (rope-type-bindings type-desc))
        (define all-bindings
          (list* (cons 'κ #'κ)
                 (cons 'τ #'τ)
                 (append class-bindings imports type-bindings)))]

  #:with (~var rope:%:*) (format-id #'τ "rope:~a:~a" (syntax-e #'κ) (syntax-e #'τ))
  #:with ([k . v] ...) (for/list ([entry (in-list class-bindings)])
                         (cons (datum->syntax #f (car entry))
                               (star-expand #'τ (syntax-e #'τ) (cdr entry))))
  #:with body*
  ;; substitute then expand
  (star-expand #'τ (syntax-e #'τ) (star-substitute all-bindings class-body))

  #:do [(pop-caller)]

  (begin
    (define-syntax rope:%:* (rope-instance (list (cons 'k #'v) ...)))
    . body*))
