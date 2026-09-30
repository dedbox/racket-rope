#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "../private/class.rkt"
                     "../private/error.rkt"
                     "../private/instance.rkt"
                     "../private/star.rkt"
                     "../private/type.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-type-op (op-id:id τ:id . op-args) . body)
  #:with inner-τ (generate-temporary #'τ)
  (define-syntax-parse-rule (op-id inner-τ . op-args)
    #:do [(push-caller 'op-id this-syntax #'inner-τ)
          (define type-desc (describe-type #'inner-τ))
          (define type-bindings (rope-type-bindings type-desc))]
    #:with τ #'inner-τ
    #:with body* (star-substitute type-bindings #'body)
    #:do [(pop-caller)]

    (begin . body*)))

(define-syntax-parse-rule (define-class-op outer-κ:id (op-id:id τ:id . op-args) . body)
  #:with inner-τ (generate-temporary #'τ)
  (define-syntax-parse-rule (op-id inner-τ . op-args)
    #:do [(push-caller 'op-id this-syntax #f)
          (define type-desc (with-sub-expr #'inner-τ (describe-type #'inner-τ)))
          (define class-desc (with-sub-expr #'outer-κ (describe-class #'outer-κ)))
          (define instance-desc
            (with-sub-expr #'inner-τ (describe-instance #'outer-κ #'inner-τ)))
          (define reqs (rope-class-reqs class-desc))
          (define imports (resolve-imports #'inner-τ reqs))
          (define instance-bindings (rope-instance-bindings instance-desc))
          (define params (rope-class-params class-desc))
          (define class-bindings (build-class-args (syntax-e #'κ) params instance-bindings))
          (define type-bindings (rope-type-bindings type-desc))
          (define all-bindings (list* (cons 'κ #'outer-κ) (cons 'τ #'inner-τ)
                                      (append class-bindings imports type-bindings)))]
    #:with τ #'inner-τ
    #:with body*
    ;; substitute then expand
    (star-expand #'inner-τ (syntax-e #'inner-τ) (star-substitute all-bindings #'body))
    #:do [(pop-caller)]

    (begin . body*)))
