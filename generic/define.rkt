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
    #:with τ #'inner-τ
    #:with body* (with-caller ('op-id this-syntax #'inner-τ)
                   (define type-desc (describe-type #'inner-τ))
                   (define type-bindings (rope-type-bindings type-desc))
                   (star-substitute type-bindings #'body)) 

    (begin . body*)))

(define-syntax-parse-rule (define-class-op outer-κ:id (op-id:id τ:id . op-args)
                            . body)
  #:with inner-τ (generate-temporary #'τ)

  (define-syntax-parse-rule (op-id inner-τ . op-args)
    #:with τ #'inner-τ
    #:with body*
    (with-caller ('op-id this-syntax #f)
      (define type-desc (with-caller-sub #'inner-τ (describe-type #'inner-τ)))
      (define type-bindings (rope-type-bindings type-desc))
      (define class-desc (with-caller-sub #'outer-κ (describe-class #'outer-κ)))
      (define params (rope-class-params class-desc))
      (define reqs (rope-class-reqs class-desc))
      (define imports (resolve-imports #'inner-τ reqs))
      (define instance-desc (with-caller-sub #'inner-τ
                              (describe-instance #'outer-κ #'inner-τ)))
      (define instance-bindings (rope-instance-bindings instance-desc))
      (define class-bindings
        (build-class-args (syntax-e #'κ) params instance-bindings))
      (define all-bindings
        (list* (cons 'κ #'outer-κ) (cons 'τ #'inner-τ)
               (append class-bindings imports type-bindings)))
      (define body-subst (star-substitute all-bindings #'body))
      (star-expand #'inner-τ (syntax-e #'inner-τ) body-subst))

    (begin . body*)))
