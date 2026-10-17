#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     "../private/scope.rkt")
         rope/signature
         syntax/parse/define
         "../private/type-scope.rkt")

(provide (all-defined-out))

(define-syntax-parse-rule (define-type-op (op-id:id τ:id . op-args) body ...+)
  #:do [(define forms (attribute body))
        (define sig (syntax-local-value #'rope-signature))
        (check-member-refs 'define-type-op sig #'τ (collect-member-refs #'τ forms) this-syntax)
        (define body-refs (remove-duplicates (map car (collect-member-refs #'τ (attribute body)))))]
  #:with inner-τ (generate-temporary #'τ)
  #:with ([ref key] ...) (for/list ([k (in-list body-refs)])
                           (list (member-ref-id #'τ k) (datum->syntax #f k)))
  (define-syntax-parse-rule (op-id inner-τ . op-args)
    (with-type-members (τ inner-τ ([ref key] ...)) body ...)))

;; (define-syntax-parse-rule (define-class-op outer-κ:id (op-id:id τ:id . op-args)
;;                             . body)
;;   #:with inner-τ (generate-temporary #'τ)

;;   (define-syntax-parse-rule (op-id inner-τ . op-args)
;;     #:with τ #'inner-τ
;;     #:with body*
;;     (with-caller ('op-id this-syntax #f)
;;       (define type-desc (with-caller-sub #'inner-τ (describe-type #'inner-τ)))
;;       (define type-bindings (rope-type-bindings type-desc))
;;       (define class-desc (with-caller-sub #'outer-κ (describe-class #'outer-κ)))
;;       (define params (rope-class-params class-desc))
;;       (define reqs (rope-class-reqs class-desc))
;;       (define imports (resolve-imports #'inner-τ reqs))
;;       (define instance-desc (with-caller-sub #'inner-τ
;;                               (describe-instance #'outer-κ #'inner-τ)))
;;       (define instance-bindings (rope-instance-bindings instance-desc))
;;       (define class-bindings
;;         (build-class-args (syntax-e #'κ) params instance-bindings))
;;       (define all-bindings
;;         (list* (cons 'κ #'outer-κ) (cons 'τ #'inner-τ)
;;                (append class-bindings imports type-bindings)))
;;       (define body-subst (star-substitute all-bindings #'body))
;;       (star-expand #'inner-τ (syntax-e #'inner-τ) body-subst))

;;     (begin . body*)))
