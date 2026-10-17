#lang racket/base

;; (require (for-syntax racket/base
;;                      racket/syntax
;;                      syntax/parse
;;                      "./private/class.rkt"
;;                      "./private/error.rkt"
;;                      "./private/instance.rkt"
;;                      "./private/type.rkt")
;;          syntax/parse/define)

;; (provide (all-defined-out))

;; (define-syntax-parse-rule (define-rope-instance κ:id τ:id args:instance-args)
;;   #:do [(define κ-name (syntax-e #'κ)) (define τ-name (syntax-e #'τ))]
;;   #:with (~var rope:%:*) (format-id #'τ "rope:~a:~a" κ-name τ-name)
;;   #:with (([k . v] ...) body*)
;;   (with-caller ('define-rope-instance this-syntax #f)
;;     (define type-desc (with-caller-sub #'τ (describe-type #'τ)))
;;     (define class-desc (with-caller-sub #'κ (describe-class #'κ)))

;;     (define params (rope-class-params class-desc))
;;     (define reqs (rope-class-reqs class-desc))
;;     (define class-body (rope-class-body class-desc))

;;     (define imports (resolve-imports #'τ reqs))

;;     (define instance-bindings (for/list ([k (in-list (attribute args.key))]
;;                                          [v (in-list (attribute args.val))])
;;                                 (cons (syntax-e k) v)))

;;     (define class-bindings
;;       (build-class-args κ-name params instance-bindings))

;;     (define stxclasses
;;       (for/list ([param (in-list params)])
;;         (cons (car param) (class-param-stxclass (cdr param)))))

;;     (for ([instance-entry (in-list instance-bindings)]
;;           [k (in-list (attribute args.key))])
;;       (define name (car instance-entry))
;;       (define val-stx (cdr instance-entry))
;;       (define stxclass
;;         (cond
;;           [(assoc name stxclasses) => cdr]
;;           [else (with-caller-sub k
;;                   (rope-error "unknown member ~a of ~a" name κ-name))]))
;;       (assert-syntax-class-match! this-syntax stxclass val-stx))

;;     (define type-bindings (rope-type-bindings type-desc))
;;     (define all-bindings
;;       (list* (cons 'κ #'κ)
;;              (cons 'τ #'τ)
;;              (append class-bindings imports type-bindings)))
;;     (define k-v-pairs
;;       (for/list ([entry (in-list class-bindings)])
;;         (cons (datum->syntax #f (car entry))
;;               (star-expand #'τ τ-name (cdr entry)))))
;;     (let* ([body* (star-substitute all-bindings class-body)]
;;            [body* (star-expand #'τ τ-name body*)])
;;       (list k-v-pairs body*)))

;;   (begin
;;     (define-syntax rope:%:* (rope-instance (list (cons 'k #'v) ...)))
;;     . body*))
