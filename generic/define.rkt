#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "../private/instance.rkt"
                     "../private/type.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(begin-for-syntax
  (define-splicing-syntax-class op-args
    #:description "operation arguments"
    ;; Arguments ending with ...
    (pattern (~seq arg:id ... last-arg:id (~datum ...))
             #:with (inner-arg ...) (generate-temporaries #'(arg ...))
             #:with inner-last      (generate-temporary #'last-arg)
             #:with (inner-pattern ...) #'(inner-arg ... inner-last (... ...))
             ;; The left and right sides of the inner #:with clause
             #:with rebind-pattern  #'(arg ... last-arg (... ...))
             #:with rebind-value    #'(inner-arg ... inner-last (... ...)))
    ;; Fixed arity arguments
    (pattern (~seq arg:id ...)
             #:with (inner-arg ...)     (generate-temporaries #'(arg ...))
             #:with (inner-pattern ...) #'(inner-arg ...)
             #:with rebind-pattern      #'(arg ...)
             #:with rebind-value        #'(inner-arg ...))))

(define-syntax-parse-rule (define-type-op (op-id:id type-id:id args:op-args) template:expr)
  ;; The first argument of the outer macro (type-id) binds a rope type name at
  ;; definition time, so it can be passed on to other generic rope operations
  ;; from inside the template.
  ;;
  ;; The first argument of the inner macro (τ) is an expansion-time binder
  ;; that determines which rope type descriptor's components should be
  ;; implicitly bound inside the template.
  ;;
  ;; When define-rope-operation is expanded, the following pattern directive
  ;; sets the name of the inner macro's first argument to whatever type-id is
  ;; bound to.
  #:with τ (format-id this-syntax (symbol->string (syntax-e #'type-id)))

  ;; Identifiers that are implicitly bound inside the template are declared
  ;; here to inherit the scope of the outer macro invocation.
  #:do [(define (mk-op name) (format-id this-syntax name))]

  ;; chunk operations
  #:with *-chunk?         (mk-op "*-chunk?")
  #:with *-chunk-limit    (mk-op "*-chunk-limit")
  #:with *-chunk-empty    (mk-op "*-chunk-empty")
  #:with *-chunk-length   (mk-op "*-chunk-length")
  #:with *-chunk-width    (mk-op "*-chunk-width")
  #:with *-chunk-ref      (mk-op "*-chunk-ref")
  #:with *-chunk-slice    (mk-op "*-chunk-slice")
  #:with *-chunk-append   (mk-op "*-chunk-append")

  ;; element operations
  #:with *-elem-width     (mk-op "*-elem-width")

  ;; smart constructors
  #:with *-make-leaf      (mk-op "*-make-leaf")
  #:with *-make-node      (mk-op "*-make-node")

  ;; internal hashing / equality
  #:with *-chunk-hash     (mk-op "*-chunk-hash")
  #:with *-node-hash      (mk-op "*-node-hash")
  #:with *-rope-content=? (mk-op "*-rope-content=?")

  ;; Passing arbitrary user-supplied arguments directly to the inner macro
  ;; definition is not safe because syntax/parse binds _ as the no-bind
  ;; catch-all pattern, so any user-supplied arg named _ will become a
  ;; catch-all pattern for the inner macro. To prevent this, we embed
  ;; temporary identifiers into the inner macro's pattern and then bind them
  ;; back to the original identifiers on the inside.
  #:with inner-τ (generate-temporary #'type-id)

  (define-syntax-parse-rule (op-id inner-τ args.inner-pattern ...)
    #:do [(define type-desc-id (format-id #'inner-τ "rope:~a" #'inner-τ))
          (define type-desc
            (or (syntax-local-value type-desc-id (λ () #f))
                (raise-syntax-error 'op-id "expected a rope type descriptor"
                                    this-syntax #'inner-τ)))
          (define Eq-desc-id (format-id #'inner-τ "rope:~a:Eq" #'inner-τ))
          (define Eq-desc
            (or (syntax-local-value Eq-desc-id (λ () #f))
                (raise-syntax-error 'op-id "expected an Eq instance")))
          (define (lookup-Eq-member x)
            (cdr (assoc x (rope-instance-descriptor-members Eq-desc))))]

    ;; chunk operations
    #:with *-chunk?          (rope-type-descriptor-chunk?         type-desc)
    #:with *-chunk-limit     (rope-type-descriptor-chunk-limit    type-desc)
    #:with *-chunk-empty     (rope-type-descriptor-chunk-empty    type-desc)
    #:with *-chunk-length    (rope-type-descriptor-chunk-length   type-desc)
    #:with *-chunk-width     (rope-type-descriptor-chunk-width    type-desc)
    #:with *-chunk-ref       (rope-type-descriptor-chunk-ref      type-desc)
    #:with *-chunk-slice     (rope-type-descriptor-chunk-slice    type-desc)
    #:with *-chunk-append    (rope-type-descriptor-chunk-append   type-desc)

    ;; element operations
    #:with *-elem-width      (rope-type-descriptor-elem-width     type-desc)

    ;; smart constructors
    #:with *-make-leaf       (rope-type-descriptor-make-leaf      type-desc)
    #:with *-make-node       (rope-type-descriptor-make-node      type-desc)

    ;; hashing / equality
    #:with *-chunk=?         (lookup-Eq-member 'chunk=?)
    #:with *-chunk-overlap=? (lookup-Eq-member 'chunk-overlap=?)
    #:with *-elem=?          (lookup-Eq-member 'elem=?)
    #:with *-elem-hash       (lookup-Eq-member 'elem-hash)
    #:with *-chunk-hash      (lookup-Eq-member 'chunk-hash)
    #:with *-node-hash       (lookup-Eq-member 'node-hash)
    #:with *-rope-content=?  (lookup-Eq-member 'rope-content=?)

    ;; Rebind the temporary identifiers to the corresponding originals.
    #:with τ                   #'inner-τ
    #:with args.rebind-pattern #'args.rebind-value

    template))
