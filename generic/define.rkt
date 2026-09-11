#lang racket/base

;; rope/generic.rkt

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "../private/type/descriptor.rkt")
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

(define-syntax-parse-rule (define-rope-operation (op-id:id type-id:id args:op-args) template:expr)
  ;; The first argument of the outer macro (type-id) binds a rope type name at
  ;; definition time, so it can be passed on to other generic rope operations
  ;; from inside the template.
  ;;
  ;; The first argument of the inner macro (ρ) is an expansion-time binder
  ;; that determines which rope type descriptor's components should be
  ;; implicitly bound inside the template.
  ;;
  ;; When define-rope-operation is expanded, the following pattern directive
  ;; sets the name of the inner macro's first argument to whatever type-id is
  ;; bound to.
  #:with ρ (format-id this-syntax (symbol->string (syntax-e #'type-id)))

  ;; Identifiers that are implicitly bound inside the template are declared
  ;; here to inherit the scope of the outer macro invocation.
  #:do [(define (mk-op name) (format-id this-syntax name))]

  ;; per-chunk primitives
  #:with chunk?                (mk-op "chunk?")
  #:with chunk-limit           (mk-op "chunk-limit")
  #:with chunk-empty           (mk-op "chunk-empty")
  #:with chunk-length          (mk-op "chunk-length")
  #:with chunk-width           (mk-op "chunk-width")
  #:with chunk-ref             (mk-op "chunk-ref")
  #:with chunk-slice           (mk-op "chunk-slice")
  #:with chunk-append          (mk-op "chunk-append")
  #:with chunk=?               (mk-op "chunk=?")
  #:with chunk-compare         (mk-op "chunk-compare")
  #:with chunk-overlap=?       (mk-op "chunk-overlap=?")
  #:with chunk-compare-overlap (mk-op "chunk-compare-overlap")
  #:with chunk-hash            (mk-op "chunk-hash")

  ;; per-element primitives
  #:with elem-width            (mk-op "elem-width")
  #:with elem-hash             (mk-op "elem-hash")
  #:with elem<?                (mk-op "elem<?")
  #:with elem>?                (mk-op "elem>?")

  ;; per-rope primitives
  #:with leaf-constructor      (mk-op "leaf-constructor")
  #:with node-constructor      (mk-op "node-constructor")
  #:with node-hash             (mk-op "node-hash")
  #:with rope-hashing          (mk-op "rope-hashing")
  #:with content=?             (mk-op "content=?")

  ;; Passing arbitrary user-supplied arguments directly to the inner macro
  ;; definition is not safe because syntax/parse binds _ as the no-bind
  ;; catch-all pattern, so any user-supplied arg named _ will become a
  ;; catch-all pattern for the inner macro. To prevent this, we embed
  ;; temporary identifiers into the inner macro's pattern and then bind them
  ;; back to the original identifiers on the inside.
  #:with inner-ρ (generate-temporary #'type-id)

  (define-syntax-parse-rule (op-id inner-ρ args.inner-pattern ...)
    #:do [(define (raise-op-error msg stx)
            (raise-syntax-error 'op-id msg this-syntax stx))

          (define desc-id (format-id #'inner-ρ "rope:~a" #'inner-ρ))
          (define desc    (syntax-local-value desc-id (λ () #f)))
          (unless desc
            (raise-op-error "expected a rope type descriptor" #'inner-ρ))]

    ;; per-chunk primitives
    #:with chunk?                 (rope-type-descriptor-chunk?                 desc)
    #:with chunk-limit            (rope-type-descriptor-chunk-limit            desc)
    #:with chunk-empty            (rope-type-descriptor-chunk-empty            desc)
    #:with chunk-length           (rope-type-descriptor-chunk-length           desc)
    #:with chunk-width            (rope-type-descriptor-chunk-width            desc)
    #:with chunk-ref              (rope-type-descriptor-chunk-ref              desc)
    #:with chunk-slice            (rope-type-descriptor-chunk-slice            desc)
    #:with chunk-append           (rope-type-descriptor-chunk-append           desc)
    ;; #:with chunk=?                (rope-type-descriptor-chunk=?                desc)
    #:with chunk-compare          (rope-type-descriptor-chunk-compare          desc)
    ;; #:with chunk-overlap=?        (rope-type-descriptor-chunk-overlap=?        desc)
    #:with chunk-compare-overlap  (rope-type-descriptor-chunk-compare-overlap  desc)
    ;; #:with chunk-hash             (rope-type-descriptor-rope-chunk-hash        desc)

    ;; per-element primitives
    #:with elem-width             (rope-type-descriptor-elem-width             desc)
    ;; #:with elem-hash              (rope-type-descriptor-elem-hash              desc)
    #:with elem<?                 (rope-type-descriptor-elem<?                 desc)
    #:with elem>?                 (rope-type-descriptor-elem>?                 desc)

    ;; per-rope primitives
    #:with leaf-constructor       (rope-type-descriptor-leaf-constructor       desc)
    #:with node-constructor       (rope-type-descriptor-node-constructor       desc)
    ;; #:with node-hash              (rope-type-descriptor-rope-node-hash         desc)
    ;; #:with rope-hashing           (rope-type-descriptor-make-rope-hash         desc)
    ;; #:with content=?              (rope-type-descriptor-content=?              desc)

    ;; Rebind the temporary identifiers to the corresponding originals.
    #:with ρ                   #'inner-ρ
    #:with args.rebind-pattern #'args.rebind-value

    template))

