#lang racket/base

;; rope/generic-ops.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor
                     syntax/parse)
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-operation (op-id:id type-id:id arg:id ...) template:expr)
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
  #:with chunk?           (mk-op "chunk?")
  #:with chunk-limit      (mk-op "chunk-limit")
  #:with chunk-length     (mk-op "chunk-length")
  #:with chunk-width      (mk-op "chunk-width")
  #:with chunk-ref        (mk-op "chunk-ref")
  #:with chunk-slice      (mk-op "chunk-slice")
  #:with chunk-append     (mk-op "chunk-append")
  #:with chunk-compare    (mk-op "chunk-compare")
  #:with chunk-overlap=?  (mk-op "chunk-overlap=?")

  ;; per-element primitives
  #:with elem-width       (mk-op "elem-width")
  #:with elem-hash        (mk-op "elem-hash")

  ;; per-rope primitives
  #:with leaf-constructor (mk-op "leaf-constructor")
  #:with node-constructor (mk-op "node-constructor")
  #:with rope-chunk-hash  (mk-op "rope-chunk-hash")
  #:with rope-node-hash   (mk-op "rope-node-hash")
  #:with rope-hashing     (mk-op "rope-hashing")

  ;; content-based hashing * equality
  #:with make-rope-hash   (mk-op "make-rope-hash")
  #:with content=?        (mk-op "content=?")

  ;; smart constructors
  #:with make-rope-leaf   (mk-op "make-rope-leaf")
  #:with make-rope-node   (mk-op "make-rope-node")

  ;; conversions
  #:with chunk->rope      (mk-op "chunk->rope")
  #:with rope->chunk      (mk-op "rope->chunk")

  ;; basic operations
  #:with rope-concat      (mk-op "rope-concat")

  ;; Passing arbitrary user-supplied arguments directly to the inner macro
  ;; definition is not safe because syntax/parse binds _ as the no-bind
  ;; catch-all pattern, so any user-supplied arg named _ will become a
  ;; catch-all pattern for the inner macro. To prevent this, we embed
  ;; temporary identifiers into the inner macro's pattern and then bind them
  ;; back to the original identifiers on the inside.
  #:with inner-ρ         (generate-temporary #'type-id)
  #:with (inner-arg ...) (generate-temporaries #'(arg ...))

  (define-syntax-parse-rule (op-id inner-ρ inner-arg ...)
    #:do [(define (raise-op-error msg stx)
            (raise-syntax-error 'op-id msg this-syntax stx))

          (define desc-id (format-id #'inner-ρ "rope:~a" #'inner-ρ))
          (define desc    (syntax-local-value desc-id (λ () #f)))
          (unless desc
            (raise-op-error "expected a rope type descriptor" #'inner-ρ))]

    ;; per-chunk primitives
    #:with chunk?           (rope-type-descriptor-chunk?           desc)
    #:with chunk-limit      (rope-type-descriptor-chunk-limit      desc)
    #:with chunk-empty      (rope-type-descriptor-chunk-empty      desc)
    #:with chunk-length     (rope-type-descriptor-chunk-length     desc)
    #:with chunk-width      (rope-type-descriptor-chunk-width      desc)
    #:with chunk-ref        (rope-type-descriptor-chunk-ref        desc)
    #:with chunk-slice      (rope-type-descriptor-chunk-slice      desc)
    #:with chunk-append     (rope-type-descriptor-chunk-append     desc)
    #:with chunk-compare    (rope-type-descriptor-chunk-compare    desc)
    #:with chunk-overlap=?  (rope-type-descriptor-chunk-overlap=?  desc)

    ;; per-element primitives
    #:with elem-width       (rope-type-descriptor-elem-width       desc)
    #:with elem-hash        (rope-type-descriptor-elem-hash        desc)

    ;; per-rope primitives
    #:with leaf-constructor (rope-type-descriptor-leaf-constructor desc)
    #:with node-constructor (rope-type-descriptor-node-constructor desc)
    #:with rope-chunk-hash  (rope-type-descriptor-rope-chunk-hash  desc)
    #:with rope-node-hash   (rope-type-descriptor-rope-node-hash   desc)

    ;; content-based hashing & equality
    #:with rope-hashing     (rope-type-descriptor-make-rope-hash   desc)
    #:with rope-content=?   (rope-type-descriptor-content=?        desc)

    ;; Rebind the temporary identifiers to the corresponding originals.
    #:with ρ         #'inner-ρ
    #:with (arg ...) #'(inner-arg ...)

    template))

;; per-chunk primitives
(define-rope-operation (rope-chunk?          _ x)     (chunk?          x))
(define-rope-operation (rope-chunk-limit     _)       (chunk-limit))
(define-rope-operation (rope-chunk-empty     _)       (chunk-empty))
(define-rope-operation (rope-chunk-length    _ c)     (chunk-length    c))
(define-rope-operation (rope-chunk-width     _ c)     (chunk-width     c))
(define-rope-operation (rope-chunk-ref       _ c i)   (chunk-ref       c i))
(define-rope-operation (rope-chunk-slice     _ c i k) (chunk-slice     c i k))
(define-rope-operation (rope-chunk-append    _ cs)    (chunk-append    cs))
(define-rope-operation (rope-chunk-compare   _ a b)   (chunk-compare   a b))
(define-rope-operation (rope-chunk-overlap=? _ a b)   (chunk-overlap=? a b))

;; per-element primitives
(define-rope-operation (rope-elem-width      _ c i)   (elem-width      c i))
(define-rope-operation (rope-elem-hash       _ e)     (elem-hash       e))

;; content-based hashing * equality
(define-rope-operation (make-rope-hash       _ a)     (rope-hashing    a))
(define-rope-operation (rope-content=?       _ a b)   (content=?       a b))

;; smart constructors
(define-rope-operation (make-rope-leaf _ c)
  (let-values ([(hash1 hash2) (rope-chunk-hash c)])
    (leaf-constructor (chunk-length c) (chunk-width c) hash1 hash2 c)))

(define-rope-operation (make-rope-node _ l r)
  (let-values ([(hash1 hash2) (rope-node-hash l r)])
    (node-constructor (+ (rope-length l) (rope-length r))
                      (+ (rope-width l) (rope-width r))
                      hash1 hash2
                      (add1 (max (rope-depth l) (rope-depth r)))
                      l r)))

;; conversions
(define-rope-operation (chunk->rope ρ c0)
  (let loop ([c c0])
    (let ([n (chunk-length c)])
      (if (<= n (chunk-limit))
          (make-rope-leaf ρ c)
          (let* ([mid (quotient n 2)]
                 [l (loop (chunk-slice c 0 mid))]
                 [r (loop (chunk-slice c mid (- n mid)))])
            (rope-concat ρ l r))))))

(define-rope-operation (rope->chunk _ a)
  (chunk-append (rope-flatten a)))

;; basic operations
(define-rope-operation (rope-concat ρ l r)
  (make-rope-node ρ l r))
