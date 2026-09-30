#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "./private/class.rkt"
                     "./private/error.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-class κ:id (field:field-spec ...)
                            (~optional (~seq #:require ~! (req:required-class ...)))
                            . body)
  #:do [(push-caller 'define-rope-class this-syntax #f)

        (for ([req-κ (in-list (or (attribute req.id) null))]
              [except-ids (in-list (or (attribute req.excepts) null))]
              [rename-ids (in-list (or (attribute req.rename-from) null))])
          (set-caller-sub-expr! req-κ)
          (define class-desc (describe-class req-κ))
          (define params (rope-class-params class-desc))
          (for ([param-id (in-list (append except-ids rename-ids))])
            (or (assoc (syntax-e param-id) params)
                (with-sub-expr param-id
                  (rope-error "expected a parameter of ~a" (syntax-e req-κ))))))]

  #:with (~var rope:%) (format-id #'κ "rope:~a" (syntax-e #'κ))

  #:do [(pop-caller)]

  (define-syntax rope:%
    (rope-class
     (reverse
      (list (cons 'field.name (class-param #'field.stxclass #'field.default)) ...))
     (~? (reverse
          (list (require-spec #'req.id
                              (list #'req.excepts ...)
                              (list (cons #'req.rename-from 'req.rename-to) ...))
                ...))
         null)
     (quote-syntax body))))






;; (require (for-syntax racket/base
;;                      racket/syntax
;;                      rope2/descriptors
;;                      rope2/stxclasses
;;                      syntax/parse)
;;          syntax/parse/define)

;; (provide (all-defined-out))

;; (begin-for-syntax
;;   (define-syntax-class field-spec
;;     #:attributes (name stxclass default has-default? optional?)
;;     (pattern (name:id stxclass:id
;;                       (~optional (~and #:optional opt-kw))
;;                       (~optional (~seq #:default default-expr:expr)))
;;              #:attr has-default? (if (attribute default-expr) #t #f)
;;              #:attr default      (or (attribute default-expr) #'#f)
;;              #:attr optional?    (if (attribute opt-kw) #'#t #'#f))))

;; ;; -----------------------------------------------------------------------------
;; ;; Class
;; ;; -----------------------------------------------------------------------------

;; ;; NOTE: body must be embedded in quote-syntax to avoid premature resolution
;; ;; of ~? and ~@.
;; (define-syntax-parse-rule (define-rope-class class-id:id (field:field-spec ...) body ...)
;;   #:with (~var rope:%) (format-id #'class-id "rope:~a" (syntax-e #'class-id))
;;   #:with body* (datum->syntax #f (attribute body))
;;   (define-syntax rope:%
;;     (rope-class-descriptor
;;      (list (list #'field.name #'field.stxclass field.optional?) ...)
;;      (quote-syntax body*))))

;; ;; -----------------------------------------------------------------------------
;; ;; Instance
;; ;; -----------------------------------------------------------------------------

;; (begin-for-syntax
;;   (define type-id-placeholder-rx #px"\\*(?=-(rope|chunk|elem)\\b)")

;;   ;; *-chunk-ref etc. name the type's OWN operations, which already exist
;;   ;; as real, correctly-bound identifiers inside type-desc (put there by
;;   ;; define-rope-type's own, unrelated placeholder-renaming pass). Renaming
;;   ;; "*-chunk-ref" to a FRESH identifier textually spelled "string-chunk-ref"
;;   ;; (the old approach, via regexp-replace* + datum->syntax) produces a
;;   ;; same-name identifier that is NOT bound-identifier=? to the real one --
;;   ;; two independent macro layers minting "the same" name from scratch
;;   ;; don't produce the same binding. The fix: for names type-desc actually
;;   ;; knows about, splice in type-desc's own stored identifier directly,
;;   ;; rather than re-deriving a same-spelled one. Only truly class-internal
;;   ;; names (*-rope-compare-with and friends, defined AND used entirely
;;   ;; within this same body) fall back to the string-rename, which is safe
;;   ;; there specifically because both the binding and every use are renamed
;;   ;; together, uniformly, in this one pass.
;;   (define (type-op-table type-desc)
;;     (list (cons '*-chunk?       (rope-type-descriptor-chunk? type-desc))
;;           (cons '*-chunk-limit  (rope-type-descriptor-chunk-limit type-desc))
;;           (cons '*-chunk-empty  (rope-type-descriptor-chunk-empty type-desc))
;;           (cons '*-chunk-length (rope-type-descriptor-chunk-length type-desc))
;;           (cons '*-chunk-width  (rope-type-descriptor-chunk-width type-desc))
;;           (cons '*-chunk-ref    (rope-type-descriptor-chunk-ref type-desc))
;;           (cons '*-chunk-slice  (rope-type-descriptor-chunk-slice type-desc))
;;           (cons '*-chunk-append (rope-type-descriptor-chunk-append type-desc))
;;           (cons '*-elem-width   (rope-type-descriptor-elem-width type-desc))
;;           (cons '*-rope-leaf    (rope-type-descriptor-make-leaf type-desc))
;;           (cons '*-rope-node    (rope-type-descriptor-make-node type-desc))))

;;   ;; (~? name default): our own hand-rolled version, not syntax-parse's.
;;   ;; syntax-parse's ~? only resolves against attributes of the enclosing
;;   ;; pattern *at the point its own template is compiled* -- which, for
;;   ;; reasons covered above, is never a layer that has the right
;;   ;; attributes in scope for this. Since subst-placeholders already does
;;   ;; its own plain-data walk, handling (~? name default) here directly --
;;   ;; matched by spelling, not by any binding -- sidesteps the cross-layer
;;   ;; problem entirely: name-supplied? is answered by member-table, a
;;   ;; plain assoc list, not by any macro's attribute-tracking.
;;   (define (equality-op-table equality-desc)
;;     (for/list ([entry (in-list (rope-instance-descriptor-members equality-desc))])
;;       (cons (string->symbol (format "*-~a" (car entry))) (cdr entry))))

;;   ;; Bare `type-id` / `class-id` tokens in a class body are a second,
;;   ;; whole-word placeholder convention, distinct from the *-prefixed one:
;;   ;; they're how the body passes the concrete type/class name as a literal
;;   ;; token to a define-class-op-defined operation (e.g. `(rope-compare-with
;;   ;; type-id class-id f a b)`), since those operations are macros that need
;;   ;; the name as a syntactic token, not a runtime value. Whole-word match
;;   ;; only -- must not fire on `type-id` as a substring of some other name.
;;   (define (subst-placeholders stx type-id [type-desc #f] [equality-desc #f]
;;                               [class-id #f] [member-table '()])
;;     (define type-str (symbol->string (syntax-e type-id)))
;;     (define op-table
;;       (append member-table
;;               (if type-desc (type-op-table type-desc) '())
;;               (if equality-desc (equality-op-table equality-desc) '())))

;;     (let loop ([stx stx])
;;       (cond
;;         [(identifier? stx)
;;          (cond
;;            [(assoc (syntax-e stx) op-table)
;;             => cdr]
;;            [(eq? (syntax-e stx) 'type-id) type-id]
;;            [(and class-id (eq? (syntax-e stx) 'class-id)) class-id]
;;            [else
;;             (define old-name (symbol->string (syntax-e stx)))
;;             (define new-name (regexp-replace* type-id-placeholder-rx old-name type-str))
;;             (if (string=? new-name old-name)
;;                 stx
;;                 ;; format-id (unlike a raw datum->syntax) applies the
;;                 ;; syntax-local-introduce adjustment appropriate for a
;;                 ;; name being minted from within a macro transformer, so
;;                 ;; it lands correctly as a reachable module-level binding
;;                 ;; here, whereas plain datum->syntax did not.
;;                 (format-id type-id "~a" new-name #:source stx))])]
;;         [(syntax? stx)
;;          (define lst (syntax-e stx))
;;          (cond
;;            ;; (~? name default), matched structurally by spelling.
;;            [(and (pair? lst) (identifier? (car lst)) (eq? (syntax-e (car lst)) '~?)
;;                  (pair? (cdr lst)) (identifier? (cadr lst))
;;                  (pair? (cddr lst)) (null? (cdddr lst)))
;;             (define name-sym (syntax-e (cadr lst)))
;;             (define default-stx (caddr lst))
;;             (cond [(assoc name-sym op-table) => cdr]
;;                   [else (loop default-stx)])]
;;            [else (datum->syntax stx (loop lst) stx stx)])]
;;         [(pair?   stx) (cons (loop (car stx)) (loop (cdr stx)))]
;;         [(vector? stx) (list->vector (map loop (vector->list stx)))]
;;         [else stx]))))

;; (define-syntax-parse-rule (define-rope-class-instance type-id:id class-id:id
;;                             (~seq kw:keyword kw-val:expr) ...)
;;   #:do [(define type-desc-id (format-id #'type-id "rope:~a" (syntax-e #'type-id)))
;;         (define type-desc
;;           (or (syntax-local-value type-desc-id (λ () #f))
;;               (raise-syntax-error 'define-rope-class-instance "expected a rope type descriptor"
;;                                   this-syntax #'type-id)))

;;         (define equality-desc-id (format-id #'type-id "~a:equality" type-desc-id))
;;         (define equality-desc
;;           (or (syntax-local-value equality-desc-id (λ () #f))
;;               (raise-syntax-error 'define-rope-class-instance "expected an equality instance"
;;                                   this-syntax #'type-id)))

;;         (define class-desc-id (format-id #'class-id "rope:~a" (syntax-e #'class-id)))
;;         (define class-desc
;;           (or (syntax-local-value class-desc-id (λ () #f))
;;               (raise-syntax-error 'define-rope-class-instance "expected a rope class descriptor"
;;                                   this-syntax #'class-id)))

;;         (define (keyword-syntax->symbol kw-stx)
;;           (string->symbol (keyword->string (syntax-e kw-stx))))

;;         (define (symbol->keyword-syntax ctx x)
;;           (datum->syntax ctx (string->keyword (symbol->string x)) ctx ctx))

;;         ;; instance-supplied primitive bindings
;;         (define instance-members
;;           (map cons (map keyword-syntax->symbol (attribute kw)) (attribute kw-val)))

;;         (for ([prim (in-list (rope-class-descriptor-primitives class-desc))])
;;           (define name      (car prim))
;;           (define required? (not (caddr prim)))
;;           (when (and required? (not (assoc (syntax-e name) instance-members)))
;;             (raise-syntax-error 'define-rope-class-instance
;;                                 (format "missing required member ~a for class ~a"
;;                                         (syntax-e name) (syntax-e #'class-id))
;;                                 this-syntax)))

;;         ;; Validation-only formals: each supplied value gets checked
;;         ;; against its declared syntax class via ~var, exactly as before.
;;         ;; This keeps the class system open to any syntax class (builtin,
;;         ;; project-defined, or a downstream user's own) without a fixed
;;         ;; dispatch table. What changed is that this macro's *output* is
;;         ;; no longer the class's real definitions -- see below.
;;         (define formals
;;           (for/list ([prim (in-list (rope-class-descriptor-primitives class-desc))])
;;             (define name (car prim))
;;             (define stxclass (cadr prim))
;;             (define optional? (caddr prim))
;;             (define kw (symbol->keyword-syntax name (syntax-e name)))
;;             (quasisyntax/loc name
;;               (#,(if optional? #'~optional #'~once) (~seq #,kw (~var #,name #,stxclass))))))

;;         (define actuals
;;           (for/list ([prim (in-list (rope-class-descriptor-primitives class-desc))]
;;                      #:when (assoc (syntax-e (car prim)) instance-members))
;;             (define name (car prim))
;;             (define kw (symbol->keyword-syntax name (syntax-e name)))
;;             (define val (cdr (assoc (syntax-e name) instance-members)))
;;             (cons kw val)))

;;         ;; member-table: instance-members, keyed the same way type-op-table
;;         ;; and equality-op-table are, so subst-placeholders substitutes
;;         ;; elem<?/elem>?/chunk-compare-overlap through the same single
;;         ;; mechanism as everything else -- no separate binding pass, and
;;         ;; no macro layer of its own for these definitions to get lost in.
;;         (define member-table instance-members)]

;;   #:with (formal ...) formals
;;   #:with ((actual-kw . actual-val) ...) actuals
;;   #:with (body* ...) (subst-placeholders (rope-class-descriptor-body class-desc) #'type-id
;;                                          type-desc equality-desc #'class-id member-table)
;;   #:with validate-id (generate-temporary 'validate)

;;   ;; validate-id exists ONLY to check the supplied values against their
;;   ;; declared syntax classes and raise a clear error if something's
;;   ;; wrong or missing; its own expansion result (void) is thrown away.
;;   ;; The real definitions are produced directly below, as THIS macro's
;;   ;; own template -- define-rope-class-instance is what the user calls
;;   ;; directly (e.g. in type/string.rkt), so names minted here via
;;   ;; subst-placeholders/format-id, from type-id (the user's own token),
;;   ;; land as genuinely reachable module-level bindings. Routing them
;;   ;; through a second, nested "define a helper macro and immediately
;;   ;; call it" layer -- as this used to do -- does not: that layer's own,
;;   ;; separate macro-introduction scope makes its output unreachable to
;;   ;; any identifier written outside of it, even at plain module top
;;   ;; level and even for names that were already correctly scoped going
;;   ;; in. This is a general property of Racket hygiene, not specific to
;;   ;; anything renamed here -- confirmed with a two-line repro having
;;   ;; nothing to do with this codebase before changing this.
;;   (begin
;;     (define-syntax-parse-rule (validate-id (~alt formal ...) (... ...)) (void))
;;     (validate-id (~@ actual-kw actual-val) ...)
;;     body* ...))
