#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     rope2/descriptors
                     rope2/stxclasses
                     syntax/parse)
         syntax/parse/define)

(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class field-spec
    #:attributes (name stxclass default has-default? optional?)
    (pattern (name:id stxclass:id
                      (~optional (~and #:optional opt-kw))
                      (~optional (~seq #:default default-expr:expr)))
             #:attr has-default? (if (attribute default-expr) #t #f)
             #:attr default      (or (attribute default-expr) #'#f)
             #:attr optional?    (if (attribute opt-kw) #'#t #'#f))))

;; -----------------------------------------------------------------------------
;; Class
;; -----------------------------------------------------------------------------

(define-syntax-parse-rule (define-rope-class class-id:id (field:field-spec ...) body ...)
  #:with (~var rope:%) (format-id #'class-id "rope:~a" (syntax-e #'class-id))
  (define-syntax rope:%
    (rope-class-descriptor (list (list #'field.name #'field.stxclass field.optional?) ...)
                           #'(body ...))))

;; -----------------------------------------------------------------------------
;; Instance
;; -----------------------------------------------------------------------------

(begin-for-syntax
  (define type-id-placeholder-rx #px"\\*(?=-(rope|chunk|elem)\\b)")

  (define (subst-placeholders stx type-id)
    (define type-str (symbol->string (syntax-e type-id)))

    (let loop ([stx stx])
      (cond
        [(identifier? stx)
         (define old-name (symbol->string (syntax-e stx)))
         (define new-name (regexp-replace* type-id-placeholder-rx old-name type-str))
         (if (string=? new-name old-name)
             stx
             (datum->syntax stx (string->symbol new-name) stx stx))]
        [(syntax? stx) (datum->syntax stx (loop (syntax-e stx)) stx stx)]
        [(pair?   stx) (cons (loop (car stx)) (loop (cdr stx)))]
        [(vector? stx) (list->vector (map loop (vector->list stx)))]
        [else stx]))))

(define-syntax-parse-rule (define-rope-class-instance type-id:id class-id:id
                            (~seq kw:keyword kw-val:expr) ...)
  #:do [(define type-desc-id (format-id #'type-id "rope:~a" (syntax-e #'type-id)))
        (define type-desc
          (or (syntax-local-value type-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected a rope type descriptor"
                                  this-syntax #'type-id)))

        (define equality-desc-id (format-id #'type-id "~a:equality" type-desc-id))
        (define equality-desc
          (or (syntax-local-value equality-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected an equality instance"
                                  this-syntax #'type-id)))

        (define class-desc-id (format-id #'class-id "rope:~a" (syntax-e #'class-id)))
        (define class-desc
          (or (syntax-local-value class-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected a rope class descriptor"
                                  this-syntax #'class-id)))

        (define (keyword-syntax->symbol kw-stx)
          (string->symbol (keyword->string (syntax-e kw-stx))))

        (define (symbol->keyword-syntax ctx x)
          (datum->syntax ctx (string->keyword (symbol->string x)) ctx ctx))

        ;; instance-supplied primitive bindings
        (define instance-members
          (map cons (map keyword-syntax->symbol (attribute kw)) (attribute kw-val)))

        (for ([prim (in-list (rope-class-descriptor-primitives class-desc))])
          (define name      (car prim))
          (define required? (not (caddr prim)))
          (when (and required? (not (assoc (syntax-e name) instance-members)))
            (raise-syntax-error 'define-rope-class-instance
                                (format "missing required member ~a for class ~a"
                                        (syntax-e name) (syntax-e #'class-id))
                                this-syntax)))

        (define formals
          (for/list ([prim (in-list (rope-class-descriptor-primitives class-desc))])
            (define name (car prim))
            (define stxclass (cadr prim))
            (define optional? (caddr prim))
            (define kw (symbol->keyword-syntax name (syntax-e name)))
            (quasisyntax/loc name
              (#,(if optional? #'~optional #'~once) (~seq #,kw (~var #,name #,stxclass))))))

        (define supplied
          (for/list ([prim (in-list (rope-class-descriptor-primitives class-desc))]
                     #:when (assoc (syntax-e (car prim)) instance-members))
            prim))

        (define actuals
          (for/list ([prim (in-list supplied)])
            (define name (car prim))
            (define kw (symbol->keyword-syntax name (syntax-e name)))
            (define val (cdr (assoc (syntax-e name) instance-members)))
            (cons kw val)))]

  #:with (formal ...) formals
  #:with ((actual-kw . actual-val) ...) actuals
  #:with (body* ...) (subst-placeholders (rope-class-descriptor-body class-desc) #'type-id)
  #:with inst-id (generate-temporary 'instantiate)

  (begin
    (define-syntax-parse-rule (inst-id (~alt formal ...) (... ...)) (begin body* ...))
    (inst-id (~@ actual-kw actual-val) ...)))
