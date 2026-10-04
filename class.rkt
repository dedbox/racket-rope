#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     racket/set
                     racket/syntax
                     syntax/parse
                     "./private/class.rkt"
                     "./private/error.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-class κ:id (field:field-spec ...)
                            (~optional (~seq #:require ~! (mask:depmask ...)))
                            . body)
  #:do [(with-caller ('define-rope-class this-syntax #f)
          (for ([mask-κ (in-list (or (attribute mask.class) null))]
                [only-ids (in-list (or (attribute mask.include) null))]
                [except-ids (in-list (or (attribute mask.exclude) null))]
                [rename-ids (in-list (or (attribute mask.from) null))])
            (with-caller-sub mask-κ
              (define class-desc (describe-class mask-κ))
              (define params (rope-class-params class-desc))
              (for ([param-id (in-list (append only-ids except-ids rename-ids))])
                (define name (syntax-e param-id))
                (or (assoc name params)
                    (with-caller-sub param-id
                      ;; TODO account for #:exclude
                      (define names-set (for/seteq ([param params]) (car param)))
                      (define names-list (for/list ([param params])
                                           (symbol->string (car param))))
                      (with-suggest (name names-set (reverse names-list))
                        (rope-error "expected a parameter of ~a" (syntax-e mask-κ)))))))))]
  #:with (~var rope:%) (format-id #'κ "rope:~a" (syntax-e #'κ))

  (define-syntax rope:%
    (let ([params (for/list ([name (in-list '(field.name ...))]
                             [cls (in-syntax #'(field.stxclass ...))]
                             [dfl (in-syntax #'(field.default ...))])
                    (cons name (class-param cls dfl)))]
          [masks (~? (for/list ([id (in-syntax #'(mask.class ...))]
                                [has-include? (in-syntax #'(mask.has-include? ...))]
                                [includes (in-syntax #'((mask.include ...) ...))]
                                [excludes (in-syntax #'((mask.exclude ...) ...))]
                                [froms (in-syntax #'((mask.from ...) ...))]
                                [tos (in-syntax #'((mask.to ...) ...))])
                       (mask-spec id
                                  has-include?
                                  (syntax->list includes)
                                  (syntax->list excludes)
                                  (map cons (syntax->list froms) (syntax->list tos))))
                     null)])
      (rope-class
       (reverse params) masks (quote-syntax body)))))
