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
              [only-ids (in-list (or (attribute req.onlys) null))]
              [except-ids (in-list (or (attribute req.excepts) null))]
              [rename-ids (in-list (or (attribute req.rename-from) null))])
          (set-caller-sub-expr! req-κ)
          (define class-desc (describe-class req-κ))
          (define params (rope-class-params class-desc))
          (for ([param-id (in-list (append only-ids except-ids rename-ids))])
            (or (assoc (syntax-e param-id) params)
                (with-sub-expr param-id
                  (rope-error "expected a parameter of ~a" (syntax-e req-κ))))))]
  #:with (~var rope:%) (format-id #'κ "rope:~a" (syntax-e #'κ))
  #:do [(pop-caller)]

  (define-syntax rope:%
    (rope-class
     (reverse
      (list (cons 'field.name (class-param #'field.stxclass #'field.default)) ...))
     (~? (reverse (list (require-spec #'req.id
                                      (list #'req.onlys ...)
                                      (list #'req.excepts ...)
                                      (list (cons #'req.rename-from 'req.rename-to) ...))
                        ...))
         null)
     (quote-syntax body))))
