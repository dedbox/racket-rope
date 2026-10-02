#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/struct
         syntax/parse/define)

(provide (all-defined-out))

(struct caller (name expr sub-expr) #:transparent)

(define current-caller (make-parameter (caller #f #f #f)))

(define (get-caller-info) (apply values (struct->list (current-caller))))

(define (rope-error fmt . args)
  (define-values (caller expr sub-expr) (get-caller-info))
  (raise-syntax-error caller (apply format fmt args) expr sub-expr))

(define-syntax-parse-rule (with-caller (name expr sub) . body)
  (parameterize ([current-caller (caller name expr sub)]) . body))

(define-syntax-parse-rule (with-caller-sub sub-expr . body)
  (let-values ([(name expr _) (get-caller-info)])
    (parameterize ([current-caller (caller name expr sub-expr)]) . body)))
