#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/struct
         syntax/parse/define
         "./edit-distance.rkt")

(provide (all-defined-out))

(struct caller (name expr sub-expr) #:transparent)

(define current-caller (make-parameter (caller #f #f #f)))

(define (get-caller-info) (apply values (struct->list (current-caller))))

(define-syntax-parse-rule (with-caller (name expr sub) . body)
  (parameterize ([current-caller (caller name expr sub)]) . body))

(define-syntax-parse-rule (with-caller-sub sub-expr . body)
  (let-values ([(name expr _) (get-caller-info)])
    (parameterize ([current-caller (caller name expr sub-expr)]) . body)))

(struct suggest (name names-set names-list) #:transparent)

(define current-suggest (make-parameter #f))

(define (get-suggest-info) (apply values (struct->list (current-suggest))))

(define-syntax-parse-rule (with-suggest (name names-set names-list) . body)
  (parameterize ([current-suggest (suggest name names-set names-list)]) . body))

(define (rope-error fmt . args)
  (define-values (caller expr sub-expr) (get-caller-info))
  (define-values (fmt* args*)
    (if (current-suggest)
        (let*-values ([(name set lst) (get-suggest-info)]
                      [(other-name) (nearest-name name set lst)])
          (if other-name
              (values (string-append fmt " (did you mean ~a?)")
                      (append args (list other-name)))
              (values fmt args)))
        (values fmt args)))
  (raise-syntax-error caller (apply format fmt* args*) expr sub-expr))
