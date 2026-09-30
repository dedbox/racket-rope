#lang racket/base

(require
 ;; (for-template racket/base)
 racket/syntax
 racket/syntax-srcloc
 ;; syntax/parse
 "./class.rkt"
 "./error.rkt"
 "./type.rkt"
 )

(provide (all-defined-out))

(struct rope-instance (bindings) #:transparent)

(define (describe-instance κ-stx τ-stx)
  (with-sub-expr κ-stx (describe-class κ-stx))
  (with-sub-expr τ-stx (describe-type τ-stx))
  (define κ-name (syntax-e κ-stx))
  (define τ-name (syntax-e τ-stx))
  (define desc-id (format-id τ-stx "rope:~a:~a" κ-name τ-name))
  (or (syntax-local-value desc-id (λ () #f))
      (rope-error "no instance of ~a for ~a" τ-name κ-name)))

(define (alist-remq-or-fail v lst)
  (unless (list? lst)
    (raise-argument-error 'alist-remq-or-fail "list?" lst))
  (unless (or (null? lst) (pair? (car lst)))
    (raise-argument-error 'alist-remq-or-fail "(or/c null? pair?)" (car lst)))
  (cond [(null? lst) #f]
        [(eq? (caar lst) v) (cdr lst)]
        [else
         (define lst* (alist-remq-or-fail v (cdr lst)))
         (and lst* (cons (car lst) lst*))]))

(define (alist-rename-or-fail v-old v-new lst)
  (unless (list? lst)
    (raise-argument-error 'alist-remq-or-fail "list?" lst))
  (unless (or (null? lst) (pair? (car lst)))
    (raise-argument-error 'alist-rename-or-fail "(or/c null? pair?)" (car lst)))
  (cond [(null? lst) #f]
        [(eq? (caar lst) v-old) (cons (cons v-new (cdar lst)) (cdr lst))]
        [else
         (define lst* (alist-rename-or-fail v-old v-new (cdr lst)))
         (and lst* (cons (car lst) lst*))]))

(define (resolve-imports τ-stx reqs)
  (apply append
         (for/list ([req (in-list reqs)])
           (define κ-stx (require-spec-class req))
           (define desc (with-sub-expr τ-stx (describe-instance κ-stx τ-stx)))
           (define bindings (rope-instance-bindings desc))
           (rename-bindings req (except-bindings req bindings)))))

(define (except-bindings req bindings)
  (for/fold ([bindings bindings]) ([name (in-list (require-spec-excepts req))])
    (or (alist-remq-or-fail name bindings)
        (with-sub-expr name (rope-error "class parameter is not in scope")))))

(define (rename-bindings req bindings)
  (define excepts (require-spec-excepts req))
  (for/fold ([bindings bindings]) ([ren (in-list (require-spec-renames req))])
    (define name (car ren))
    (cond
      [(memq name excepts)
       (with-sub-expr name (rope-error "duplicate import mask"))]
      [(alist-rename-or-fail name (cdr ren) bindings) => values]
      [else
       (with-sub-expr name (rope-error "class parameter is not in scope"))])))
