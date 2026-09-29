#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/struct
         syntax/parse/define)

(provide (all-defined-out))

(struct caller (name expr sub-expr) #:transparent)

(define current-callers (make-parameter (list (caller #f #f #f))))

(define (get-caller)
  (apply values (struct->list (car (current-callers)))))

(define (push-caller name expr sub-expr)
  (current-callers (cons (caller name expr sub-expr) (current-callers))))

(define (pop-caller)
  (define callers (current-callers))
  (if (null? (cdr callers))
      (car callers)
      (begin0 (get-caller) (current-callers (cdr callers)))))

(define (set-caller-name! name)
  (define-values (_ expr sub-expr) (pop-caller))
  (push-caller name expr sub-expr))

(define (set-caller-expr! expr)
  (define-values (name _ sub-expr) (pop-caller))
  (push-caller name expr sub-expr))

(define (set-caller-sub-expr! sub-expr)
  (define-values (name expr _) (pop-caller))
  (push-caller name expr sub-expr))

(define (set-caller-exprs! expr sub-expr)
  (define-values (name _e _s) (pop-caller))
  (push-caller name expr sub-expr))

(define-syntax-parse-rule (with-expr expr body ...)
  (let-values ([(_n old-expr _s) (get-caller)])
    (set-caller-expr! expr)
    (begin0 (begin body ...) (set-caller-expr! old-expr))))

(define-syntax-parse-rule (with-sub-expr sub-expr body ...)
  (let-values ([(_n _e old-sub-expr) (get-caller)])
    (set-caller-sub-expr! sub-expr)
    (begin0 (begin body ...) (set-caller-sub-expr! old-sub-expr))))






;; (define-syntax-parse-rule (with-caller (name expr sub-expr) . body)
;;   (parameterize ([current-callers (caller name expr sub-expr)]) . body))

;; (define-syntax-parse-rule (with-caller-exprs (expr sub-expr) . body)
;;   (let ([old-caller (current-callers)])
;;     (define new-caller
;;       (if old-caller
;;           (caller (caller-name old-caller) expr sub-expr)
;;           (caller #f expr sub-expr)))
;;     (parameterize ([current-callers new-caller]) . body)))

;; (define-syntax-parse-rule (with-sub-expr sub-expr . body)
;;   (let ([old-caller (current-callers)])
;;     (define new-caller
;;       (if old-caller
;;           (caller (caller-name old-caller) (caller-expr old-caller) sub-expr)
;;           (caller #f #f sub-expr)))
;;     (parameterize ([current-callers new-caller]) . body)))

;; (define-syntax-parse-rule (with-caller-expr expr . body)
;;   (let ([old-caller (current-callers)])
;;     (define new-caller
;;       (if old-caller
;;           (caller (caller-name old-caller) expr (caller-sub-expr old-caller))
;;           (caller #f expr #f)))
;;     (parameterize ([current-callers new-caller]) . body)))

(define (rope-error fmt . args)
  (define-values (caller expr sub-expr) (get-caller))
  (raise-syntax-error caller (apply format fmt args) expr sub-expr))
