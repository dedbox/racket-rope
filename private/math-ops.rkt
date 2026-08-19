#lang racket/base

;; rope/private/math-ops.rkt

(require (for-syntax racket/base)
         racket/stxparam)

(provide (all-defined-out))

(define-syntax-parameter ^+         (make-rename-transformer #'+))
(define-syntax-parameter ^-         (make-rename-transformer #'-))
(define-syntax-parameter ^*         (make-rename-transformer #'*))
(define-syntax-parameter ^quotient  (make-rename-transformer #'quotient))
(define-syntax-parameter ^remainder (make-rename-transformer #'remainder))
(define-syntax-parameter ^modulo    (make-rename-transformer #'modulo))
(define-syntax-parameter ^abs       (make-rename-transformer #'abs))
(define-syntax-parameter ^=         (make-rename-transformer #'=))
(define-syntax-parameter ^<         (make-rename-transformer #'<))
(define-syntax-parameter ^>         (make-rename-transformer #'>))
(define-syntax-parameter ^<=        (make-rename-transformer #'<=))
(define-syntax-parameter ^>=        (make-rename-transformer #'>=))
(define-syntax-parameter ^max       (make-rename-transformer #'max))
(define-syntax-parameter ^min       (make-rename-transformer #'min))
(define-syntax-parameter ^add1      (make-rename-transformer #'add1))
(define-syntax-parameter ^sub1      (make-rename-transformer #'sub1))
