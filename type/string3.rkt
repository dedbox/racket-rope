#lang racket/base

;; rope/type/string3.rkt

(require rope2/class
         rope2/type)

(provide (all-defined-out))

(define-rope-type string3
  #:chunk?                string?
  #:chunk-limit           3
  #:chunk-empty           ""
  #:chunk-length          string-length
  #:chunk-ref             string-ref
  #:chunk-slice           (λ (c i k) (substring c i (+ i k)))
  #:chunk-append          (λ (cs) (apply string-append cs))
  #:chunk=?               string=?
  #:chunk-overlap=?       (λ (ca cb ia ib k)
                            (for/and ([i (in-range k)])
                              (char=? (string-ref ca (+ ia i)) (string-ref cb (+ ib i)))))
  ;; #:chunk-compare-overlap (λ (ac bc ap bp k)
  ;;                           (let loop ([i 0])
  ;;                             (cond [(= i k) '=]
  ;;                                   [(char<? (string-ref ac (+ ap i)) (string-ref bc (+ bp i))) '<]
  ;;                                   [(char>? (string-ref ac (+ ap i)) (string-ref bc (+ bp i))) '>]
  ;;                                   [else (loop (add1 i))])))
  #:elem-width            1
  #:elem-hash             char->integer
  #:elem<?                char<?
  #:elem>?                char>?)

;; (require rope2/generic-ops)
;; (require rope2/cursor)
;; (require rope2/rope)
;; (require racket/sequence)

(define-rope-Eq-instance string3)
