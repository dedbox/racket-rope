#lang racket/base

;; rope/string-rope.rkt

(require rope2/rope-type)

(provide (all-defined-out))

(define-rope-type string
  #:chunk?        string?
  #:elem-size     1
  #:chunk-limit   (λ () 512)
  #:chunk-empty   (λ () "")
  #:chunk-count   string-length
  #:chunk-size    string-length
  #:chunk-slice   (λ (str i k) (substring str i (+ i k)))
  #:chunk-append  (λ (strs) (apply string-append strs))
  #:chunk-ref     string-ref
  #:chunk-compare (λ (a b) (cond [(string<? a b) '<] [(string=? a b) '=] [else '>])))

(define empty-string-rope (make-empty-string-rope))

(require racket/list)

(define a
  (for/fold ([a empty-string-rope])
            ([s (in-list (map string (make-list 300 #\a)))])
    (string-rope-concat a (string->rope s))))

(define b (string-rope-rebalance a))
