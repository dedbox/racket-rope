#lang racket/base

(require rope/type
         ;; rope/class/total-order
         ;; rope/instance
         )

(provide (all-defined-out))

(define-rope-type string
  #:chunk?       string?
  #:chunk-limit  512
  #:chunk-empty  ""
  #:chunk-length string-length
  #:chunk-ref    string-ref
  #:chunk-slice  (λ (c i k) (substring c i (+ i k)))
  #:chunk-append (λ (cs) (apply string-append cs))
  #:elem-width   1
  ;; content-based equality
  #:chunk=?      string=?
  #:elem=?       char=?
  #:elem-hash    char->integer)

(define empty-string-rope (make-empty-string-rope))

(define (string->rope str) (source->string-rope str))
(define (rope->string r) (string-rope->source r))

;; (define-rope-instance total-order string ([elem<? char<?] [elem>? char>?]))
