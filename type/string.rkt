#lang racket/base

(require rope/class
         rope/class/total-order
         rope/instance
         rope/type)

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
  ;; equality class members
  #:chunk=?      string=?
  #:elem=?       char=?
  #:elem-hash    char->integer)

(define-rope-instance total-order string
  ([elem<? char<?]
   [elem>? char>?]))
