#lang racket/base

;; rope/rope-type-descriptor.rkt

(provide (all-defined-out))

(struct rope-type-descriptor
  ;; user-defined operations
  (chunk?
   chunk-limit
   chunk-empty
   chunk-length
   chunk-width
   chunk-ref
   chunk-slice
   chunk-append
   chunk-compare
   chunk-overlap=?
   elem-width
   elem-hash
   ;; automatically generated operations
   leaf-constructor
   node-constructor
   rope-chunk-hash
   rope-node-hash
   make-rope-hash
   content=?)
  #:transparent)
