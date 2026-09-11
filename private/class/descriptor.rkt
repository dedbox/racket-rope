#lang racket/base

;; rope/private/class/descriptor.rkt

(provide (all-defined-out))

(struct rope-class-descriptor
  (;; primitives
   elem-hash
   chunk=?
   chunk-overlap=?
   ;; derived operations
   chunk-hash
   node-hash
   rope-hash-proc
   rope-content=?) #:transparent)
