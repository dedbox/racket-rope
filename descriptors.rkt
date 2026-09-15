#lang racket/base

(provide (all-defined-out))

(struct rope-class-descriptor (primitives) #:transparent)

(struct rope-instance-descriptor (members) #:transparent)

(struct rope-type-descriptor
  (chunk?
   chunk-limit
   chunk-empty
   chunk-length
   chunk-width
   chunk-ref
   chunk-slice
   chunk-append
   elem-width
   make-leaf
   make-node)
  #:transparent)
