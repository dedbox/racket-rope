#lang racket/base

;; rope/private/type/descriptor.rkt

(provide (all-defined-out))

(struct rope-type-descriptor
  (;; chunk operations
   chunk?
   chunk-limit
   chunk-empty
   chunk-length
   chunk-width
   chunk-ref
   chunk-slice
   chunk-append
   ;; element operations
   elem-width
   ;; smart constructors
   make-leaf
   make-node
   ;; internal hashing / equality
   chunk-hash
   node-hash
   rope-content=?)
  #:transparent)

;; (struct rope-type-descriptor
;;   ;; user-defined operations
;;   (chunk?
;;    chunk-limit
;;    chunk-empty
;;    chunk-length
;;    chunk-width
;;    chunk-ref
;;    chunk-slice
;;    chunk-append
;;    ;; chunk=?
;;    chunk-compare
;;    chunk-compare-overlap
;;    ;; chunk-overlap=?
;;    elem-width
;;    ;; elem-hash
;;    elem<?
;;    elem>?
;;    ;; automatically generated operations
;;    leaf-constructor
;;    node-constructor
;;    ;; rope-chunk-hash
;;    ;; rope-node-hash
;;    ;; make-rope-hash
;;    ;; content=?
;;    )
;;   #:transparent)
