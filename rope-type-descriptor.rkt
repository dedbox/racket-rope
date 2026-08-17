#lang racket/base

(provide (all-defined-out))

(struct rope-type-descriptor
  ;; user-defined operations
  (chunk? elem-size chunk-limit chunk-empty chunk-count
          chunk-size chunk-slice chunk-append chunk-ref
          chunk-compare chunk-overlap=?
          ;; automatically generated operations
          leaf-constructor node-constructor)
  #:transparent)
