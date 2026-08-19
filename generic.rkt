#lang racket/base

;; rope/generic.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor)
         racket/include
         rope2/rope
         syntax/parse/define
         "private/math-ops.rkt")

(provide (all-defined-out))

(include "private/generic-ops.rktl")
