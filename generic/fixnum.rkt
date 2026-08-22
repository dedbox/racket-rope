#lang racket/base

;; rope/generic/fixnum.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor)
         racket/fixnum
         racket/include
         racket/splicing
         rope2/private/math-ops
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(splicing-syntax-parameterize
    ([^+         (make-rename-transformer #'fx+)]
     [^-         (make-rename-transformer #'fx-)]
     [^*         (make-rename-transformer #'fx*)]
     [^quotient  (make-rename-transformer #'fxquotient)]
     [^remainder (make-rename-transformer #'fxremainder)]
     [^modulo    (make-rename-transformer #'fxmodulo)]
     [^abs       (make-rename-transformer #'fxabs)]
     [^=         (make-rename-transformer #'fx=)]
     [^<         (make-rename-transformer #'fx<)]
     [^>         (make-rename-transformer #'fx>)]
     [^<=        (make-rename-transformer #'fx<=)]
     [^>=        (make-rename-transformer #'fx>=)]
     [^max       (make-rename-transformer #'fxmax)]
     [^min       (make-rename-transformer #'fxmin)]
     [^add1      (make-rename-transformer #'fxadd1)]
     [^sub1      (make-rename-transformer #'fxsub1)])
  (include "../private/generic-ops.rktl"))
