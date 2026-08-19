#lang racket/base

;; rope/generic/fixnum.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor)
         racket/unsafe/ops
         racket/include
         racket/splicing
         rope2/private/math-ops
         syntax/parse/define)

(provide (all-defined-out))

(splicing-syntax-parameterize
    ([^+         (make-rename-transformer #'unsafe-fx+)]
     [^-         (make-rename-transformer #'unsafe-fx-)]
     [^*         (make-rename-transformer #'unsafe-fx*)]
     [^quotient  (make-rename-transformer #'unsafe-fxquotient)]
     [^remainder (make-rename-transformer #'unsafe-fxremainder)]
     [^modulo    (make-rename-transformer #'unsafe-fxmodulo)]
     [^abs       (make-rename-transformer #'unsafe-fxabs)]
     [^=         (make-rename-transformer #'unsafe-fx=)]
     [^<         (make-rename-transformer #'unsafe-fx<)]
     [^>         (make-rename-transformer #'unsafe-fx>)]
     [^<=        (make-rename-transformer #'unsafe-fx<=)]
     [^>=        (make-rename-transformer #'unsafe-fx>=)]
     [^max       (make-rename-transformer #'unsafe-fxmax)]
     [^min       (make-rename-transformer #'unsafe-fxmin)]
     [^add1      (make-rename-transformer #'unsafe-fxadd1)]
     [^sub1      (make-rename-transformer #'unsafe-fxsub1)])
  (include "../private/generic-ops.rktl"))
