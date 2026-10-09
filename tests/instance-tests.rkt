#lang racket/base

(module+ test
  (require (for-syntax rope/stxclasses)
           rope/class
           rope/generic/define
           rope/instance
           "../private/testing.rkt")

  ;; A toy class, independent of total-order, so this suite is only about
  ;; instance-building mechanics, not any particular class's own logic.
  (define-rope-class toy-class
      ([req id+fun1]
       [opt id+fun1 #:optional])
    ;; Names must match star-expand's suffix list (rope/node/leaf/chunk/
    ;; elem/cursor) to be expanded per type -- hence "chunk" here.
    (define *-chunk-req-value req)
    (define *-chunk-opt-value opt))

  (define-test-rope-type ty1)

  (define-rope-instance toy-class ty1 ([req (lambda (x) x)]))

  ;; A define-class-op'd operation, defined in this same submodule but
  ;; compiled as a genuinely separate module from both class.rkt and
  ;; instance.rkt -- confirms an instance built via define-rope-instance
  ;; is actually reachable from a real, independent macro expansion, not
  ;; just visible to code textually inside instance.rkt itself. The full
  ;; three-separate-files version of this concern lives in
  ;; class-dependency-tests.rkt.
  (define-class-op toy-class (toy-req-value τ) req)

  (define basic-suite
    (test-suite "basic instance building"
      (test-case "a required field is bound correctly in the class body"
        (check-equal? (ty1-chunk-req-value 5) 5))

      (test-case "an omitted #:optional field degrades to #f, not an error"
        (check-false ty1-chunk-opt-value))

      (test-case "the resulting instance is reachable from a separate macro expansion"
        ;; req is substituted by the supplied *expression*, so this is a
        ;; behavioral check, not an identity check against ty1-req-value.
        (check-equal? ((toy-req-value ty1) 5) 5)
        (check-equal? ((toy-req-value ty1) 'x) (ty1-chunk-req-value 'x)))))

  (define error-suite
    (test-suite "instance-building error paths"
      (test-case "a missing required field fails, naming the field"
        (check-true (expand-fails? (define-rope-instance toy-class ty-x ())))
        (check-regexp-match
         #rx"expected a rope type name"
         (expand-error-message (define-rope-instance toy-class ty-x ()))))

      (test-case "an unknown member fails, naming the class"
        (check-true
         (expand-fails?
          (define-rope-instance toy-class ty-x ([zzz (lambda (x) x)]))))
        (check-regexp-match
         #rx"toy-class"
         (expand-error-message
          (define-rope-instance toy-class ty-x ([zzz (lambda (x) x)])))))

      (test-case "a wrong-arity value fails the declared syntax class"
        (check-true
         (expand-fails?
          (define-rope-instance toy-class ty-x ([req (lambda (x y) x)])))))

      (test-case "an unknown class name fails"
        (check-true (expand-fails? (define-rope-instance no-such-class ty1 ()))))

      (test-case "an unknown type name fails"
        (check-true (expand-fails? (define-rope-instance toy-class no-such-type ()))))))

  ;; #:include/#:exclude/#:rename's mutual-exclusivity rules
  ;; (validate-mask!, private/instance.rkt) only run once an instance of
  ;; the *requiring* class is actually built -- define-rope-class's own
  ;; declaration-time check (class.rkt's own suite) only confirms every
  ;; name is a real parameter, not these additional rules. ty1 already
  ;; has a toy-class instance from the basic-suite above, which these
  ;; reuse so describe-instance has something to find before validate-mask!
  ;; itself ever runs.
  (define-rope-class dep-rename-included ([m id+fun1])
    #:require ([toy-class #:include (req) #:rename ([req req2])]))

  (define-rope-class dep-rename-excluded ([m id+fun1])
    #:require ([toy-class #:exclude (req) #:rename ([req req2])]))

  (define require-validation-suite
    (test-suite "#:require validation at instance-build time"
      (test-case "renaming an included parameter fails"
        (check-true
         (expand-fails?
          (define-rope-instance dep-rename-included ty1 ([m (lambda (x) x)])))))

      (test-case "renaming an excluded parameter fails"
        (check-true
         (expand-fails?
          (define-rope-instance dep-rename-excluded ty1 ([m (lambda (x) x)])))))))

  (run-suite! (test-suite "instance.rkt" basic-suite error-suite require-validation-suite)))
