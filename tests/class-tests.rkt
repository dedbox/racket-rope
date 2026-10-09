#lang racket/base

;; Scope is deliberately limited to declaration-time correctness of
;; define-rope-class itself: field-spec's three forms, and #:require's
;; validation that every #:include/#:exclude/#:rename name is actually a
;; parameter of the class being depended on. No instance is built here --
;; that only matters once #:require's imports actually need to be
;; *resolved* against a real instance, which is instance.rkt's job and
;; (for the full multi-class chain) class-dependency-tests.rkt's.

(module+ test
  (require (for-syntax rope/stxclasses)
           rope/class
           "../private/testing.rkt")

  (define-rope-class base-class
    ([p id+fun1]
     [q id+fun1]))

  (define field-spec-suite
    (test-suite "field-spec"
      (test-case "a required field with no default compiles"
        (check-true (expands-ok? (define-rope-class t1 ([x id+fun1])))))

      (test-case "#:optional compiles, with no validation of anything"
        (check-true (expands-ok? (define-rope-class t2 ([x id+fun1 #:optional])))))

      (test-case "#:default with a matching expression compiles"
        (check-true (expands-ok?
                     (define-rope-class t3 ([x id+fun2 #:default (lambda (a b) a)])))))

      (test-case "#:default is checked against the declared class at definition time"
        (check-true
         (expand-fails?
          (define-rope-class t4 ([x id+fun2 #:default (lambda (a) a)]))))
        (check-regexp-match
         #rx"two-argument function"
         (expand-error-message
          (define-rope-class t5 ([x id+fun2 #:default (lambda (a) a)])))))))

  (define require-suite
    (test-suite "#:require"
      (test-case "no #:require clause at all compiles"
        (check-true (expands-ok? (define-rope-class t6 ([x id+fun1])))))

      (test-case "a bare dependency (import everything) compiles"
        (check-true (expands-ok?
                     (define-rope-class t7 ([x id+fun1]) #:require (base-class)))))

      (test-case "#:include compiles when every name is a real parameter"
        (check-true
         (expands-ok?
          (define-rope-class t8 ([x id+fun1]) #:require ([base-class #:include (p)])))))

      (test-case "#:exclude compiles when every name is a real parameter"
        (check-true
         (expands-ok?
          (define-rope-class t9 ([x id+fun1]) #:require ([base-class #:exclude (p)])))))

      (test-case "#:rename compiles when every `from` name is a real parameter"
        (check-true
         (expands-ok?
          (define-rope-class t10 ([x id+fun1])
            #:require ([base-class #:rename ([p p2] [q q2])])))))

      (test-case "#:include then #:rename on a disjoint name compiles"
        (check-true
         (expands-ok?
          (define-rope-class t11 ([x id+fun1])
            #:require ([base-class #:include (p) #:rename ([q q2])])))))

      (test-case "an unknown #:include name fails, naming the right class"
        (check-true
         (expand-fails?
          (define-rope-class t12 ([x id+fun1]) #:require ([base-class #:include (zzz)]))))
        (check-regexp-match
         #rx"base-class"
         (expand-error-message
          (define-rope-class t13 ([x id+fun1]) #:require ([base-class #:include (zzz)])))))

      (test-case "an unknown #:exclude name fails"
        (check-true
         (expand-fails?
          (define-rope-class t14 ([x id+fun1]) #:require ([base-class #:exclude (zzz)])))))

      (test-case "an unknown #:rename `from` name fails"
        (check-true
         (expand-fails?
          (define-rope-class t15 ([x id+fun1])
            #:require ([base-class #:rename ([zzz z2])])))))

      (test-case "a near-miss unknown name gets a \"did you mean\" suggestion"
        (check-regexp-match
         #rx"did you mean p"
         (expand-error-message
          (define-rope-class t16 ([x id+fun1]) #:require ([base-class #:include (pp)])))))))
  ;; NOTE: #:include/#:exclude/#:rename's *mutual-exclusivity* rules
  ;; ("cannot rename an included/excluded parameter") are enforced by
  ;; validate-mask! in private/instance.rkt, which only runs once an
  ;; instance of the *requiring* class is actually built (via
  ;; define-rope-instance or define-class-op) -- define-rope-class alone
  ;; never reaches it. Those cases belong to, and are tested in,
  ;; instance.rkt's own suite.

  (run-suite! (test-suite "class.rkt" field-spec-suite require-suite)))
