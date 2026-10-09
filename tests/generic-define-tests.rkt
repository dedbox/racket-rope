#lang racket/base

;; This is the file most directly responsible for the cross-layer
;; substitution this project's history has repeatedly gotten wrong, so
;; its own throwaway type and class are deliberately minimal and built
;; fresh here rather than reused, to keep this suite's success
;; independent of whether any production class (e.g. total-order) is
;; itself correct.

(module+ test
  (require (for-syntax rope/stxclasses)
           rope/class
           rope/generic/define
           rope/instance
           "../private/testing.rkt")

  (define-test-rope-type dty #:chunk-limit 4)
  (define-test-rope-type dty-no-instance #:chunk-limit 4) ; deliberately never instantiated

  (define-type-op (dty-is-vec τ x) (*-chunk? x))
  (define-type-op (dty-sum-all τ . xs) (apply + xs))

  (define-rope-class op-test-class ([k id+fun1]))
  (define-rope-instance op-test-class dty ([k (lambda (x) (* x 2))]))

  (define-class-op op-test-class (dty-apply-k τ x) (k x))
  (define-class-op op-test-class (dty-class-name-of τ) 'κ)
  (define-class-op op-test-class (dty-type-name-of τ) 'τ)
  (define-class-op op-test-class (dty-chain-sum τ . xs) (apply + xs))

  (define type-op-suite
    (test-suite "define-type-op"
      (test-case "a fixed-arity operation resolves a bare type binding"
        (check-true  (dty-is-vec dty (vector 1 2 3)))
        (check-false (dty-is-vec dty "not a vector")))

      (test-case "a rest-arg (...-tailed) operation works"
        (check-equal? (dty-sum-all dty 1 2 3 4) 10))

      (test-case "an operation called with an undefined type fails to expand"
        (check-true
         (expand-fails? (dty-is-vec no-such-type-at-all (vector 1)))))))

  (define class-op-suite
    (test-suite "define-class-op"
      (test-case "a class member is resolved inside the operation body"
        (check-equal? (dty-apply-k dty 5) 10))

      (test-case "bare κ substitutes to the defining class's own name"
        (check-eq? (dty-class-name-of dty) 'op-test-class))

      (test-case "bare τ substitutes to the call site's type name"
        (check-eq? (dty-type-name-of dty) 'dty))

      (test-case "a rest-arg class operation works"
        (check-equal? (dty-chain-sum dty 1 2 3) 6))

      (test-case "calling with a type that has no instance of this class fails"
        (check-true (expand-fails? (dty-apply-k dty-no-instance 5))))))

  (run-suite! (test-suite "generic/define.rkt" type-op-suite class-op-suite)))
