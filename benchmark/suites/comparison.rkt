#lang racket/base

(require rope
         "../suite.rkt")

(provide make-comparison-benchmarks)

;; Two ropes equal for the first (n - Δ) elements, then diverge, built via
;; different tree shapes so the walk must actually cross leaf boundaries.
(define (fixture n diverge-at)
  (define base (make-string n #\a))
  (define s1 (string->rope base))
  (define s2 (string-rope-splice (string->rope base) diverge-at 1 "b"))
  (cons s1 s2))

(define (make-comparison-benchmarks sizes)
  (apply append
         (for/list ([n (in-list sizes)] #:when (> n 0))
           (list
            (make-bench "compare/diverge-at-start" "compare" n
                        (λ () (fixture n 0)) (λ (p) (string-rope-compare (car p) (cdr p))))
            (make-bench "compare/diverge-at-end" "compare" n
                        (λ () (fixture n (sub1 n))) (λ (p) (string-rope-compare (car p) (cdr p))))
            (make-bench "compare/equal" "compare" n
                        (λ () (cons (string->rope (make-string n #\a))
                                    (string->rope (make-string n #\a))))
                        (λ (p) (string-rope-compare (car p) (cdr p))))
            (make-bench "ci-compare/equal-different-case" "compare" n
                        (λ () (cons (string->rope (make-string n #\a))
                                    (string->rope (make-string n #\A))))
                        (λ (p) (string-rope-ci-compare (car p) (cdr p))))))))
