#lang racket/base

(require racket/contract
         racket/format
         rope
         "../suite.rkt"
         "../type-ops.rkt"
         "./equality.rkt")

(provide make-comparison-benchmarks)

(define/contract (make-comparison-benchmarks ops sizes)
  (-> rope-type-ops? (listof exact-nonnegative-integer?) (listof bench?))
  (define type-label (rope-type-ops-label ops))
  (define ci? (string=? type-label "string"))
  (apply append
         (for*/list ([n (in-list sizes)] [label (in-list scenario-labels)])
           (define (bname op) (~a op "/" type-label "/" label))
           (define group (~a "comparison/" type-label))
           (append
            (list
             (make-bench (bname 'compare) group n
                         (λ () (build-pair ops n label))
                         (λ (p) ((rope-type-ops-compare ops) (car p) (cdr p))))
             (make-bench (bname 'rope=?) group n
                         (λ () (build-pair ops n label))
                         (λ (p) ((rope-type-ops-rope=? ops) (car p) (cdr p)))))
            (if (and ci? (> n 0))
                (list (make-bench (~a "ci-compare/" type-label "/" label) group n
                                  (λ () (build-pair ops n label))
                                  (λ (p) (string-rope-ci-compare (car p) (cdr p)))))
                '())))))
