#lang racket/base

;;; suites/generic-ops.rkt -- core rope operation benchmarks: build, flatten,
;;; append, split, splice, slice, offset-index, cursor walk, fold, sequence
;;; iteration, and the three fixture-construction shapes (typed, fragmented,
;;; edited) as benchmarks in their own right. Written once against
;;; `rope-type-ops`, so it runs unmodified against every rope type.

(require racket/contract
         racket/format
         rope/rope
         "../bench-core.rkt"
         "../suite.rkt"
         "../type-ops.rkt")

(provide make-core-benchmarks)

(define/contract (make-core-benchmarks ops sizes)
  (-> rope-type-ops? (listof exact-nonnegative-integer?) (listof bench?))
  (define label (rope-type-ops-label ops))
  (define (bname op) (~a label "/" op))
  (define (build n) ((rope-type-ops-to-rope ops) ((rope-type-ops-random-raw ops) n)))

  (apply append
         (for/list ([n (in-list sizes)])
           (list

            (make-bench (bname 'build) "core/build" n
                        (λ () ((rope-type-ops-random-raw ops) n))
                        (λ (raw) ((rope-type-ops-to-rope ops) raw)))

            (make-bench (bname 'flatten) "core/build" n
                        (λ () (build n))
                        (λ (r) ((rope-type-ops-to-raw ops) r)))

            (make-bench (bname 'append1) "core/edit" n
                        (λ () (cons (build n) (build n)))
                        (λ (p) ((rope-type-ops-append1 ops) (car p) (cdr p))))

            (make-bench (bname 'split-midpoint) "core/edit" n
                        (λ () (build (max 1 n)))
                        (λ (r)
                          (call-with-values
                           (λ () ((rope-type-ops-split ops) r (quotient (rope-length r) 2)))
                           (λ (l rt) (cons l rt)))))

            (make-bench (bname 'splice-4-at-midpoint) "core/edit" n
                        (λ () (build (max 1 n)))
                        (λ (r)
                          (define len (rope-length r))
                          (define start (quotient len 2))
                          (define old-len (min 4 (- len start)))
                          ((rope-type-ops-splice ops)
                           r start old-len ((rope-type-ops-random-raw ops) 4))))

            (make-bench (bname 'slice-quarter) "core/edit" n
                        (λ () (build (max 1 n)))
                        (λ (r)
                          (define len (rope-length r))
                          ((rope-type-ops-slice ops) r (quotient len 4) (quotient len 4))))

            (make-bench (bname 'offset-index-midpoint) "core/index" n
                        (λ () (build (max 1 n)))
                        (λ (r) ((rope-type-ops-offset-index ops) r (quotient (rope-width r) 2))))

            (make-bench (bname 'cursor-full-walk) "core/cursor" n
                        (λ () (build n))
                        (λ (r)
                          (let loop ([c ((rope-type-ops-to-cursor ops) r)] [i 0])
                            (if ((rope-type-ops-cursor-at-end? ops) c)
                                i
                                (loop ((rope-type-ops-cursor-advance ops) c) (add1 i))))))

            (make-bench (bname 'cursor-peek-walk) "core/cursor" n
                        (λ () (build n))
                        (λ (r)
                          (let loop ([c ((rope-type-ops-to-cursor ops) r)] [acc 0])
                            (if ((rope-type-ops-cursor-at-end? ops) c)
                                acc
                                (loop ((rope-type-ops-cursor-advance ops) c)
                                      (+ acc (if ((rope-type-ops-cursor-peek ops) c) 1 0)))))))

            (make-bench (bname 'fold-count) "core/fold" n
                        (λ () (build n))
                        (λ (r) ((rope-type-ops-fold ops) (λ (acc _e) (add1 acc)) 0 r)))

            (make-bench (bname 'sequence-walk) "core/sequence" n
                        (λ () (build n))
                        (rope-type-ops-walk-sequence ops))

            (make-bench (bname 'typed-build) "core/shape" n
                        (λ () ((rope-type-ops-random-raw ops) n))
                        (λ (raw) (typed-rope-from ops raw)))

            (make-bench (bname 'fragmented-build) "core/shape" n
                        (λ () ((rope-type-ops-random-raw ops) n))
                        (λ (raw) (fragmented-rope-from ops raw)))

            (make-bench (bname 'edited-build) "core/shape" n
                        (λ () ((rope-type-ops-random-raw ops) n))
                        (λ (raw) (edited-rope-from ops raw)))))))
