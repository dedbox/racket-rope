#lang racket/base

;;; suites/equality.rkt -- generic equal?/equal-hash-code benchmark matrix.
;;; See type-ops.rkt for the generic vocabulary and the shape/perturbation
;;; helpers this reuses.
;;;
;;; Every scenario contributes three benches:
;;;   - equal?           : the comparison itself
;;;   - hash-code.warm   : equal-hash-code on a fixture reused across all
;;;                        trials, so an eq?-keyed memoization cache (if any)
;;;                        gets to show its steady-state benefit starting
;;;                        from the second trial
;;;   - hash-code.cold   : equal-hash-code on a fresh fixture every single
;;;                        trial, with no warmup, so the true first-call cost
;;;                        is what's measured, not what a cache makes it look
;;;                        like once primed

(require racket/contract
         racket/format
         "../suite.rkt"
         "../type-ops.rkt")

(provide scenario-labels
         scenario-same-content?
         make-equality-benchmarks)

(define scenario-labels
  '(identical-object
    same-content-same-shape
    same-content-typed-shape
    same-content-fragmented-shape
    differ-at-start
    differ-at-end
    differ-in-middle))

;; The intended A/B relationship for `label`, independent of whatever any
;; particular equal? implementation actually returns. A report can use this to
;; flag disagreement between what a scenario is supposed to test and what a
;; given run's `equal?` actually said.
(define (scenario-same-content? label)
  (not (memq label '(differ-at-start differ-at-end differ-in-middle))))

(define (build-pair ops n label)
  (define raw ((rope-type-ops-random-raw ops) (max n 1)))
  (define (rope r) ((rope-type-ops-to-rope ops) r))
  (case label
    [(identical-object)
     (define r (rope raw))
     (cons r r)]
    [(same-content-same-shape)
     (cons (rope raw) (rope raw))]
    [(same-content-typed-shape)
     (cons (rope raw) (typed-rope-from ops raw))]
    [(same-content-fragmented-shape)
     (cons (rope raw) (fragmented-rope-from ops raw))]
    [(differ-at-start)
     (cons (rope raw) (rope (perturb-raw ops raw 0)))]
    [(differ-at-end)
     (cons (rope raw) (rope (perturb-raw ops raw (sub1 (max 1 n)))))]
    [(differ-in-middle)
     (cons (rope raw) (rope (perturb-raw ops raw (quotient n 2))))]
    [else (error 'build-pair "unknown scenario: ~a" label)]))

(define/contract (make-equality-benchmarks ops sizes)
  (-> rope-type-ops? (listof exact-nonnegative-integer?) (listof bench?))
  (define type-label (rope-type-ops-label ops))
  (apply append
         (for*/list ([n (in-list sizes)]
                     [label (in-list scenario-labels)])
           (define (bname op) (~a op "/" type-label "/" label))
           (define group (~a "equality/" type-label))
           (list
            (make-bench (bname 'equal?) group n
                        (λ () (build-pair ops n label))
                        (λ (p) (equal? (car p) (cdr p))))
            (make-bench (bname 'hash-code.warm) group n
                        (λ () (build-pair ops n label))
                        (λ (p) (equal-hash-code (car p))))
            (make-bench (bname 'hash-code.cold) group n
                        (λ () (build-pair ops n label))
                        (λ (p) (equal-hash-code (car p)))
                        #:fresh? #t)))))
