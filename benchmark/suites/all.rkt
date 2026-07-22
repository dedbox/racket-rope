#lang racket/base

;;; suites/all.rkt -- the suite registry. A "suite" is just a function from a
;;; list of sizes to a list of benches. New suites, or new rope types,
;;; register here and automatically pick up main.rkt's CLI.

(require "../generators.rkt"
         "comparison.rkt"
         "generic-ops.rkt"
         "equality.rkt")

(provide suite-registry suite-names)

;; name -> (sizes -> (listof bench))
(define suite-registry
  (list
   (cons "string-core"     (λ (sizes) (make-core-benchmarks      string-ops sizes)))
   (cons "bytes-core"      (λ (sizes) (make-core-benchmarks       bytes-ops sizes)))
   (cons "string-equality" (λ (sizes) (make-equality-benchmarks   string-ops sizes)))
   (cons "bytes-equality"  (λ (sizes) (make-equality-benchmarks   bytes-ops sizes)))
   (cons "string-comparison" (λ (sizes) (make-comparison-benchmarks string-ops sizes)))
   (cons "bytes-comparison"  (λ (sizes) (make-comparison-benchmarks bytes-ops  sizes)))))

(define suite-names (map car suite-registry))
