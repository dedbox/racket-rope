#lang racket

;; rope/benchmarks/rebalance-cost-bench.rkt
;;
;; Classifies each append as trivial or real-rebalance using the same
;; O(1) predicate the library already exposes, and reports GC time
;; separately from compute time for each bucket. This tells you whether
;; rebalancing is even a meaningful fraction of total session cost, and
;; whether the tail is GC or algorithm — the two things last session's
;; benchmark couldn't distinguish.

(require racket/format
         rope2/generic
         rope2/rope
         rope2/rope-type
         rope2/string-rope)

(define SESSION-LENGTH 20000)

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)  (format "~a sec"  (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

;; Returns (values trivial-samples rebalance-samples), each a list of
;; (compute-ms . gc-ms) pairs.
(define (run-session/instrumented len)
  (define-values (_r trivial rebalance)
    (for/fold ([r (make-empty-string-rope)] [trivial null] [rebalance null])
              ([_ (in-range len)])
      (define leaf     (make-rope-leaf string "a"))
      (define combined (string-rope-concat r leaf))
      (define will-rebalance? (not (rope-mostly-balanced? combined)))
      (define gc0    (current-gc-milliseconds))
      (define start  (current-inexact-monotonic-milliseconds))
      (define r*     (if will-rebalance? (string-rope-rebalance combined) combined))
      (define end    (current-inexact-monotonic-milliseconds))
      (define gc1    (current-gc-milliseconds))
      (define sample (cons (- (- end start) (- gc1 gc0)) (- gc1 gc0))) ; (compute . gc)
      (if will-rebalance?
          (values r* trivial (cons sample rebalance))
          (values r* (cons sample trivial) rebalance))))
  (values (reverse trivial) (reverse rebalance)))

(define (summarize label samples)
  (define computes (map car samples))
  (define gcs      (map cdr samples))
  (printf "~a: n=~a  compute-mean=~a  compute-max=~a  total-gc=~a\n"
          label (length samples)
          (format-result (if (null? computes) 0 (/ (apply + computes) (length computes))))
          (format-result (if (null? computes) 0 (apply max computes)))
          (format-result (apply + gcs))))

(module+ main
  (define-values (_x _y) (run-session/instrumented 2000)) ;; warmup
  (define-values (trivial rebalance) (run-session/instrumented SESSION-LENGTH))
  (summarize "trivial appends  " trivial)
  (summarize "real rebalances  " rebalance))
