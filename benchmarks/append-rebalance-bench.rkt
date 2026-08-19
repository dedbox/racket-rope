#lang racket

;; rope/benchmarks/append-rebalance-bench.rkt

(require racket/format
         racket/list
         rope2/generic
         rope2/rope
         rope2/rope-type
         rope2/string-rope)

(define (make-string-rope chunks elems)
  (define (make-str _) (make-string elems #\a))
  (string->rope (apply string-append (build-list chunks make-str))))

;; =============================================================================
;; Benchmarked Implementations
;; =============================================================================

(define (append/early-rebalance as)
  (for/fold ([l empty-string-rope]) ([r (in-list as)])
    (string-rope-append2 l r)))

(define (append/late-rebalance as)
  (let ([b (for/fold ([l empty-string-rope]) ([r (in-list as)])
             (string-rope-concat l r))])
    (if (rope-balanced? b) b (string-rope-rebalance b))))

;; =============================================================================
;; Benchmarking Apparatus
;; =============================================================================

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)  (format "~a sec" (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (measure-time proc iters trials)
  (apply min
         (for/list ([_ (in-range trials)])
           (collect-garbage)
           (collect-garbage)
           (define start (current-inexact-monotonic-milliseconds))
           (for/fold ([acc 0]) ([_ (in-range iters)])
             ;; Extract a cheap fixnum from the struct to prevent dead-code elimination
             (bitwise-xor acc (rope-count (proc))))
           (define end (current-inexact-monotonic-milliseconds))
           (/ (- end start) (exact->inexact iters)))))

;; =============================================================================
;; Execution & Tabulation
;; =============================================================================

(define BASE-ITERATIONS 5000)
(define TRIALS          5)

(define (run-benchmark)
  (printf "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
          (~a "Ropes" #:min-width 8 #:align 'right)
          (~a "Chunks" #:min-width 8 #:align 'right)
          (~a "Early Time (A)" #:min-width 15 #:align 'right)
          (~a "Late Time (B)" #:min-width 15 #:align 'right)
          (~a "Δ (A - B)" #:min-width 15 #:align 'right)
          (~a "Speedup (A/B)" #:min-width 15 #:align 'right))
  (printf "|----------+----------+-----------------+-----------------+-----------------+-----------------|\n")

  (for* ([ropes  (in-list '(10 100 1000))]
         [chunks (in-list '(1 10 100))])

    ;; 1. Generate the isolated test payload
    (define as (build-list ropes (λ (_) (make-string-rope chunks 512))))

    ;; Dynamically scale iterations inversely to workload size to prevent
    ;; unbounded execution times
    (define active-iters (max 1 (quotient BASE-ITERATIONS (* ropes chunks))))

    ;; 2. Tier-2 JIT Warmup
    (for ([_ (in-range (min active-iters 10000))])
      (append/early-rebalance as)
      (append/late-rebalance as))

    ;; 3. Strict Measurement
    (define a-ms (measure-time (λ () (append/early-rebalance as)) active-iters TRIALS))
    (define b-ms (measure-time (λ () (append/late-rebalance as)) active-iters TRIALS))

    ;; 4. Statistical Analysis
    (define speedup
      (if (zero? b-ms)
          "∞x"
          (format "~ax" (real->decimal-string (/ a-ms b-ms) 2))))

    (printf "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
            (~a ropes #:min-width 8 #:align 'right)
            (~a chunks #:min-width 8 #:align 'right)
            (~a (format-result a-ms) #:min-width 15 #:align 'right)
            (~a (format-result b-ms) #:min-width 15 #:align 'right)
            (~a (format-result (- a-ms b-ms)) #:min-width 15 #:align 'right)
            (~a speedup #:min-width 15 #:align 'right))))

(module+ main
  (run-benchmark))
