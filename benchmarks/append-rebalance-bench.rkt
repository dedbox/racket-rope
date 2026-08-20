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
;; Benchmarking Apparatus
;; =============================================================================

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)  (format "~a sec" (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (p99 . results)
  (define sorted (sort results <))
  (define i (max 0 (sub1 (inexact->exact (ceiling (* 0.99 (length sorted)))))))
  (list-ref sorted i))

(define (measure-time proc iters trials)
  (define results
    (for/list ([_ (in-range trials)])
      (collect-garbage)
      (collect-garbage)
      (define start (current-inexact-monotonic-milliseconds))
      (for/fold ([acc 0]) ([_ (in-range iters)])
        ;; Extract a cheap fixnum from the struct to prevent dead-code elimination
        (bitwise-xor acc (rope-count (proc))))
      (define end (current-inexact-monotonic-milliseconds))
      (/ (- end start) (exact->inexact iters))))

  (values (apply min results)
          (apply max results)
          (apply p99 results)))

;; =============================================================================
;; Execution & Tabulation
;; =============================================================================

(define BASE-ITERATIONS 5000)
(define TRIALS          5)

(define (run-benchmark)
  (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
          (~a "Ropes" #:min-width 8 #:align 'right)
          (~a "Chunks" #:min-width 8 #:align 'right)
          (~a "A min" #:min-width 15 #:align 'right)
          (~a "B min" #:min-width 15 #:align 'right)
          (~a "Δ min (A - B)" #:min-width 15 #:align 'right)
          (~a "Speedup min (A/B)" #:min-width 15 #:align 'right)
          (~a "A max" #:min-width 15 #:align 'right)
          (~a "B max" #:min-width 15 #:align 'right)
          (~a "Δ max (A - B)" #:min-width 15 #:align 'right)
          (~a "Speedup max (A/B)" #:min-width 15 #:align 'right)
          (~a "A p99" #:min-width 15 #:align 'right)
          (~a "B p99" #:min-width 15 #:align 'right)
          (~a "Δ p99 (A - B)" #:min-width 15 #:align 'right)
          (~a "Speedup p99 (A/B)" #:min-width 15 #:align 'right))
  (printf "|-\n")

  (for* ([ropes  (in-list '(10 100 1000))]
         [chunks (in-list '(1 10 100))])

    ;; 1. Generate the isolated test payload
    (define as (build-list ropes (λ (_) (make-string-rope chunks 512))))

    ;; Dynamically scale iterations inversely to workload size to prevent
    ;; unbounded execution times
    (define active-iters (max 1 (quotient BASE-ITERATIONS (* ropes chunks))))

    ;; 2. Tier-2 JIT Warmup
    (for ([_ (in-range (min active-iters 10000))])
      (rope-append string as)
      (rope-append-rebalance string as))

    ;; 3. Strict Measurement
    (define-values (a-min-ms a-max-ms a-p99-ms)
      (measure-time (λ () (rope-append string as)) active-iters TRIALS))
    (define-values (b-min-ms b-max-ms b-p99-ms)
      (measure-time (λ () (rope-append-rebalance string as)) active-iters TRIALS))

    ;; 4. Statistical Analysis
    (define speedup-min
      (if (zero? b-min-ms)
          "∞x"
          (format "~ax" (real->decimal-string (/ a-min-ms b-min-ms) 2))))
    (define speedup-max
      (if (zero? b-max-ms)
          "∞x"
          (format "~ax" (real->decimal-string (/ a-max-ms b-max-ms) 2))))
    (define speedup-p99
      (if (zero? b-p99-ms)
          "∞x"
          (format "~ax" (real->decimal-string (/ a-p99-ms b-p99-ms) 2))))

    (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
            (~a ropes #:min-width 8 #:align 'right)
            (~a chunks #:min-width 8 #:align 'right)
            (~a (format-result a-min-ms) #:min-width 15 #:align 'right)
            (~a (format-result b-min-ms) #:min-width 15 #:align 'right)
            (~a (format-result (- a-min-ms b-min-ms)) #:min-width 15 #:align 'right)
            (~a speedup-min #:min-width 15 #:align 'right)
            (~a (format-result a-max-ms) #:min-width 15 #:align 'right)
            (~a (format-result b-max-ms) #:min-width 15 #:align 'right)
            (~a (format-result (- a-max-ms b-max-ms)) #:min-width 15 #:align 'right)
            (~a speedup-max #:min-width 15 #:align 'right)
            (~a (format-result a-p99-ms) #:min-width 15 #:align 'right)
            (~a (format-result b-p99-ms) #:min-width 15 #:align 'right)
            (~a (format-result (- a-p99-ms b-p99-ms)) #:min-width 15 #:align 'right)
            (~a speedup-p99 #:min-width 15 #:align 'right))))

(module+ main
  (run-benchmark))
