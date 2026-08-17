#lang racket

(require racket/fixnum
         racket/format)

;; =============================================================================
;; Target Functions
;; =============================================================================

(define (loop-test elem-size-fn pos)
  (let loop ([p pos] [i 0])
    (define k (elem-size-fn i))
    (if (fx< p k) i (loop (fx- p k) (fx+ i 1)))))

(define (quotient-test elem-size-fn pos)
  (fxquotient pos (elem-size-fn 0)))

;; =============================================================================
;; Benchmarking Apparatus
;; =============================================================================

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)
     (format "~a sec" (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)
     (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3)
     (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else
     (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (measure-time proc iters trials)
  ;; Execute the block `trials` times and select the lowest duration
  (apply min
         (for/list ([_ (in-range trials)])
           (collect-garbage)
           (collect-garbage)
           (define start (current-inexact-monotonic-milliseconds))
           (for/fold ([acc 0]) ([_ (in-range iters)])
             (bitwise-xor acc (proc)))
           (define end (current-inexact-monotonic-milliseconds))
           (/ (- end start) (exact->inexact iters)))))

;; =============================================================================
;; Experiment Execution
;; =============================================================================

(define ITERATIONS 10000000)
(define TRIALS     5)

(define (run-benchmark)
  (displayln "| Elem Size | Pos | Ratio | Loop Time | Quotient Time | Δ (Loop - Quot) |")
  (displayln "|-----------+-----+-------+-----------+---------------+-----------------|")
  
  (for* ([elem-size (in-list '(1 10 100))]
         [ratio     (in-list '(0 1 10 100))])
    
    (define pos (* elem-size ratio))
    (define (elem-size-fn _) elem-size)

    (for ([_ (in-range 10000)])
      (loop-test elem-size-fn pos)
      (quotient-test elem-size-fn pos))

    (define loop-ms (measure-time (λ () (loop-test elem-size-fn pos)) ITERATIONS TRIALS))
    (define quot-ms (measure-time (λ () (quotient-test elem-size-fn pos)) ITERATIONS TRIALS))

    (printf "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
            (~a elem-size #:min-width 9)
            (~a pos #:min-width 3)
            (~a ratio #:min-width 5)
            (~a (format-result loop-ms) #:min-width 9)
            (~a (format-result quot-ms) #:min-width 13)
            (~a (format-result (abs (- loop-ms quot-ms))) #:min-width 15))))

(module+ main
  (run-benchmark))
