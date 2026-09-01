#lang racket

;; rope/benchmarks/incremental-append-bench.rkt
;;
;; Simulates sustained typing: SESSION-LENGTH single-character appends to a
;; growing rope, one at a time. Reports mean/median per-append cost AND the
;; single worst append within each session — the number that predicts
;; whether an editor built on this ever visibly stutters.

(require racket/format
         rope2/generic-ops
         rope2/rope
         rope2/rope-type
         rope2/string-rope)

(define SESSION-LENGTH 20000)
(define TRIALS         5)

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)  (format "~a sec"  (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (median xs)
  (define sorted (sort xs <))
  (list-ref sorted (quotient (length sorted) 2)))

;; One simulated session. Deliberately does NOT collect-garbage inside the
;; loop — GC pauses are part of what a real editor experiences, and hiding
;; them would defeat the point of measuring tail latency.
(define (run-session append-fn len)
  (define-values (_ times)
    (for/fold ([r (make-empty-string-rope)] [times null])
              ([_ (in-range len)])
      (define leaf (make-rope-leaf string "a"))
      (define start (current-inexact-monotonic-milliseconds))
      (define r* (append-fn r leaf))
      (define end (current-inexact-monotonic-milliseconds))
      (values r* (cons (- end start) times))))
  times)

(define strategies
  (list (cons "current (mostly-trigger / strict-prune)"
              (λ (r leaf) (string-rope-append2 r leaf)))
        (cons "eager (check+rebalance every append)"
              (λ (r leaf) (rope-rebalance string (string-rope-concat r leaf))))))

(define (run-benchmark)
  (printf "| ~a | ~a | ~a | ~a |\n"
          (~a "Strategy"     #:min-width 40)
          (~a "mean/append"  #:min-width 12 #:align 'right)
          (~a "median/append" #:min-width 13 #:align 'right)
          (~a "session max"  #:min-width 12 #:align 'right))
  (printf "|-\n")
  (for ([strategy (in-list strategies)])
    (define name (car strategy))
    (define fn   (cdr strategy))
    (run-session fn 2000) ;; JIT warmup, discarded
    (define-values (means* medians* maxes*)
      (for/lists (ms md mx) ([_ (in-range TRIALS)])
        (define times (run-session fn SESSION-LENGTH))
        (values (/ (apply + times) (length times)) (median times) (apply max times))))
    (printf "| ~a | ~a | ~a | ~a |\n"
            (~a name #:min-width 40)
            (~a (format-result (apply min means*))   #:min-width 12 #:align 'right)
            (~a (format-result (apply min medians*)) #:min-width 13 #:align 'right)
            (~a (format-result (apply max maxes*))   #:min-width 12 #:align 'right))))

(module+ main
  (run-benchmark))
