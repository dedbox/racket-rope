#lang racket/base

;; rope/benchmarks/hash-bench.rkt

(require math
         racket/fixnum
         racket/format
         rope2/private/hash)

;; ─────────────────────────────────────────────────────────────────
;; § 3. Corpus — fixed seed ⇒ identical input across every run, so
;;   results are comparable across manual git-tracked edits.
;; ─────────────────────────────────────────────────────────────────

(define CORPUS-SIZE 1000000)
(define CORPUS-SEED 42)

(define (make-corpus size seed)
  (random-seed seed)
  (build-vector size (λ (_) (random M-1))))

(define exponents (make-corpus CORPUS-SIZE CORPUS-SEED))

;; ─────────────────────────────────────────────────────────────────
;; § 4. Timing
;; ─────────────────────────────────────────────────────────────────

(define WARMUP-ITERS 3)
(define TRIALS       20)

;; One full pass over the corpus, folded through fxxor so the
;; result is observably used and cannot be eliminated as dead code.
;; Returns elapsed time in milliseconds (as measured by
;; current-inexact-monotonic-milliseconds).
(define (time-one-pass)
  (define t0 (current-inexact-monotonic-milliseconds))
  (define sink
    (for/fold ([acc 0]) ([p (in-vector exponents)])
      (fxxor acc (equal-hash-code p))))
  (define t1 (current-inexact-monotonic-milliseconds))
  (values (- t1 t0) sink))

(define (run-timing)
  (for ([_ (in-range WARMUP-ITERS)])
    (define-values (_ms sink) (time-one-pass))
    (void sink))
  (collect-garbage)
  (define times
    (for/list ([_ (in-range TRIALS)])
      (define-values (ms sink) (time-one-pass))
      (void sink)
      ms))
  times)

;; ─────────────────────────────────────────────────────────────────
;; § 5. Statistics
;; ─────────────────────────────────────────────────────────────────

;; p99 via nearest-rank on the sorted sample; adequate for the
;; small trial counts used here (no interpolation).
(define (percentile sorted-vals p)
  (define n (length sorted-vals))
  (define idx (max 0 (min (sub1 n) (exact-ceiling (- (* (/ p 100.0) n) 1)))))
  (list-ref sorted-vals idx))

(define (stats times)
  (define sorted (sort times <))
  (define n (length sorted))
  (define total (apply + sorted))
  (values (apply min sorted)             ; min
          (/ total n 1.0)                ; mean
          (apply max sorted)             ; max
          (percentile sorted 99)))       ; p99

;; ─────────────────────────────────────────────────────────────────
;; § 6. Human-readable duration formatting
;;   Input is always in milliseconds; auto-scales to nsec / μsec /
;;   msec / sec depending on magnitude.
;; ─────────────────────────────────────────────────────────────────

(define (format-duration-ms ms)
  (define nsec (* ms 1000000.0))
  (cond
    [(< ms 0.001)   (format "~a nsec" (~r nsec #:precision '(= 1)))]
    [(< ms 1.0)     (format "~a μsec" (~r (* ms 1000.0) #:precision '(= 3)))]
    [(< ms 1000.0)  (format "~a msec" (~r ms #:precision '(= 3)))]
    [else           (format "~a sec"  (~r (/ ms 1000.0) #:precision '(= 3)))]))

;; Per-call time: total pass time divided by corpus size, formatted
;; with finer precision since individual calls are sub-microsecond.
(define (format-per-call-ms pass-ms)
  (format-duration-ms (/ pass-ms CORPUS-SIZE)))

;; ─────────────────────────────────────────────────────────────────
;; § 7. Entry point — prints an Org-mode table to stdout
;; ─────────────────────────────────────────────────────────────────

(module+ main
  (printf "#+CAPTION: fxexpt-M benchmark\n")
  (printf "#+ATTR: corpus=~a exponents, seed=~a, warmup=~a, trials=~a\n"
          CORPUS-SIZE CORPUS-SEED WARMUP-ITERS TRIALS)

  (define times (run-timing))
  (define-values (min-ms mean-ms max-ms p99-ms) (stats times))

  (printf "| metric | per full pass (~a calls) | per call |\n" CORPUS-SIZE)
  (printf "|--------+---------------------------+----------|\n")
  (printf "| min    | ~a | ~a |\n" (format-duration-ms min-ms)  (format-per-call-ms min-ms))
  (printf "| mean   | ~a | ~a |\n" (format-duration-ms mean-ms) (format-per-call-ms mean-ms))
  (printf "| max    | ~a | ~a |\n" (format-duration-ms max-ms)  (format-per-call-ms max-ms))
  (printf "| p99    | ~a | ~a |\n" (format-duration-ms p99-ms)  (format-per-call-ms p99-ms)))
