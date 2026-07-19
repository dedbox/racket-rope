#lang racket/base

;;; bench-core.rkt -- timing & statistics primitives. Nothing in this module
;;; knows anything about ropes; it only knows how to run a thunk `trials`
;;; times (after `warmup` untimed priming calls) and summarize the results.

(require racket/contract
         racket/list
         racket/math)

(provide
 (struct-out sample)
 (struct-out stats)
 measure-batch
 calibrate-reps
 run-trials
 summarize)

;; One measured invocation of a thunk, already normalized to a *per-call*
;; estimate (see `measure-batch`).
;;   cpu-ms/real-ms/gc-ms : flonum milliseconds
;;   mem-delta            : flonum bytes, (current-memory-use) after minus
;;                          before. Noisy without a full GC on both sides of
;;                          every trial (which `run-trials` does by
;;                          default), but a useful coarse signal, especially
;;                          when comparing the same benchmark across builds.
;;   result               : the thunk's return value from its last call in
;;                          the batch, kept so a caller (e.g. an equality
;;                          benchmark) can record what was computed, not
;;                          just how long it took. Meaningful even when
;;                          batched, since a warm batch calls the same op on
;;                          the same fixture every time.
(struct sample (cpu-ms real-ms gc-ms mem-delta result) #:transparent)

;; Aggregate statistics over a run's `real-ms` samples. `real-ms` (wall time)
;; is used rather than `cpu-ms` because it's what the total gc-ms is already
;; folded into and what a user actually experiences.
(struct stats (n mean median stddev min max total-gc-ms total-mem-delta)
  #:transparent)

;; ---------------------------------------------------------------------------
;; Measurement
;; ---------------------------------------------------------------------------
;;
;; `time-apply` / `current-process-milliseconds` / `current-gc-milliseconds`
;; only resolve to whole milliseconds. Any single call faster than ~1ms is
;; therefore invisible to them: almost every trial reads back exactly 0, with
;; an occasional 1 from scheduling jitter. Thus, "mean" ends up measuring
;; jitter frequency, resulting in values like 0.067ms with most of the
;; underlying samples at 0.
;;
;; The fix:
;;  - Use `current-inexact-monotonic-milliseconds`, which returns a flonum
;;    with sub-millisecond resolution, instead of integer-ms clocks.
;;  - For anything still too fast for that clock's overhead to be negligible
;;    (a single `append1` on a tiny rope can be tens of nanoseconds):
;;      1. Run the thunk in a batch of `reps` back-to-back calls.
;;      2. Time the whole batch once.
;;      3. Divide by `reps` to get a per-call estimate.
;;
;; This is the standard microbenchmarking technique (as used by
;; e.g. Criterion, Google Benchmark) -- a fast constant-time measurement's
;; overhead becomes negligible once amortized over enough repetitions,
;; regardless of the clock's own resolution.

;; Times one call to `(thunk)` per element of a `reps`-long batch, as a single
;; measurement, and returns per-call estimates. `reps` = 1 is a plain,
;; unbatched single-call measurement.
(define/contract (measure-batch thunk reps)
  (-> (-> any) exact-positive-integer? sample?)
  (define mem-before (current-memory-use))
  (define cpu-before (current-process-milliseconds))
  (define gc-before (current-gc-milliseconds))
  (define real-before (current-inexact-monotonic-milliseconds))
  (define last-result (for/fold ([_r (void)]) ([_ (in-range reps)]) (thunk)))
  (define real-after (current-inexact-monotonic-milliseconds))
  (define gc-after (current-gc-milliseconds))
  (define cpu-after (current-process-milliseconds))
  (define mem-after (current-memory-use))
  (define n (* 1.0 reps)) ; forces flonum (rather than exact-rational) division
  (sample (/ (- cpu-after cpu-before) n)
          (/ (- real-after real-before) n)
          (/ (- gc-after gc-before) n)
          (/ (- mem-after mem-before) n)
          last-result))

;; Doubles `reps` until a batch takes at least `min-batch-ms`, so the eventual
;; per-call division isn't itself dominated by clock-resolution or measurement
;; overhead noise. Bails out at `max-reps` regardless (protects against a
;; `thunk` so fast -- or so optimized away -- that the threshold is never
;; reached), and returns immediately at reps=1 for anything already at or
;; above the threshold (typical of anything already costing >= min-batch-ms
;; per call, so no further calibration work is wasted on genuinely slow
;; operations).
(define/contract (calibrate-reps thunk min-batch-ms max-reps)
  (-> (-> any) (and/c real? (not/c negative?)) exact-positive-integer?
      exact-positive-integer?)
  (let loop ([reps 1])
    (define t0 (current-inexact-monotonic-milliseconds))
    (for ([_ (in-range reps)]) (thunk))
    (define elapsed (- (current-inexact-monotonic-milliseconds) t0))
    (cond [(>= elapsed min-batch-ms) reps]
          [(>= reps max-reps) reps]
          [else (loop (* reps 2))])))

(define/contract (run-trials thunk
                             #:warmup [warmup 3]
                             #:trials [trials 20]
                             #:gc-between? [gc-between? #t]
                             #:min-batch-ms [min-batch-ms 5.0]
                             #:max-reps [max-reps 1000000])
  (->* ((-> any))
       (#:warmup exact-nonnegative-integer?
        #:trials exact-nonnegative-integer?
        #:gc-between? boolean?
        #:min-batch-ms (and/c real? (not/c negative?))
        #:max-reps exact-positive-integer?)
       (listof sample?))
  ;; Discard `warmup` invocations. Lets JIT-level inline caches, contract
  ;; wrappers, and any internal memoization settle before anything (including
  ;; calibration) runs.
  (for ([_ (in-range warmup)]) (thunk))
  ;; Calibrate once, up front, and reuse the same batch size for every trial,
  ;; so all `trials` measurements of one benchmark are directly comparable to
  ;; one another.
  (define reps (calibrate-reps thunk min-batch-ms max-reps))
  (for/list ([_ (in-range trials)])
    (when gc-between? (collect-garbage))
    (measure-batch thunk reps)))

(define (mean xs) (/ (exact->inexact (apply + xs)) (max 1 (length xs))))

(define (stddev xs)
  (define n (length xs))
  (cond
    [(< n 2) 0.0]
    [else
     (define m (mean xs))
     (define ss (apply + (map (λ (x) (sqr (- x m))) xs)))
     (sqrt (/ ss (sub1 n)))]))

(define (median xs)
  (define sorted (sort xs <))
  (define n (length sorted))
  (exact->inexact
   (if (odd? n)
       (list-ref sorted (quotient n 2))
       (/ (+ (list-ref sorted (sub1 (quotient n 2)))
             (list-ref sorted (quotient n 2)))
          2))))

(define/contract (summarize samples)
  (-> (non-empty-listof sample?) stats?)
  (define reals (map sample-real-ms samples))
  (define gcs   (map sample-gc-ms samples))
  (define mems  (map sample-mem-delta samples))
  (stats (length samples)
         (mean reals)
         (median reals)
         (stddev reals)
         (apply min reals)
         (apply max reals)
         (apply + gcs)
         (apply + mems)))
