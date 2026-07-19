#lang racket/base

;;; suite.rkt -- the `bench` fixture/op pairing, and how to execute one. Sits
;;; one layer above bench-core.rkt's raw timing primitives.

(require racket/contract
         "bench-core.rkt")

(provide (struct-out bench)
         make-bench
         execute-bench)

;; name             : string, unique within `group` -- shown in reports
;; group            : string, a coarse category for grouping/filtering output
;; size             : exact-nonnegative-integer?, the input size this
;;                    particular bench instance was generated for (0 for
;;                    benches that aren't size-parameterized)
;; fixture-thunk    : (-> fixture) -- builds the input; never timed
;; op               : (fixture -> any) -- the operation actually measured
;; fresh?           : #t if `fixture-thunk` must be re-run before every
;;                    trial (e.g. to keep a memoizing operation honestly
;;                    cold); #f (the default, via `make-bench`) builds the
;;                    fixture once and reuses it warm across all trials.
(struct bench (name group size fixture-thunk op fresh?) #:transparent)

(define/contract (make-bench name group size fixture-thunk op #:fresh? [fresh? #f])
  (->* (string? string? exact-nonnegative-integer? (-> any) (-> any/c any))
       (#:fresh? boolean?)
       bench?)
  (bench name group size fixture-thunk op fresh?))

;; Runs `b`, returning its list of samples. Two execution strategies:
;;
;;  - warm (default): the fixture is built once and `op` is applied to it
;;    `trials` times, after `warmup` untimed priming calls. Use this to
;;    measure steady-state/amortized cost It is needed to see any benefit from
;;    an `eq?`-keyed memoization cache attached to the fixture.
;;
;;  - fresh (`bench-fresh?` is #t): a brand new fixture is built (untimed)
;;    immediately before every measured call, and there is no warmup. Use this
;;    to measure true cold-start cost. Warm execution or a shared fixture
;;    would let a memoization cache mask the cost you're trying to observe.
;;
;; Cold-path measurements are deliberately never batched (see bench-core.rkt's
;; `measure-batch`/`calibrate-reps`). Batching would call `op` on the same
;; fixture more than once, and every call after the first would no longer be
;; cold. `measure-batch` is still used here with reps=1, just to get the
;; higher resolution clock.
(define/contract (execute-bench b #:warmup [warmup 3] #:trials [trials 20]
                                #:gc-between? [gc-between? #t]
                                #:min-batch-ms [min-batch-ms 5.0]
                                #:max-reps [max-reps 1000000])
  (->* (bench?) (#:warmup exact-nonnegative-integer?
                 #:trials exact-nonnegative-integer?
                 #:gc-between? boolean?
                 #:min-batch-ms (and/c real? (not/c negative?))
                 #:max-reps exact-positive-integer?)
       (listof sample?))
  (cond
    [(bench-fresh? b)
     (for/list ([_ (in-range trials)])
       (define fx ((bench-fixture-thunk b)))
       (when gc-between? (collect-garbage))
       (measure-batch (λ () ((bench-op b) fx)) 1))]
    [else
     (define fx ((bench-fixture-thunk b)))
     (run-trials (λ () ((bench-op b) fx))
                 #:warmup warmup #:trials trials #:gc-between? gc-between?
                 #:min-batch-ms min-batch-ms #:max-reps max-reps)]))
