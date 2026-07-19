#lang racket/base

;;; main.rkt -- CLI entry point for the rope benchmark suite.
;;;
;;;   racket benchmark/main.rkt [options]
;;;
;;; Typical A/B workflow for evaluating an implementation change:
;;;
;;;   racket benchmark/main.rkt --label before --save /tmp/before.json
;;;   ... apply the change under test ...
;;;   racket benchmark/main.rkt --label after --baseline /tmp/before.json \
;;;                              --save /tmp/after.json
;;;
;;; The second invocation prints a regression/improvement report and exits
;;; non-zero if anything regressed by more than --threshold percent, or if any
;;; equality/hash scenario's boolean result changed between the two runs
;;; (flagged as a semantic change, not a perf regression.

(require racket/cmdline
         racket/format
         racket/string
         "bench-core.rkt"
         "suite.rkt"
         "report.rkt"
         "suites/all.rkt")

(define default-sizes (list 0 8 64 512 4096 32768))

(define (parse-sizes str)
  (map (λ (s) (or (string->number (string-trim s))
                  (error 'rope-benchmarks "not a number in --sizes: ~a" s)))
       (string-split str ",")))

(define (run-all #:sizes sizes #:trials trials #:warmup warmup #:gc-between? gc-between?
                 #:min-batch-ms min-batch-ms #:suite-names names #:quiet? quiet?)
  (define wanted (filter (λ (p) (member (car p) names)) suite-registry))
  (when (null? wanted)
    (error 'rope-benchmarks "no matching suites among: ~a (available: ~a)"
           (string-join names ", ") (string-join suite-names ", ")))
  (apply append
         (for/list ([entry (in-list wanted)])
           (define this-suite-name (car entry))
           (define benches ((cdr entry) sizes))
           (for/list ([b (in-list benches)])
             (unless quiet?
               (eprintf "  ~a :: ~a/~a (n=~a)\n"
                        this-suite-name (bench-group b) (bench-name b) (bench-size b)))
             (define samples
               (execute-bench b #:warmup warmup #:trials trials #:gc-between? gc-between?
                              #:min-batch-ms min-batch-ms))
             (make-run-record b samples)))))

(define (print-plain-results records)
  (define (row<? a b)
    (or (string<? (run-record-group a) (run-record-group b))
        (and (string=? (run-record-group a) (run-record-group b))
             (string<? (run-record-name a) (run-record-name b)))))
  (for ([r (in-list (sort records row<?))])
    (printf "~a  n=~a  mean=~ams  median=~ams  sd=~ams  gc=~ams\n"
            (~a (run-record-group r) "/" (run-record-name r) #:min-width 45)
            (~a (run-record-size r) #:min-width 6)
            (~r (run-record-mean r) #:precision 3)
            (~r (run-record-median r) #:precision 3)
            (~r (run-record-stddev r) #:precision 3)
            (run-record-gc-ms r))))

(module+ main
  ;; CLI-parsing state: the one deliberately, narrowly imperative corner of
  ;; this codebase, matching racket/cmdline's own documented idiom.
  (define sizes default-sizes)
  (define trials 15)
  (define warmup 5)
  (define suite-names-opt #f) ; #f means "all"
  (define label "run")
  (define save-path #f)
  (define baseline-path #f)
  (define threshold 10.0)
  (define quiet? #f)
  (define gc-between? #t)
  (define min-batch-ms 5.0)

  (command-line
   #:program "rope-benchmarks"
   #:once-each
   [("--sizes")
    s ((~a "Comma-separated input sizes (default: "
           (string-join (map number->string default-sizes) ",")
           ")"))
    (set! sizes (parse-sizes s))]
   [("--trials")
    t "Measured trials per benchmark (default: 15)"
    (set! trials (string->number t))]
   [("--warmup")
    w "Untimed warmup calls per benchmark (default: 5)"
    (set! warmup (string->number w))]
   [("--suites")
    names ("Comma-separated suite names to run (default: all)."
           (~a "Available: " (string-join suite-names ", ")))
    (set! suite-names-opt (string-split names ","))]
   [("--label")
    l "Label recorded with this run (default: \"run\")"
    (set! label l)]
   [("--save")
    path "Write this run's results to PATH as JSON"
    (set! save-path path)]
   [("--baseline")
    path "Compare this run against a previously --save'd JSON run"
    (set! baseline-path path)]
   [("--threshold")
    pct "Percent mean-time increase counted as a regression (default: 10)"
    (set! threshold (string->number pct))]
   [("--no-gc")
    "Skip forcing a GC before each trial (faster, noisier)"
    (set! gc-between? #f)]
   [("--min-batch-ms")
    ms ("Calibrate each warm benchmark's inner repeat count so one measured "
        "batch takes at least this many ms (default: 5). Raise it for less "
        "noisy results on very fast operations; lower it for a quicker, "
        "coarser run.")
    (set! min-batch-ms (string->number ms))]
   [("--quiet")
    "Suppress per-benchmark progress lines"
    (set! quiet? #t)])

  (define names (or suite-names-opt suite-names))
  (define records (run-all #:sizes sizes #:trials trials #:warmup warmup #:gc-between? gc-between?
                           #:min-batch-ms min-batch-ms #:suite-names names #:quiet? quiet?))
  (define this-run (make-run label records))

  (when save-path (save-run! this-run save-path))

  (define failed?
    (if baseline-path
        (print-report (compare-runs (load-run baseline-path) this-run #:threshold threshold))
        (begin (print-plain-results records) #f)))

  (exit (if failed? 1 0)))
