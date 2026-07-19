#lang racket/base

;;; report.rkt -- serializing benchmark runs to/from JSON, comparing two runs,
;;; and compiling the results into a regression/improvement report.

(require racket/contract
         racket/format
         racket/list
         json
         "bench-core.rkt"
         "suite.rkt")

(provide (struct-out run-record)
         (struct-out run)
         (struct-out diff-row)
         make-run-record
         make-run
         run->jsexpr
         jsexpr->run
         save-run!
         load-run
         compare-runs
         print-report)

;; ---------------------------------------------------------------------------
;; Records
;; ---------------------------------------------------------------------------

;; Stores one benchmark's statistics within one run, plus (optionally) the
;; boolean result of its last measured call. This is used only by equality
;; benchmarks, to let a comparison flag a semantic change (equal? disagreeing
;; between two runs) distinctly from a performance change.
(struct run-record
  (name group size n mean median stddev min max gc-ms mem-delta result-present? result)
  #:transparent)

;; label           : e.g. "pre-patch" / "post-patch" / a git commit hash
;; timestamp       : (current-seconds) when the run was produced
;; racket-version  : (version), recorded because timing comparisons across
;;                   different Racket builds are not meaningful
(struct run (label timestamp racket-version records) #:transparent)

(define (make-run-record b samples)
  (define st (summarize samples))
  (define final (sample-result (last samples)))
  (define present? (boolean? final))
  (run-record (bench-name b) (bench-group b) (bench-size b)
              (stats-n st) (stats-mean st) (stats-median st) (stats-stddev st)
              (stats-min st) (stats-max st) (stats-total-gc-ms st) (stats-total-mem-delta st)
              present? (and present? final)))

(define (make-run label records)
  (run label (current-seconds) (version) records))

;; ---------------------------------------------------------------------------
;; JSON I/O
;; ---------------------------------------------------------------------------

(define (record->jsexpr r)
  (hasheq 'name           (run-record-name r)
          'group          (run-record-group r)
          'size           (run-record-size r)
          'n              (run-record-n r)
          'mean           (run-record-mean r)
          'median         (run-record-median r)
          'stddev         (run-record-stddev r)
          'min            (run-record-min r)
          'max            (run-record-max r)
          'gc-ms          (run-record-gc-ms r)
          'mem-delta      (run-record-mem-delta r)
          'result-present (run-record-result-present? r)
          'result         (if (run-record-result-present? r) (run-record-result r) 'null)))

(define (jsexpr->record j)
  (run-record (hash-ref j 'name) (hash-ref j 'group)  (hash-ref j 'size)   (hash-ref j 'n)
              (hash-ref j 'mean) (hash-ref j 'median) (hash-ref j 'stddev) (hash-ref j 'min)
              (hash-ref j 'max)  (hash-ref j 'gc-ms)  (hash-ref j 'mem-delta)
              (hash-ref j 'result-present)
              (let ([v (hash-ref j 'result)]) (and (not (eq? v 'null)) v))))

(define (run->jsexpr r)
  (hasheq 'label          (run-label r)
          'timestamp      (run-timestamp r)
          'racket-version (run-racket-version r)
          'records        (map record->jsexpr (run-records r))))

(define (jsexpr->run j)
  (run (hash-ref j 'label) (hash-ref j 'timestamp) (hash-ref j 'racket-version)
       (map jsexpr->record (hash-ref j 'records))))

(define/contract (save-run! r path)
  (-> run? path-string? void?)
  (call-with-output-file path #:exists 'replace
    (λ (out) (write-json (run->jsexpr r) out) (void))))

(define/contract (load-run path)
  (-> path-string? run?)
  (call-with-input-file path (λ (in) (jsexpr->run (read-json in)))))

;; ---------------------------------------------------------------------------
;; Comparison
;; ---------------------------------------------------------------------------

(define (record-key r) (list (run-record-group r) (run-record-name r) (run-record-size r)))

;; status    : 'regression | 'improvement | 'same | 'unknown | 'new | 'removed
;; delta-pct : percent change in mean time, current vs. baseline (#f if
;;             unknown/not applicable, e.g. a 'new or 'removed row)
(struct diff-row
  (group name size status delta-pct baseline-mean current-mean semantic-change?)
  #:transparent)

(define/contract (compare-runs baseline current #:threshold [threshold 10.0])
  (->* (run? run?) (#:threshold real?) (listof diff-row?))
  (define baseline-index
    (for/hash ([r (in-list (run-records baseline))]) (values (record-key r) r)))
  (define current-index
    (for/hash ([r (in-list (run-records current))]) (values (record-key r) r)))
  (define keys (remove-duplicates (append (hash-keys baseline-index) (hash-keys current-index))))
  (for/list ([k (in-list keys)])
    (define b (hash-ref baseline-index k #f))
    (define c (hash-ref current-index k #f))
    (define-values (group name size) (apply values k))
    (cond
      [(not b) (diff-row group name size 'new #f #f (run-record-mean c) #f)]
      [(not c) (diff-row group name size 'removed #f (run-record-mean b) #f #f)]
      [else
       (define bm (run-record-mean b))
       (define cm (run-record-mean c))
       (define delta (if (zero? bm) #f (* 100.0 (/ (- cm bm) bm))))
       (define status
         (cond [(not delta) 'unknown]
               [(> delta threshold) 'regression]
               [(< delta (- threshold)) 'improvement]
               [else 'same]))
       (define semantic?
         (and (run-record-result-present? b) (run-record-result-present? c)
              (not (equal? (run-record-result b) (run-record-result c)))))
       (diff-row group name size status delta bm cm semantic?)])))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(define (status-tag s)
  (case s
    [(regression)  "REGRESSION"]
    [(improvement) "IMPROVED"]
    [(same)        "same"]
    [(new)         "NEW"]
    [(removed)     "REMOVED"]
    [(unknown)     "N/A"]
    [else          "?"]))

(define (row<? a b)
  (or (string<? (diff-row-group a) (diff-row-group b))
      (and (string=? (diff-row-group a) (diff-row-group b))
           (or (string<? (diff-row-name a) (diff-row-name b))
               (and (string=? (diff-row-name a) (diff-row-name b))
                    (< (diff-row-size a) (diff-row-size b)))))))

;; Prints an aligned comparison table to `port` and returns #t if any row is a
;; performance regression or an unexpected semantic change -- suitable for use
;; as a CI pass/fail signal (see main.rkt's exit code).
(define/contract (print-report rows #:port [port (current-output-port)])
  (->* ((listof diff-row?)) (#:port output-port?) boolean?)
  (define sorted (sort rows row<?))
  (for ([r (in-list sorted)])
    (define delta-str
      (if (diff-row-delta-pct r)
          (let ([d (diff-row-delta-pct r)])
            (string-append (if (>= d 0) "+" "") (~r d #:precision 1) "%"))
          "N/A"))
    (fprintf port "~a  ~a  n=~a  ~a=~a~a  [base=~a cur=~a]\n"
             (~a (status-tag (diff-row-status r)) #:min-width 11)
             (~a (diff-row-group r) "/" (diff-row-name r) #:min-width 45)
             (~a (diff-row-size r) #:min-width 6)
             "delta" delta-str
             (if (diff-row-semantic-change? r) "  (SEMANTIC CHANGE)" "")
             (or (and (diff-row-baseline-mean r) (~r (diff-row-baseline-mean r) #:precision 3)) "-")
             (or (and (diff-row-current-mean r) (~r (diff-row-current-mean r) #:precision 3)) "-")))
  (define regressions (filter (λ (r) (eq? (diff-row-status r) 'regression)) sorted))
  (define improvements (filter (λ (r) (eq? (diff-row-status r) 'improvement)) sorted))
  (define semantic (filter diff-row-semantic-change? sorted))
  (fprintf port "\n~a regression(s), ~a improvement(s), ~a semantic change(s), ~a total\n"
           (length regressions) (length improvements) (length semantic) (length sorted))
  (or (pair? regressions) (pair? semantic)))
