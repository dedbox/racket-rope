#lang racket

;; rope/benchmarks/forest-vs-tree-bench.rkt
;;
;; Compares "N sequential tree appends, checked/fixed every step" against
;; "N sequential forest inserts, collapsed once at the end" — the exact
;; shape a real typing session has, since nothing reads the buffer mid-
;; keystroke in either version here.

(require racket/format
         rope2/generic
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

(define (tree-session len)
  (for/fold ([r (make-empty-string-rope)]) ([_ (in-range len)])
    (string-rope-append2 r (make-rope-leaf string "a"))))

(define (forest-session len)
  (rope-forest->rope string
   (for/fold ([f (make-rope-forest)]) ([_ (in-range len)])
     (string-rope-forest-add f (make-rope-leaf string "a")))))

(define (time-it thunk)
  (collect-garbage)
  (define gc0 (current-gc-milliseconds))
  (define start (current-inexact-monotonic-milliseconds))
  (thunk)
  (define end (current-inexact-monotonic-milliseconds))
  (values (- end start) (- (current-gc-milliseconds) gc0)))

(module+ main
  (define _t (tree-session 2000))
  (define _f (forest-session 2000)) ;; warmup
  (for ([label (in-list '("tree (current)" "forest"))]
        [fn    (in-list (list tree-session forest-session))])
    (define-values (times gcs)
      (for/lists (ts gs) ([_ (in-range TRIALS)]) (time-it (λ () (fn SESSION-LENGTH)))))
    (printf "~a: total=~a  gc=~a  per-append=~a\n"
            label
            (format-result (apply min times))
            (format-result (apply min gcs))
            (format-result (/ (apply min times) SESSION-LENGTH)))))
