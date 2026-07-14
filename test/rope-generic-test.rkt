#lang racket/base

(module+ test
  (require rackunit
           racket/vector
           rope/rope
           "./private/harness.rkt")

  (define ops weighted-ops)

  ;;; -------------------------------------------------------------------------------------------
  ;;; Empty rope
  ;;; -------------------------------------------------------------------------------------------
  (test-case "empty rope: zero count/width, empty content"
    (define e (make-empty-rope ops))
    (check-equal? (rope-count e) 0)
    (check-equal? (rope-width e) 0)
    (check-true   (rope-empty? e))
    (check-equal? (weighted->vec e) (vector)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Leaves: count and width are independent axes
  ;;; -------------------------------------------------------------------------------------------
  (test-case "leaf count ≠ leaf width for weighted elements"
    (define raw (vector 1 2 3 4))                       ; count 4, width 10
    (define l (make-rope-leaf ops raw))
    (check-equal? (rope-count l) 4)
    (check-equal? (rope-width l) 10)
    (check-equal? (rope-length l) (rope-width l)))       ; rope-length is defined as rope-width

  ;;; -------------------------------------------------------------------------------------------
  ;;; Append: additive counts/widths, content preserved, invariant maintained
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
                   ([a (random-weighted-raw (random 40))]
                    [b (random-weighted-raw (random 40))])
    (define r (rope-append1 ops (make-rope-leaf ops a) (make-rope-leaf ops b)))
    (and (= (rope-count r) (+ (vector-length a) (vector-length b)))
         (= (rope-width r) (+ (weighted-raw-width a) (weighted-raw-width b)))
         (equal? (weighted->vec r) (vector-append a b))
         (rope-balanced? r)))

  (test-case "append is a content identity on either empty side"
    (define e (make-empty-rope ops))
    (define r (make-rope-leaf ops (vector 1 2 3)))
    (check-equal? (weighted->vec (rope-append1 ops e r)) (weighted->vec r))
    (check-equal? (weighted->vec (rope-append1 ops r e)) (weighted->vec r)))

  (test-case "many sequential appends stay Fibonacci-balanced at every step"
    (void
     (for/fold ([r (make-empty-rope ops)]) ([_ (in-range 800)])
       (define r+ (rope-append1 ops r (make-rope-leaf ops (random-weighted-raw (add1 (random 6))))))
       (check-true (rope-balanced? r+))
       r+)))

  (test-case "rope-append over an empty list yields the empty rope"
    (check-true (rope-empty? (rope-append ops '()))))

  (check-property #:trials 50
                   ([chunks (for/list ([_ (in-range (add1 (random 12)))])
                              (random-weighted-raw (random 20)))])
    (define r (rope-append ops (map (λ (c) (make-rope-leaf ops c)) chunks)))
    (equal? (weighted->vec r) (apply vector-append chunks)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Split: every index in [0, count] partitions content losslessly
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
                   ([raw (random-weighted-raw (add1 (random 200)))])
    (define n (vector-length raw))
    (define r (raw->rope ops raw))
    (for/and ([i (in-range (add1 n))])
      (define-values (l rr) (rope-split ops r i))
      (and (= (rope-count l) i)
           (= (rope-count rr) (- n i))
           (equal? (vector-append (weighted->vec l) (weighted->vec rr)) raw))))

  (test-case "split at 0 / at count produces an empty side"
    (define raw (random-weighted-raw 30))
    (define r (raw->rope ops raw))
    (define-values (l0 r0) (rope-split ops r 0))
    (define-values (ln rn) (rope-split ops r (vector-length raw)))
    (check-true  (rope-empty? l0))
    (check-equal? (weighted->vec r0) raw)
    (check-equal? (weighted->vec ln) raw)
    (check-true  (rope-empty? rn)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Splice / slice against a plain-vector oracle
  ;;; -------------------------------------------------------------------------------------------
  (define (vector-splice v start old-len new)
    (vector-append (vector-copy v 0 start) new (vector-copy v (+ start old-len) (vector-length v))))

  (check-property #:trials 300
                   ([raw (random-weighted-raw (add1 (random 100)))])
    (define n (vector-length raw))
    (define start (random (add1 n)))
    (define old-len (random (add1 (- n start))))
    (define new (random-weighted-raw (random 20)))
    (define r (rope-splice ops (raw->rope ops raw) start old-len new))
    (equal? ((rope-ops-raw-append ops) (rope-flatten r))
            (vector-splice raw start old-len new)))

  ;; (check-property #:trials 300
  ;;                  ([raw (random-weighted-raw (add1 (random 100)))])
  ;;   (define n (vector-length raw))
  ;;   (define start (random (add1 n)))
  ;;   (define len (random (add1 (- n start))))
  ;;   (define slice ((rope-ops-raw-append ops) (rope-slice ops (raw->rope ops raw) start len)))
  ;;   (equal? slice (vector-copy raw start (+ start len))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; rope-offset-index: leftmost element index containing a given width-offset.
  ;;;
  ;;; NOTE: these assert the *documented* contract. As analyzed in the strategy write-up, the
  ;;; current multi-element-leaf clause returns (sub1 i) instead of i, so most of these are
  ;;; expected to FAIL against the present implementation — that is the intended diagnostic value.
  ;;; -------------------------------------------------------------------------------------------
  (define (owning-index raw ofs)                       ; oracle: linear scan
    (let loop ([i 0] [acc 0])
      (define w (vector-ref raw i))
      (if (< ofs (+ acc w)) i (loop (add1 i) (+ acc w)))))

  (check-property #:trials 200
                   ([raw (random-weighted-raw (add1 (random 30)))])
    (define r (raw->rope ops raw))
    (define width (rope-width r))
    (define ofs (random width))
    (= (rope-offset-index ops r ofs) (owning-index raw ofs)))

  (test-case "rope-offset-index at ofs = 0 on a multi-element leaf"
    (define raw (vector 3 1 4 1 5))
    (define r (make-rope-leaf ops raw))
    (check-equal? (rope-offset-index ops r 0) 0))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Conversions: raw->rope / rope->raw round-trip, and balance for large inputs
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
      ([raw (random-weighted-raw (random 500))])
    (define r (raw->rope ops raw))
    (and (equal? (weighted->vec r) raw)
         (rope-balanced? r)))

  (test-case "raw exceeding the leaf limit produces internal structure"
    (define raw (random-weighted-raw (* 3 WEIGHTED-LEAF-LIMIT)))
    (check-true (> (rope-depth (raw->rope ops raw)) 0)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; fib-bound / rope-balanced? at pathological depth
  ;;; -------------------------------------------------------------------------------------------
  (test-case "fib-bound clamps gracefully past its literal table"
    ;; Build a deliberately unbalanced left comb 45 levels deep — deeper than fib-bound's table —
    ;; entirely via naive rope-concat (never rope-append1), so no rebalancing kicks in.
    (define deep
      (for/fold ([r (make-rope-leaf ops (vector 1))]) ([_ (in-range 45)])
        (rope-concat ops r (make-rope-leaf ops (vector 1)))))
    (check-equal? (rope-depth deep) 45)
    (check-not-exn (λ () (rope-balanced? deep)))
    (check-false (rope-balanced? deep)))         ; a 46-leaf left comb is not remotely balanced

  ;;; -------------------------------------------------------------------------------------------
  ;;; Regression Tests
  ;;; -------------------------------------------------------------------------------------------

  (test-case "rope-offset-index no longer crashes at the last valid offset"
    (define raw (vector 3 1 4 1 5))                     ; width 14
    (define r (make-rope-leaf ops raw))
    (check-equal? (rope-offset-index ops r 13) 4)        ; last element, last valid offset
    (check-equal? (rope-offset-index ops r 0)  0))

  (test-case "rope-offset-index clamps when ofs runs past the end"
    (define raw (vector 3 1 4 1 5))
    (define r (make-rope-leaf ops raw))
    (check-not-exn (λ () (rope-offset-index ops r 14)))  ; previously crashed
    (check-equal?  (rope-offset-index ops r 14) 4))

  (test-case "raw->rope balance for empty input"
    (define raw #())
    (define r (raw->rope ops raw))
    (check-true (and (equal? (weighted->vec r) raw) (rope-balanced? r)))))
