#lang racket/base

(module+ test
  (require rackunit
           racket/vector
           rope/rope
           "private/harness.rkt")

  ;;; -------------------------------------------------------------------------------------------
  ;;; ropeable? / raw generics / ctor generics smoke tests
  ;;; -------------------------------------------------------------------------------------------
  (test-case "the witness satisfies ropeable?, arbitrary values do not"
    (check-true  (ropeable? gen))
    (check-false (ropeable? 42))
    (check-false (ropeable? (vector 1 2 3))))          ; a raw is not itself ropeable

  (test-case "raw generics agree with their direct definitions"
    (check-equal? (raw-limit gen) WEIGHTED-LEAF-LIMIT)
    (check-true   (raw? gen (vector 1 2 3)))
    (check-false  (raw? gen "not a vector"))
    (check-equal? (raw-empty gen) (vector))
    (check-equal? (raw-count gen (vector 1 2 3)) 3)
    (check-equal? (raw-width gen (vector 1 2 3)) 6)
    (check-equal? (raw-slice gen (vector 1 2 3 4) 1 3) (vector 2 3))
    (check-equal? (raw-append gen (vector 1 2) (vector 3 4)) (vector 1 2 3 4))
    (check-equal? (raw-ref gen (vector 10 20 30) 1) 20))

  (test-case "rope-leaf-ctor / rope-node-ctor return usable constructors"
    (check-equal? ((rope-leaf-ctor gen) 1 1 (vector 1)) (make-rope-leaf gen (vector 1)))
    (check-true (procedure? (rope-node-ctor gen))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Empty rope
  ;;; -------------------------------------------------------------------------------------------
  (test-case "empty rope: zero count/width, empty content, and is balanced"
    (define e (make-empty-rope gen))
    (check-equal? (rope-count e) 0)
    (check-equal? (rope-width e) 0)
    (check-true   (rope-empty? e))
    (check-true   (rope-balanced? e))
    (check-equal? (weighted->vec e) (vector)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Leaves: count and width are independent axes
  ;;; -------------------------------------------------------------------------------------------
  (test-case "leaf count ≠ leaf width for weighted elements"
    (define raw (vector 1 2 3 4))                       ; count 4, width 10
    (define l (make-rope-leaf gen raw))
    (check-true  (rope-leaf? l))
    (check-equal? (rope-count l) 4)
    (check-equal? (rope-width l) 10)
    (check-equal? (rope-length l) (rope-width l)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; rope-concat / make-rope-node
  ;;; -------------------------------------------------------------------------------------------
  (test-case "rope-concat derives count/width correctly"
    (define l (make-rope-leaf gen (vector 1 2)))
    (define r (make-rope-leaf gen (vector 3 4 5)))
    (define n (rope-concat gen l r))
    (check-true (rope-node? n))
    (check-equal? (rope-count n) 5)
    (check-equal? (rope-width n) 15)
    (check-equal? (weighted->vec n) (vector 1 2 3 4 5)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Append: rope-append1 (binary) and rope-append (variadic)
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
      ([a (random-weighted-raw (random 40))]
       [b (random-weighted-raw (random 40))])
    (define r (rope-append1 gen (make-rope-leaf gen a) (make-rope-leaf gen b)))
    (and (= (rope-count r) (+ (vector-length a) (vector-length b)))
         (= (rope-width r) (+ (weighted-raw-width a) (weighted-raw-width b)))
         (equal? (weighted->vec r) (vector-append a b))
         (rope-balanced? r)))

  (test-case "rope-append1 is a content identity on either empty side"
    (define e (make-empty-rope gen))
    (define r (make-rope-leaf gen (vector 1 2 3)))
    (check-equal? (weighted->vec (rope-append1 gen e r)) (weighted->vec r))
    (check-equal? (weighted->vec (rope-append1 gen r e)) (weighted->vec r)))

  (test-case "rope-append with zero ropes yields the empty rope"
    (check-true (rope-empty? (rope-append gen))))

  (check-property #:trials 50
      ([chunks (for/list ([_ (in-range (add1 (random 12)))])
                 (random-weighted-raw (random 20)))])
    (define r (apply rope-append gen (map (λ (c) (make-rope-leaf gen c)) chunks)))
    (equal? (weighted->vec r) (apply vector-append chunks)))

  (test-case "many sequential appends stay Fibonacci-balanced at every step"
    (void
     (for/fold ([r (make-empty-rope gen)]) ([_ (in-range 800)])
       (define r+ (rope-append1 gen r (make-rope-leaf gen (random-weighted-raw (add1 (random 6))))))
       (check-true (rope-balanced? r+))
       r+)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Split: every index in [0, count] partitions content losslessly
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
      ([raw (random-weighted-raw (add1 (random 200)))])
    (define n (vector-length raw))
    (define r (raw->rope gen raw))
    (for/and ([i (in-range (add1 n))])
      (define-values (l rr) (rope-split gen r i))
      (and (= (rope-count l) i)
           (= (rope-count rr) (- n i))
           (equal? (vector-append (weighted->vec l) (weighted->vec rr)) raw))))

  (test-case "split at 0 / at count produces an empty side"
    (define raw (random-weighted-raw 30))
    (define r (raw->rope gen raw))
    (define-values (l0 r0) (rope-split gen r 0))
    (define-values (ln rn) (rope-split gen r (vector-length raw)))
    (check-true   (rope-empty? l0))
    (check-equal? (weighted->vec r0) raw)
    (check-equal? (weighted->vec ln) raw)
    (check-true   (rope-empty? rn)))

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
    (equal? (weighted->vec (rope-splice gen (raw->rope gen raw) start old-len new))
            (vector-splice raw start old-len new)))

  (check-property #:trials 300
      ([raw (random-weighted-raw (add1 (random 100)))])
    (define n (vector-length raw))
    (define start (random (add1 n)))
    (define len (random (add1 (- n start))))
    (equal? (weighted->vec (rope-slice gen (raw->rope gen raw) start len))
            (vector-copy raw start (+ start len))))

  (test-case "rope-slice/rope-splice at the extremes don't error"
    (define raw (random-weighted-raw 20))
    (define r (raw->rope gen raw))
    (check-not-exn (λ () (rope-slice gen r 0 0)))
    (check-not-exn (λ () (rope-slice gen r (vector-length raw) 0)))
    (check-not-exn (λ () (rope-splice gen r 0 0 (vector))))
    (check-not-exn (λ () (rope-splice gen r 0 (vector-length raw) (vector)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; rope-offset-index — verified correct (fixed several rounds ago, still correct now)
  ;;; -------------------------------------------------------------------------------------------
  (define (owning-index raw ofs)
    (let loop ([i 0] [acc 0])
      (define w (vector-ref raw i))
      (if (< ofs (+ acc w)) i (loop (add1 i) (+ acc w)))))

  (check-property #:trials 200
      ([raw (random-weighted-raw (add1 (random 30)))])
    (define r (raw->rope gen raw))
    (define width (rope-width r))
    (define ofs (random width))
    (= (rope-offset-index gen r ofs) (owning-index raw ofs)))

  (test-case "rope-offset-index at the first, last, and one-past-end offsets"
    (define raw (vector 3 1 4 1 5))                     ; width 14
    (define r (make-rope-leaf gen raw))
    (check-equal? (rope-offset-index gen r 0)  0)
    (check-equal? (rope-offset-index gen r 13) 4)
    (check-not-exn (λ () (rope-offset-index gen r 14)))
    (check-equal?  (rope-offset-index gen r 14) 4))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Conversions and balance under scale
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
      ([raw (random-weighted-raw (random 500))])
    (define r (raw->rope gen raw))
    (and (equal? (weighted->vec r) raw)
         (rope-balanced? r)))

  (test-case "raw exceeding the leaf limit produces internal structure"
    (define raw (random-weighted-raw (* 3 WEIGHTED-LEAF-LIMIT)))
    (check-true (> (rope-depth (raw->rope gen raw)) 0)))

  (test-case "a deep, naively-concatenated comb is correctly reported unbalanced"
    (define deep
      (for/fold ([r (make-rope-leaf gen (vector 1))]) ([_ (in-range 45)])
        (rope-concat gen r (make-rope-leaf gen (vector 1)))))
    (check-equal?  (rope-depth deep) 45)
    (check-not-exn (λ () (rope-balanced? deep)))         ; fib-bound's table clamp doesn't crash
    (check-false   (rope-balanced? deep))))
