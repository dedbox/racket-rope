#lang racket/base

(module+ test
  (require rackunit
           racket/vector
           rope/rope
           "private/testing.rkt")

  (define generics-suite
    (test-suite "ropeable generics"
      (test-case "the witness satisfies ropeable?, arbitrary values do not"
        (check-true  (ropeable? weighted))
        (check-false (ropeable? 42))
        (check-false (ropeable? (vector 1 2 3))))

      (test-case "raw generics agree with their direct definitions"
        (check-equal? (raw-limit weighted) WEIGHTED-LEAF-LIMIT)
        (check-true   (raw? weighted (vector 1 2 3)))
        (check-false  (raw? weighted "not a vector"))
        (check-equal? (raw-empty weighted) (vector))
        (check-equal? (raw-count weighted (vector 1 2 3)) 3)
        (check-equal? (raw-width weighted (vector 1 2 3)) 6)
        (check-equal? (raw-slice weighted (vector 1 2 3 4) 1 3) (vector 2 3))
        (check-equal? (raw-append weighted (vector 1 2) (vector 3 4)) (vector 1 2 3 4))
        (check-equal? (raw-ref weighted (vector 10 20 30) 1) 20))

      (test-case "rope-leaf-ctor / rope-node-ctor return usable constructors"
        (check-equal? ((rope-leaf-ctor weighted) 1 1 (vector 1)) (make-rope-leaf weighted (vector 1)))
        (check-true (procedure? (rope-node-ctor weighted))))))

  (define structure-suite
    (test-suite "empty rope, leaves, concat"
      (test-case "empty rope: zero count/width, empty content, and balanced"
        (define e (make-empty-rope weighted))
        (check-equal? (rope-count e) 0)
        (check-equal? (rope-width e) 0)
        (check-true   (rope-empty? e))
        (check-true   (rope-balanced? e))
        (check-equal? (weighted->vec e) (vector)))

      (test-case "leaf count ≠ leaf width for weighted elements"
        (define l (make-rope-leaf weighted (vector 1 2 3 4)))   ; count 4, width 10
        (check-true   (rope-leaf? l))
        (check-equal? (rope-count l) 4)
        (check-equal? (rope-width l) 10)
        (check-equal? (rope-length l) (rope-width l)))

      (test-case "rope-concat derives count/width correctly"
        (define n (rope-concat weighted (make-rope-leaf weighted (vector 1 2))
                               (make-rope-leaf weighted (vector 3 4 5))))
        (check-true   (rope-node? n))
        (check-equal? (rope-count n) 5)
        (check-equal? (rope-width n) 15)
        (check-equal? (weighted->vec n) (vector 1 2 3 4 5)))))

  (define append-suite
    (test-suite "append"
      (test-property "rope-append1 agrees with a vector-append oracle, stays balanced" #:trials 300
                     ([a (random-weighted-raw (random 40))]
                      [b (random-weighted-raw (random 40))])
                     (define r (rope-append1 weighted (make-rope-leaf weighted a) (make-rope-leaf weighted b)))
                     (and (= (rope-count r) (+ (vector-length a) (vector-length b)))
                          (= (rope-width r) (+ (weighted-raw-width a) (weighted-raw-width b)))
                          (equal? (weighted->vec r) (vector-append a b))
                          (rope-balanced? r)))

      (test-case "rope-append1 is a content identity on either empty side"
        (define e (make-empty-rope weighted))
        (define r (make-rope-leaf weighted (vector 1 2 3)))
        (check-equal? (weighted->vec (rope-append1 weighted e r)) (weighted->vec r))
        (check-equal? (weighted->vec (rope-append1 weighted r e)) (weighted->vec r)))

      (test-case "rope-append with zero ropes yields the empty rope"
        (check-true (rope-empty? (rope-append weighted))))

      (test-property "variadic rope-append agrees with vector-append across N chunks" #:trials 50
                     ([chunks (for/list ([_ (in-range (add1 (random 12)))]) (random-weighted-raw (random 20)))])
                     (define r (apply rope-append weighted (map (λ (c) (make-rope-leaf weighted c)) chunks)))
                     (equal? (weighted->vec r) (apply vector-append chunks)))

      (test-case "many sequential appends stay Fibonacci-balanced at every step"
        (for/fold ([r (make-empty-rope weighted)]) ([_ (in-range 800)])
          (define r+ (rope-append1 weighted r (make-rope-leaf weighted (random-weighted-raw (add1 (random 6))))))
          (check-true (rope-balanced? r+))
          r+)
        (void))))

  (define split-suite
    (test-suite "split"
      (test-property "every index in [0, count] partitions content losslessly" #:trials 300
                     ([raw (random-weighted-raw (add1 (random 200)))])
                     (define n (vector-length raw))
                     (define r (raw->rope weighted raw))
                     (for/and ([i (in-range (add1 n))])
                       (define-values (l rr) (rope-split weighted r i))
                       (and (= (rope-count l) i)
                            (= (rope-count rr) (- n i))
                            (equal? (vector-append (weighted->vec l) (weighted->vec rr)) raw))))

      (test-case "split at 0 / at count produces an empty side"
        (define raw (random-weighted-raw 30))
        (define r (raw->rope weighted raw))
        (define-values (l0 r0) (rope-split weighted r 0))
        (define-values (ln rn) (rope-split weighted r (vector-length raw)))
        (check-true   (rope-empty? l0))
        (check-equal? (weighted->vec r0) raw)
        (check-equal? (weighted->vec ln) raw)
        (check-true   (rope-empty? rn)))))

  (define splice/slice-suite
    (let ()
      (define (vector-splice v start old-len new)
        (vector-append (vector-copy v 0 start) new (vector-copy v (+ start old-len) (vector-length v))))
      (test-suite "splice / slice"
        (test-property "rope-splice matches a vector oracle" #:trials 300
                       ([raw (random-weighted-raw (add1 (random 100)))])
                       (define n (vector-length raw))
                       (define start (random (add1 n)))
                       (define old-len (random (add1 (- n start))))
                       (define new (random-weighted-raw (random 20)))
                       (equal? (weighted->vec (rope-splice weighted (raw->rope weighted raw) start old-len new))
                               (vector-splice raw start old-len new)))

        (test-property "rope-slice matches a vector oracle" #:trials 300
                       ([raw (random-weighted-raw (add1 (random 100)))])
                       (define n (vector-length raw))
                       (define start (random (add1 n)))
                       (define len (random (add1 (- n start))))
                       (equal? (weighted->vec (rope-slice weighted (raw->rope weighted raw) start len))
                               (vector-copy raw start (+ start len))))

        (test-case "rope-slice/rope-splice at the extremes don't error"
          (define raw (random-weighted-raw 20))
          (define r (raw->rope weighted raw))
          (check-not-exn (λ () (rope-slice weighted r 0 0)))
          (check-not-exn (λ () (rope-slice weighted r (vector-length raw) 0)))
          (check-not-exn (λ () (rope-splice weighted r 0 0 (vector))))
          (check-not-exn (λ () (rope-splice weighted r 0 (vector-length raw) (vector))))))))

  (define offset-index-suite
    (let ()
      (define (owning-index raw ofs)
        (let loop ([i 0] [acc 0])
          (define w (vector-ref raw i))
          (if (< ofs (+ acc w)) i (loop (add1 i) (+ acc w)))))
      (test-suite "rope-offset-index"
        (test-property "matches a linear-scan oracle" #:trials 200
                       ([raw (random-weighted-raw (add1 (random 30)))])
                       (define r (raw->rope weighted raw))
                       (define ofs (random (rope-width r)))
                       (= (rope-offset-index weighted r ofs) (owning-index raw ofs)))

        (test-case "at the first, last, and one-past-end offsets"
          (define raw (vector 3 1 4 1 5))                        ; width 14
          (define r (make-rope-leaf weighted raw))
          (check-equal? (rope-offset-index weighted r 0)  0)
          (check-equal? (rope-offset-index weighted r 13) 4)
          (check-not-exn (λ () (rope-offset-index weighted r 14)))
          (check-equal?  (rope-offset-index weighted r 14) 4)))))

  (define balance-suite
    (test-suite "conversions and balance under scale"
      (test-property "raw->rope round-trips and stays balanced" #:trials 100
                     ([raw (random-weighted-raw (random 500))])
                     (define r (raw->rope weighted raw))
                     (and (equal? (weighted->vec r) raw) (rope-balanced? r)))

      (test-case "raw exceeding the leaf limit produces internal structure"
        (define raw (random-weighted-raw (* 3 WEIGHTED-LEAF-LIMIT)))
        (check-true (> (rope-depth (raw->rope weighted raw)) 0)))

      (test-case "a deep, naively-concatenated comb is correctly reported unbalanced"
        (define deep
          (for/fold ([r (make-rope-leaf weighted (vector 1))]) ([_ (in-range 45)])
            (rope-concat weighted r (make-rope-leaf weighted (vector 1)))))
        (check-equal?  (rope-depth deep) 45)
        (check-not-exn (λ () (rope-balanced? deep)))              ; fib-bound's table clamp doesn't crash
        (check-false   (rope-balanced? deep)))))

  (define cursor-suite
    (let ()
      (define (cursor->vec r)
        (let loop ([c (rope->cursor weighted r)] [acc '()])
          (if (cursor-at-end? weighted c)
              (list->vector (reverse acc))
              (loop (cursor-advance weighted c) (cons (cursor-peek weighted c) acc)))))
      (test-suite "cursors"
        (test-property "walking a cursor end-to-end reproduces raw content" #:trials 100
                       ([raw (random-weighted-raw (random (* 4 WEIGHTED-LEAF-LIMIT)))])
                       (equal? (cursor->vec (raw->rope weighted raw)) raw))

        (test-case "cursor-at-end? is false at every position but the last"
          (define raw (random-weighted-raw 40))
          (define r (raw->rope weighted raw))
          (for/fold ([c (rope->cursor weighted r)]) ([_ (in-range (vector-length raw))])
            (check-false (cursor-at-end? weighted c))
            (cursor-advance weighted c))
          (check-true (cursor-at-end?
                       weighted
                       (for/fold ([c (rope->cursor weighted r)]) ([_ (in-range (vector-length raw))])
                         (cursor-advance weighted c)))))

        (test-case "cursor-peek past the end returns #f, for both an empty rope and a drained one"
          (check-false (cursor-peek weighted (rope->cursor weighted (make-empty-rope weighted))))
          (define raw (random-weighted-raw 20))
          (define r (raw->rope weighted raw))
          (check-false (cursor-peek weighted (cursor-drop weighted (rope->cursor weighted r)
                                                          (vector-length raw)))))

        (test-case "cursor? predicate and field accessors are usable"
          (define c (rope->cursor weighted (raw->rope weighted (vector 1 2 3))))
          (check-true (cursor? c))
          (check-true (rope? (cursor-after c))))

        (test-property "cursor-drop agrees with repeated cursor-advance" #:trials 150
                       ([raw (random-weighted-raw (add1 (random 200)))])
                       (define n (vector-length raw))
                       (define r (raw->rope weighted raw))
                       (define k (random (add1 n)))
                       (define via-drop (cursor->vec (cursor->rope weighted (cursor-drop weighted (rope->cursor weighted r) k))))
                       (define via-step
                         (cursor->vec (cursor->rope weighted (for/fold ([c (rope->cursor weighted r)]) ([_ (in-range k)])
                                                               (cursor-advance weighted c)))))
                       (equal? via-drop via-step))

        (test-case "cursor-drop 0 is identity; cursor-drop to full length lands exactly at end"
          (define raw (random-weighted-raw 20))
          (define r (raw->rope weighted raw))
          (check-equal? (cursor->vec (cursor->rope weighted (cursor-drop weighted (rope->cursor weighted r) 0)))
                        (cursor->vec r))
          (check-true (cursor-at-end? weighted (cursor-drop weighted (rope->cursor weighted r) (vector-length raw)))))

        (test-property "cursor-take agrees with rope-slice from a cursor's position" #:trials 150
                       ([raw (random-weighted-raw (add1 (random 200)))])
                       (define n (vector-length raw))
                       (define r (raw->rope weighted raw))
                       (define start (random (add1 n)))
                       (define k (random (add1 (- n start))))
                       (define cur (cursor-drop weighted (rope->cursor weighted r) start))
                       (equal? (weighted->vec (cursor-take weighted cur k)) (vector-copy raw start (+ start k))))

        (test-case "cursor-take 0 is empty; cursor-take-all equals cursor->rope"
          (define raw (random-weighted-raw 20))
          (define r (raw->rope weighted raw))
          (check-true (rope-empty? (cursor-take weighted (rope->cursor weighted r) 0)))
          (define cur (rope->cursor weighted r))
          (check-equal? (weighted->vec (cursor-take weighted cur (vector-length raw)))
                        (weighted->vec (cursor->rope weighted cur))))

        (test-case "cursor-take k then cursor-drop k reconstructs the original content"
          (define raw (random-weighted-raw 40))
          (define r (raw->rope weighted raw))
          (define k (random (add1 (vector-length raw))))
          (define cur (rope->cursor weighted r))
          (check-equal? (vector-append (weighted->vec (cursor-take weighted cur k))
                                       (weighted->vec (cursor->rope weighted (cursor-drop weighted cur k))))
                        raw)))))

  (define fold-suite
    (test-suite "rope-foldl / rope-foldr"
      (test-property "rope-foldl reverses onto a list, like foldl" #:trials 100
                     ([raw (random-weighted-raw (add1 (random 60)))])
                     (equal? (reverse (rope-foldl weighted (λ (acc x) (cons x acc)) '() (raw->rope weighted raw)))
                             (vector->list raw)))

      (test-property "rope-foldr matches foldr" #:trials 100
                     ([raw (random-weighted-raw (add1 (random 60)))])
                     (equal? (rope-foldr weighted (λ (acc x) (cons x acc)) '() (raw->rope weighted raw))
                             (vector->list raw)))

      (test-case "multi-rope fold zips elements pairwise"
        (define a (raw->rope weighted (vector 1 2 3)))
        (define b (raw->rope weighted (vector 10 20 30)))
        (check-equal? (rope-foldl weighted (λ (acc x y) (cons (cons x y) acc)) '() a b)
                      (list (cons 3 30) (cons 2 20) (cons 1 10))))

      (test-case "multi-rope fold on mismatched lengths raises"
        (define a (raw->rope weighted (vector 1 2 3)))
        (define b (raw->rope weighted (vector 1 2)))
        (check-exn exn:fail:contract? (λ () (rope-foldl weighted cons '() a b))))))

  (define equality/ordering-suite
    (test-suite "equal?/hash and ordering fallbacks"
      (test-case "structural fallback: untagged leaves compare by raw content"
        (check-equal? (make-rope-leaf weighted (vector 1)) ((rope-leaf-ctor weighted) 1 1 (vector 1))))

      (test-case "structural fallback: same-shape nodes with equal children are equal"
        (define mk (λ (v) (make-rope-leaf weighted v)))
        (check-equal? (rope-concat weighted (mk (vector 1)) (mk (vector 2)))
                      (rope-concat weighted (mk (vector 1)) (mk (vector 2)))))

      (test-case "structural fallback does NOT see across leaf/node shape differences"
        ;; Unlike define-rope-type's content-based override, the fallback is honestly
        ;; structural: a leaf and a differently-shaped node holding the same elements are
        ;; not required to compare equal.
        (define as-leaf (make-rope-leaf weighted (vector 1 2)))
        (define as-node (rope-append1 weighted (make-rope-leaf weighted (vector 1))
                                      (make-rope-leaf weighted (vector 2))))
        (check-false (equal? as-leaf as-node)))

      (test-case "rope-compare / rope<? / rope<=? / rope>? / rope>=? agree with a raw-compare oracle"
        (define a (raw->rope weighted (vector 1 2 3)))
        (define b (raw->rope weighted (vector 1 2 4)))
        (check-eq? (rope-compare weighted a a) '=)
        (check-eq? (rope-compare weighted a b) '<)
        (check-true  (rope<? weighted a b))
        (check-false (rope>? weighted a b))
        (check-true  (rope<=? weighted a a))
        (check-true  (rope>=? weighted b a)))

      (test-case "rope-compare-with accepts an alternate comparator"
        (define a (raw->rope weighted (vector 1 2 3)))
        (define b (raw->rope weighted (vector 1 2 3)))
        (check-eq? (rope-compare-with weighted (λ (x y) (weighted-raw-compare x y)) a b) '=))))

  (run-suite!
   (test-suite "rope.rkt"
     generics-suite structure-suite append-suite split-suite splice/slice-suite
     offset-index-suite balance-suite cursor-suite fold-suite equality/ordering-suite)))
