#lang racket/base

(module+ test
  (require rackunit
           racket/vector
           rope/rope
           "private/harness.rkt")

  (define (cursor->vec r)
    (let loop ([c (rope->cursor gen r)] [acc '()])
      (if (cursor-at-end? gen c)
          (list->vector (reverse acc))
          (loop (cursor-advance gen c) (cons (cursor-peek gen c) acc)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Walking a cursor end-to-end reproduces the raw content, across leaf boundaries.
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
      ([raw (random-weighted-raw (random (* 4 WEIGHTED-LEAF-LIMIT)))])
    (equal? (cursor->vec (raw->rope gen raw)) raw))

  (test-case "cursor-at-end? is false at every position but the last"
    (define raw (random-weighted-raw 40))
    (define r (raw->rope gen raw))
    (for/fold ([c (rope->cursor gen r)]) ([i (in-range (vector-length raw))])
      (check-false (cursor-at-end? gen c))
      (cursor-advance gen c))
    (check-true
     (cursor-at-end? gen (for/fold ([c (rope->cursor gen r)]) ([_ (in-range (vector-length raw))])
                           (cursor-advance gen c)))))

  (test-case "cursor-peek past the end returns #f"
    (check-false (cursor-peek gen (rope->cursor gen (make-empty-rope gen)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; cursor-peek: #f is also returned once a *non-empty* rope's cursor is driven to its end,
  ;;; not only for a cursor built on an already-empty rope.
  ;;; -------------------------------------------------------------------------------------------
  (test-case "cursor-peek past the end of a non-empty rope, reached via cursor-drop, returns #f"
    (define raw (random-weighted-raw 20))
    (define r (raw->rope gen raw))
    (check-false (cursor-peek gen (cursor-drop gen (rope->cursor gen r) (vector-length raw)))))

  (test-case "cursor? predicate and field accessors are usable"
    (define c (rope->cursor gen (raw->rope gen (vector 1 2 3))))
    (check-true (cursor? c))
    (check-true (rope? (cursor-after c))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; cursor-drop: O(log n) skip agrees with repeated cursor-advance
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 150
      ([raw (random-weighted-raw (add1 (random 200)))])
    (define n (vector-length raw))
    (define r (raw->rope gen raw))
    (define k (random (add1 n)))
    (define via-drop (cursor->vec (cursor->rope gen (cursor-drop gen (rope->cursor gen r) k))))
    (define via-step
      (cursor->vec (cursor->rope gen (for/fold ([c (rope->cursor gen r)]) ([_ (in-range k)])
                                       (cursor-advance gen c)))))
    (equal? via-drop via-step))

  (test-case "cursor-drop 0 is the identity"
    (define r (raw->rope gen (random-weighted-raw 20)))
    (check-equal? (cursor->vec (cursor->rope gen (cursor-drop gen (rope->cursor gen r) 0)))
                  (cursor->vec r)))

  (test-case "cursor-drop to the full length lands exactly at end"
    (define raw (random-weighted-raw 20))
    (define r (raw->rope gen raw))
    (check-true (cursor-at-end? gen (cursor-drop gen (rope->cursor gen r) (vector-length raw)))))
  ;;; -------------------------------------------------------------------------------------------
  ;;; cursor-take: O(log n) slice agrees with rope-slice from a cursor's position
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 150
      ([raw (random-weighted-raw (add1 (random 200)))])
    (define n (vector-length raw))
    (define r (raw->rope gen raw))
    (define start (random (add1 n)))
    (define k (random (add1 (- n start))))
    (define cur (cursor-drop gen (rope->cursor gen r) start))
    (equal? (weighted->vec (cursor-take gen cur k))
            (vector-copy raw start (+ start k))))

  (test-case "cursor-take 0 is the empty rope"
    (define r (raw->rope gen (random-weighted-raw 20)))
    (check-true (rope-empty? (cursor-take gen (rope->cursor gen r) 0))))

  (test-case "cursor-take to the full remaining length equals cursor->rope"
    (define raw (random-weighted-raw 20))
    (define r (raw->rope gen raw))
    (define cur (rope->cursor gen r))
    (check-equal? (weighted->vec (cursor-take gen cur (vector-length raw)))
                  (weighted->vec (cursor->rope gen cur))))

  (test-case "cursor-take k then cursor-drop k reconstructs the original content"
    (define raw (random-weighted-raw 40))
    (define r (raw->rope gen raw))
    (define k (random (add1 (vector-length raw))))
    (define cur (rope->cursor gen r))
    (check-equal? (vector-append (weighted->vec (cursor-take gen cur k))
                                  (weighted->vec (cursor->rope gen (cursor-drop gen cur k))))
                  raw))

  ;;; -------------------------------------------------------------------------------------------
  ;;; rope-foldl / rope-foldr
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
      ([raw (random-weighted-raw (add1 (random 60)))])
    (equal? (reverse (rope-foldl gen (λ (acc x) (cons x acc)) '() (raw->rope gen raw)))
            (vector->list raw)))

  (check-property #:trials 100
      ([raw (random-weighted-raw (add1 (random 60)))])
    (equal? (rope-foldr gen (λ (acc x) (cons x acc)) '() (raw->rope gen raw))
            (vector->list raw)))

  (test-case "multi-rope fold zips elements pairwise"
    (define a (raw->rope gen (vector 1 2 3)))
    (define b (raw->rope gen (vector 10 20 30)))
    (check-equal? (rope-foldl gen (λ (acc x y) (cons (cons x y) acc)) '() a b)
                  (list (cons 3 30) (cons 2 20) (cons 1 10))))

  (test-case "multi-rope fold on mismatched lengths raises"
    (define a (raw->rope gen (vector 1 2 3)))
    (define b (raw->rope gen (vector 1 2)))
    (check-exn exn:fail:contract? (λ () (rope-foldl gen cons '() a b)))))
