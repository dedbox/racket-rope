#lang racket/base

(module+ test
  (require rackunit
           racket/vector
           rope/rope
           rope/cursor
           "private/harness.rkt")

  (define ops weighted-ops)

  (define (cursor->vec r)                              ; drain a cursor element-by-element
    (let loop ([c (rope->cursor ops r)] [acc '()])
      (if (cursor-at-end? ops c)
          (list->vector (reverse acc))
          (loop (cursor-advance ops c) (cons (cursor-peek ops c) acc)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Walking a cursor end-to-end reproduces the raw content, across leaf boundaries.
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
                   ([raw (random-weighted-raw (random (* 4 WEIGHTED-LEAF-LIMIT)))])
    (equal? (cursor->vec (raw->rope ops raw)) raw))

  (test-case "cursor-at-end? is false at every position but the last"
    (define raw (random-weighted-raw 40))
    (define r (raw->rope ops raw))
    (for/fold ([c (rope->cursor ops r)]) ([i (in-range (vector-length raw))])
      (check-false (cursor-at-end? ops c))
      (cursor-advance ops c))
    (check-true
     (cursor-at-end? ops (for/fold ([c (rope->cursor ops r)]) ([_ (in-range (vector-length raw))])
                           (cursor-advance ops c)))))

  (test-case "cursor-peek past the end returns #f"
    (define r (make-empty-rope ops))
    (check-false (cursor-peek ops (rope->cursor ops r))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; cursor-drop: O(log n) skip agrees with repeated cursor-advance
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 150
                   ([raw (random-weighted-raw (add1 (random 200)))])
    (define n (vector-length raw))
    (define r (raw->rope ops raw))
    (define k (random (add1 n)))
    (define via-drop (cursor->vec (cursor->rope ops (cursor-drop ops (rope->cursor ops r) k))))
    (define via-step
      (cursor->vec (cursor->rope ops (for/fold ([c (rope->cursor ops r)]) ([_ (in-range k)])
                                        (cursor-advance ops c)))))
    (equal? via-drop via-step))

  (test-case "cursor-drop 0 is the identity"
    (define r (raw->rope ops (random-weighted-raw 20)))
    (check-equal? (cursor->vec (cursor->rope ops (cursor-drop ops (rope->cursor ops r) 0)))
                  (cursor->vec r)))

  (test-case "cursor-drop to the full length lands exactly at end"
    (define raw (random-weighted-raw 20))
    (define r (raw->rope ops raw))
    (check-true (cursor-at-end? ops (cursor-drop ops (rope->cursor ops r) (vector-length raw)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; rope-foldl / rope-foldr
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
                   ([raw (random-weighted-raw (add1 (random 60)))])
    (define r (raw->rope ops raw))
    (define via-foldl (rope-foldl ops (λ (acc x) (cons x acc)) '() r))
    (equal? (reverse via-foldl) (vector->list raw)))

  (check-property #:trials 100
                   ([raw (random-weighted-raw (add1 (random 60)))])
    (define r (raw->rope ops raw))
    (define via-foldr (rope-foldr ops (λ (acc x) (cons x acc)) '() r))
    (equal? via-foldr (vector->list raw)))

  (test-case "multi-rope fold zips elements pairwise"
    (define a (raw->rope ops (vector 1 2 3)))
    (define b (raw->rope ops (vector 10 20 30)))
    (check-equal? (rope-foldl ops (λ (acc x y) (cons (cons x y) acc)) '() a b)
                  (list (cons 3 30) (cons 2 20) (cons 1 10))))

  (test-case "multi-rope fold on mismatched lengths raises"
    (define a (raw->rope ops (vector 1 2 3)))
    (define b (raw->rope ops (vector 1 2)))
    (check-exn exn:fail:contract? (λ () (rope-foldl ops cons '() a b)))))
