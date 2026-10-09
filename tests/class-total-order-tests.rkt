#lang racket/base

;; Own throwaway type (vector-of-integers, naturally ordered via
;; </>), independent of any production type -- keeps this suite's
;; success independent of whether e.g. type/string.rkt's own wiring is
;; correct.

(module+ test
  (require rope/class/total-order
           rope/generic
           rope/instance
           "../private/testing.rkt")

  (define-test-rope-type oty #:chunk-limit 4)
  (define-rope-instance total-order oty ([elem<? <] [elem>? >]))

  (define-test-rope-type oty2 #:chunk-limit 4)
  (define (custom-overlap c d ic id k)
    (let loop ([i 0])
      (cond [(= i k) '=]
            [(< (vector-ref c (+ ic i)) (vector-ref d (+ id i))) '<]
            [(> (vector-ref c (+ ic i)) (vector-ref d (+ id i))) '>]
            [else (loop (add1 i))])))
  (define-rope-instance total-order oty2
    ([elem<? <] [elem>? >] [chunk-compare-overlap custom-overlap]))

  (define (v->rope mk v) (mk (list->vector v)))

  (define (compare-suite-for name mk rope<? rope>? rope<=? rope>=? rope-compare)
    (test-suite name
      (test-case "equal, less, greater"
        (define a (v->rope mk '(1 2 3)))
        (define b (v->rope mk '(1 2 4)))
        (define c (v->rope mk '(1 2 3)))
        (check-eq? (rope-compare a b) '<)
        (check-eq? (rope-compare b a) '>)
        (check-eq? (rope-compare a c) '=)
        (check-true  (rope<? a b))
        (check-false (rope<? b a))
        (check-true  (rope>? b a))
        (check-true  (rope<=? a c))
        (check-true  (rope>=? a c))
        (check-false (rope<=? b a)))

      (test-case "unequal length, shared prefix"
        ;; the one case a per-chunk/single-element comparison could get
        ;; right while still getting the overall order wrong
        (define short (v->rope mk '(1 2)))
        (define long  (v->rope mk '(1 2 3)))
        (check-eq? (rope-compare short long) '<)
        (check-eq? (rope-compare long short) '>)
        (check-true (rope<? short long)))

      (test-case "spans more than one chunk (chunk-limit is 4)"
        (define a (v->rope mk '(1 2 3 4 5 6 7)))
        (define b (v->rope mk '(1 2 3 4 5 6 8)))
        (check-eq? (rope-compare a b) '<))))

  (define default-overlap-suite
    (compare-suite-for "default #:chunk-compare-overlap (omitted)"
                       source->oty-rope oty-rope<? oty-rope>? oty-rope<=? oty-rope>=? oty-rope-compare))

  (define custom-overlap-suite
    (compare-suite-for "explicit #:chunk-compare-overlap"
                       source->oty2-rope oty2-rope<? oty2-rope>? oty2-rope<=? oty2-rope>=? oty2-rope-compare))

  (define error-suite
    (test-suite "instantiation errors"
      (test-case "a missing required field (#:elem>?) fails"
        (check-true
         (expand-fails? (define-rope-instance total-order oty-x ([elem<? <])))))))

  (run-suite! (test-suite "class/total-order.rkt"
                          default-overlap-suite custom-overlap-suite error-suite)))
