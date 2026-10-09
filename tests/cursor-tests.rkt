#lang racket/base

;; Entirely independent of the type/class system: trees are built directly
;; from rope.rkt's own structs, with vector chunks whose contents are set
;; equal to the absolute index they occupy, so a cursor's correctness can
;; be checked just by comparing the peeked value to the cursor's own
;; abs-idx.

(module+ test
  (require rope/cursor
           rope/rope
           "../private/testing.rkt")

  ;; Three leaves of length 3, 2, 4 (total 9), values 0..8 == abs-idx.
  (define leaf-a (raw-leaf (vector 0 1 2)))
  (define leaf-b (raw-leaf (vector 3 4)))
  (define leaf-c (raw-leaf (vector 5 6 7 8)))
  (define tree (raw-node (raw-node leaf-a leaf-b) leaf-c))

  (define empty-leaf (raw-leaf (vector)))

  (define (peek cur) (vector-ref (rope-leaf-chunk (cursor-leaf cur)) (cursor-rel-idx cur)))
  (define (peek! cur) (vector-ref (rope-leaf-chunk (mutable-cursor-leaf cur)) (mutable-cursor-rel-idx cur)))

  (define immutable-suite
    (test-suite "immutable cursor"
      (test-case "rope->cursor lands in the right leaf, at the right relative index"
        (check-equal? (peek (rope->cursor tree 0)) 0)
        (check-equal? (peek (rope->cursor tree 4)) 4)  ; leaf-b, rel-idx 1
        (check-equal? (peek (rope->cursor tree 8)) 8)) ; leaf-c, rel-idx 3

      (test-case "rope->cursor on an empty rope is #f"
        (check-false (rope->cursor empty-leaf)))

      (test-case "cursor-advance crosses leaf boundaries correctly, both directions"
        (define c2 (rope->cursor tree 2))   ; leaf-a, rel-idx 2
        (define c4 (cursor-advance c2 2))   ; -> leaf-b, rel-idx 1
        (check-equal? (peek c4) 4)
        (define back (cursor-advance c4 -2))
        (check-equal? (peek back) 2))

      (test-case "cursor-advance past either end returns #f"
        (check-false (cursor-advance (rope->cursor tree 8) 2))
        (check-false (cursor-advance (rope->cursor tree 0) -1)))

      (test-case "cursor-advance by 0 is a no-op"
        (define c (rope->cursor tree 5))
        (check-equal? (peek (cursor-advance c 0)) (peek c)))

      (test-property "cursor-advance and cursor-retreat are inverses"
          #:trials 200
          ([start (random 9)]
           [k (- (random 9) 4)])
        (define c0 (rope->cursor tree start))
        (define advanced (cursor-advance c0 k))
        (cond
          [(not advanced) #t]  ; k walked off either end; nothing to invert
          [else
           (define back (cursor-retreat advanced k))
           (and back (= (cursor-abs-idx back) start))]))

      (test-property "cursor-advance by k agrees with k single steps of ±1"
          #:trials 150
          ([start (random 9)]
           [k (- (random 5) 2)])
        (define one-at-a-time
          (let loop ([cur (rope->cursor tree start)] [k k])
            (cond [(not cur) #f]
                  [(= k 0) cur]
                  [(> k 0) (loop (cursor-advance cur 1) (sub1 k))]
                  [else    (loop (cursor-advance cur -1) (add1 k))])))
        (define direct (cursor-advance (rope->cursor tree start) k))
        (cond [(and (not one-at-a-time) (not direct)) #t]
              [(and one-at-a-time direct)
               (= (cursor-abs-idx one-at-a-time) (cursor-abs-idx direct))]
              [else #f]))))

  (define mutable-suite
    (test-suite "mutable cursor"
      (test-case "rope->mutable-cursor / cursor-advance! land correctly"
        (define cur (rope->mutable-cursor tree 0))
        (cursor-advance! cur 4)
        (check-equal? (peek! cur) 4))

      (test-case "cursor-advance! past either end returns #f, without corrupting cur"
        (define cur (rope->mutable-cursor tree 8))
        (check-false (cursor-advance! cur 2))
        ;; cur's own fields are whatever the failed climb left them at;
        ;; what matters is a fresh, independent cursor still works.
        (check-equal? (peek! (rope->mutable-cursor tree 8)) 8))

      (test-case "copy-mutable-cursor makes an independent copy"
        (define original (rope->mutable-cursor tree 0))
        (define copy (copy-mutable-cursor original))
        (cursor-advance! copy 4)
        (check-equal? (peek! original) 0)
        (check-equal? (peek! copy) 4))

      (test-case "cursor->mutable-cursor / mutable-cursor->cursor round-trip"
        (define ic (rope->cursor tree 4))
        (define mc (cursor->mutable-cursor ic))
        (check-equal? (peek! mc) (peek ic))
        (define ic2 (mutable-cursor->cursor mc))
        (check-equal? (peek ic2) (peek ic)))

      (test-property "cursor-advance! round-trips with its own inverse"
          #:trials 200
          ([start (random 9)]
           [k (- (random 9) 4)])
        (define cur (rope->mutable-cursor tree start))
        (define advanced (cursor-advance! cur k))
        (cond [(not advanced) #t]
              [else
               (define back (cursor-advance! cur (- k)))
               (and back (= (mutable-cursor-abs-idx back) start))]))

      (test-property "cursor-advance! agrees with cursor-advance"
          #:trials 200
          ([start (random 9)]
           [k (- (random 9) 4)])
        (define immutable-result (cursor-advance (rope->cursor tree start) k))
        (define mutable-result (cursor-advance! (rope->mutable-cursor tree start) k))
        (cond [(and (not immutable-result) (not mutable-result)) #t]
              [(and immutable-result mutable-result)
               (= (cursor-abs-idx immutable-result) (mutable-cursor-abs-idx mutable-result))]
              [else #f]))))

  (run-suite! (test-suite "cursor.rkt" immutable-suite mutable-suite)))
