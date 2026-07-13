#lang racket/base

(require racket/match
         rope/rope)

(provide (all-defined-out))

;; The current leaf being consumed, a position within it, and the rope of everything strictly to its
;; right.
(struct cursor (raw pos rest) #:transparent)

;;; ---------------------------------------------------------------------------------------------
;;; Properties
;;; ---------------------------------------------------------------------------------------------

;; O(1).
(define (cursor-at-end? ops cur)
  (match-define (cursor raw pos rest) cur)
  (and (>= pos ((rope-ops-raw-length ops) raw))
       (zero? (rope-count rest))))

;;; ---------------------------------------------------------------------------------------------
;;; Operations
;;; ---------------------------------------------------------------------------------------------

;; Returns the element under the cursor. O(1) except when crossing a leaf boundary (amortized O(1)
;; over the leaf).
;;
;; When the cursor's position is past the end of its leaf, returns the first element of the next
;; leaf, if it exists, or false otherwise.
(define (cursor-peek ops cur)
  (match-define (cursor raw pos rest) cur)
  (cond
    [(< pos ((rope-ops-raw-length ops) raw)) ((rope-ops-raw-ref ops) raw pos)]
    [(zero? (rope-count rest)) #f]
    [else (cursor-peek ops (rope->cursor ops rest))])) ; boundary crossing

;; Shifts the cursor's position to the right by one.  O(1) except when crossing a leaf boundary
;; (amortized O(1) over the leaf).
(define (cursor-advance ops cur)
  (match-define (cursor raw pos rest) cur)
  (define pos+ (add1 pos))
  (if (< pos+ ((rope-ops-raw-length ops) raw))
      (cursor raw pos+ rest)
      (rope->cursor ops rest)))         ; boundary crossing

;; Skip k tokens at once. O(log n), independent of k.
(define (cursor-drop ops cur k)
  (if (zero? k)
      cur
      (let-values ([(_skipped after) (rope-split ops (cursor->rope ops cur) k)])
        (rope->cursor ops after))))

;;; ---------------------------------------------------------------------------------------------
;;; Conversions
;;; ---------------------------------------------------------------------------------------------

;; Descends to the leftmost leaf. O(depth) = O(log n).
(define (rope->cursor ops rope)
  (match rope
    [(rope-leaf _ _ raw)
     (cursor raw 0 (make-empty-rope ops))]
    [(rope-node _ _ l r)
     (match-define (cursor lraw lpos lrest) (rope->cursor ops l))
     (cursor lraw lpos (rope-append ops lrest r))]))

;; Reconstitutes everything from the cursor on as a rope. This is needed to implement a large
;; skip in O(log n) rather than O(k) individual steps.
(define (cursor->rope ops cur)
  (match-define (cursor raw pos rest) cur)
  (define n ((rope-ops-raw-length ops) raw))
  (rope-append ops (make-rope-leaf ops ((rope-ops-raw-slice ops) raw pos n)) rest))

;;; ---------------------------------------------------------------------------------------------
;;; Cursor-Based Rope Operations
;;; ---------------------------------------------------------------------------------------------

;; O(n)
(define (rope-foldl ops proc init rope0 . ropes)
  ;; All ropes must have the same length.
  (define len1 (rope-length rope0))
  (for ([rope (in-list ropes)])
    (define len2 (rope-length rope))
    (unless (= len1 len2)
      (raise-arguments-error 'rope-foldl
                             "all ropes must have the same length"
                             "first rope length" len1
                             "other rope length" len2
                             "procedure" proc)))

  (define (inner-foldl init curs)
    (if (cursor-at-end? ops (car curs))
        init
        (let ([head (map (λ (c) (cursor-peek    ops c)) curs)]
              [tail (map (λ (c) (cursor-advance ops c)) curs)])
          (inner-foldl (apply proc init head) tail))))

  (inner-foldl init (map (λ (r) (rope->cursor ops r)) (cons rope0 ropes))))

;; O(n)
(define (rope-foldr ops proc init rope0 . ropes)
  ;; All ropes must have the same length.
  (define len1 (rope-length rope0))
  (for ([rope (in-list ropes)])
    (define len2 (rope-length rope))
    (unless (= len1 len2)
      (raise-arguments-error 'rope-foldr
                             "all ropes must have the same length"
                             "first rope length" len1
                             "other rope length" len2
                             "procedure" proc)))

  (define (inner-foldr init curs)
    (if (cursor-at-end? ops (car curs))
        init
        (let ([head (map (λ (c) (cursor-peek    ops c)) curs)]
              [tail (map (λ (c) (cursor-advance ops c)) curs)])
          (apply proc (inner-foldr init tail) head))))

  (inner-foldr init (map (λ (r) (rope->cursor ops r)) (cons rope0 ropes))))
