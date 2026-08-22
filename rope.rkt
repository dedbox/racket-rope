#lang racket/base

;; rope/rope.rkt
;;
;; The core rope data structure, the tree balancing algorithm, and all rope
;; operations that do not require a rope type descriptor.

(require racket/fixnum)

(provide (all-defined-out))

;;; ----------------------------------------------------------------------------
;;; The Data Structure
;;; ----------------------------------------------------------------------------

(struct rope () #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b recursive-equal?)
     (and (rope? b)
          (= (rope-count a) (rope-count b))
          (= (rope-size  a) (rope-size  b))
          (cond
            [(and (rope-leaf? a) (rope-leaf? b))
             (equal? (rope-leaf-chunk a) (rope-leaf-chunk b))]
            [(and (rope-node? a) (rope-node? b))
             (and (recursive-equal? (rope-node-left  a) (rope-node-left  b))
                  (recursive-equal? (rope-node-right a) (rope-node-right b)))]
            [else #f])))
   (define (hash-proc a recursive-hash)
     (cond
       [(rope-leaf? a) (equal-hash-code (rope-leaf-chunk a))]
       [(rope-node? a) (+ (* 31 (recursive-hash (rope-node-left a)))
                          (recursive-hash (rope-node-right a)))]
       [else
        (error 'hash-proc "expected a rope, got ~v" a)]))
   (define (hash2-proc a recursive-hash)
     (cond
       [(rope-leaf? a) (equal-secondary-hash-code (rope-leaf-chunk a))]
       [(rope-node? a) (+ (* 31 (recursive-hash (rope-node-left a)))
                          (recursive-hash (rope-node-right a)))]
       [else
        (error 'hash2-proc "expected a rope, got ~v" a)]))])

;; count = the number of elements in `chunk`
;; size  = the number of valid indices spanning the elements of `chunk`
;; chunk = a sequence of raw element data
;;
;; For byte strings and strings without multibyte Unicode characters, count =
;; bytes. However, this is not always the case. For example, if a chunk
;; consists of a vector of lexical tokens containing the underlying text,
;; `count` is the number of tokens and `size` is the number of characters
;; covered by the tokens.
(struct rope-leaf rope (count size chunk) #:transparent)

;; count = the number of elements spanning `left` and `right`
;; size  = the number of valid indices spanning the elements of `left` and `right`
;; depth = the maximum distance from this node to the leaves of `left` and `right`
(struct rope-node rope (count size depth left right) #:transparent)

;;; ----------------------------------------------------------------------------
;;; Core Operations
;;; ----------------------------------------------------------------------------

;; All of these operations are O(1).

(define (rope-count a) (if (rope-leaf? a) (rope-leaf-count a) (rope-node-count a)))
(define (rope-size  a) (if (rope-leaf? a) (rope-leaf-size  a) (rope-node-size  a)))
(define (rope-depth a) (if (rope-leaf? a) 0 (rope-node-depth a)))
(define (rope-empty? a) (zero? (rope-size a)))

;;; ----------------------------------------------------------------------------
;;; Tree Balancing Algorithm
;;; ----------------------------------------------------------------------------

;; The Fibonacci bound, offset by 2, per Boehm/Atkinson/Plass:
;;
;;   A balanced rope of depth d must hold at least Fib(d+2) elements.
;;
;; A rope is a binary tree. By definition, the depth of a parent node is
;; always:
;;
;;   d_parent = 1 + max(d_left, d_right).
;;
;; For a rope of depth d with N elements, at least one of its sub-ropes must
;; have depth d - 1. For the other sub-rope, we want to find the smallest
;; possible depth that still gives O(log N) worst-case time complexity for
;; basic operations without creating more work than it saves.
;;
;; Suppose d_left = d_parent - 1. If we require d_right ≥ d_parent - 1 always,
;; then the rope will stay perfectly balanced at all times. In practice, the
;; amount of work involved negates any savings. If d_right = d_parent - K for
;; some k > 1, then the rope becomes more list-like as K increases.
;;
;; The sweet spot is K = 2. This gives the rope some room to absorb small
;; changes without triggering a rebalance, and it is always near enough to
;; perfect balance to give roughly O(log N) time complexity.
;;
;; What does this have to do with Fiboncci numbers? Let M(d) be the minimum
;; number of elements required for a balanced rope of depth d, and suppose
;; d_left = d - 1. Then we have
;;
;;   M(d_parent) = M(d_left) + M(d_right) = M(d_parent - 1) + M(d_parent - 2).
;;
;; This recurrence generates the Fibonacci numbers, giving us a cheap balance
;; test - if a rope's depth is at least the corresponding Fibonacci number, we
;; say is balanced. Since the Fibonacci sequence begins (0, 1, 1, 2, ...), and
;; a non-empty rope of depth 0 has exactly one element, and a rope of depth 1
;; has at least two elements, we offset the sequence by two.

;; Determine the largest Fibonacci number that fits into a fixnum. On a 64-bit
;; system, this should be 86.
(define max-fib-index
  (let loop ([i 0] [Fᵢ₊₁ 2] [Fᵢ 1])
    (define space-left (- (most-positive-fixnum) Fᵢ₊₁))
    (if (< space-left Fᵢ) (add1 i) (loop (add1 i) (+ Fᵢ₊₁ Fᵢ) Fᵢ₊₁))))

(define fib-vector
  (let ([vec (make-vector (add1 max-fib-index))])
    (vector-set! vec 0 1)
    (vector-set! vec 1 2)
    (for ([i (in-range 2 (add1 max-fib-index))])
      (vector-set! vec i (+ (vector-ref vec (- i 1))
                            (vector-ref vec (- i 2)))))
    vec))

;; O(1)
(define (fib-bound d) (vector-ref fib-vector (min d max-fib-index)))

;; A rope of depth n is /strictly balanced/ if its length is at least Fₙ₊₂.
;; O(1)
(define (rope-strictly-balanced? r)
  (or (= 0 (rope-count r)) (>= (rope-count r) (fib-bound (+ (rope-depth r) 2)))))

;; A rope of depth n is /mostly balanced/ if its length is at most Fₙ. O(1)
(define (rope-mostly-balanced? r)
  (or (= 0 (rope-count r)) (>= (rope-count r) (fib-bound (rope-depth r)))))

;; Collect chunks left-to-right. O(# leaves)
(define (rope-chunks a)
  (let loop ([a a] [acc null])
    (if (rope-leaf? a)
        (cons (rope-leaf-chunk a) acc)
        (loop (rope-node-left a) (loop (rope-node-right a) acc)))))
