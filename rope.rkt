#lang racket/base

;; rope/rope.rkt
;;
;; The core rope data structure, the tree balancing algorithm, and all rope
;; operations that do not require a rope type descriptor.

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

(define (rope-count ρ) (if (rope-leaf? ρ) (rope-leaf-count ρ) (rope-node-count ρ)))
(define (rope-size  ρ) (if (rope-leaf? ρ) (rope-leaf-size  ρ) (rope-node-size  ρ)))
(define (rope-depth ρ) (if (rope-leaf? ρ) 0 (rope-node-depth ρ)))
(define (rope-empty? ρ) (zero? (rope-size ρ)))

;;; ----------------------------------------------------------------------------
;;; Tree Balancing Algorithm
;;; ----------------------------------------------------------------------------

;; The Fibonacci bound, offset by 2, per Boehm/Atkinson/Plass:
;;
;;   A balanced rope of depth d must hold at least fib(d+2) elements.
;;
(define FIB-BOUND-TABLE
  #(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711
      28657 46368 75025 121393 196418 317811 514229 832040 1346269 2178309
      3524578 5702887 9227465 14930352 24157817 39088169 63245986 102334155))

;; O(1)
(define (fib-bound depth)
  (vector-ref FIB-BOUND-TABLE
              (min depth (sub1 (vector-length FIB-BOUND-TABLE)))))

;; O(1)
(define (rope-balanced? ρ)
  (or (rope-empty? ρ) (>= (rope-count ρ) (fib-bound (rope-depth ρ)))))

;; Collect chunks left-to-right. O(# leaves)
(define (rope-flatten ρ)
  (let loop ([ρ ρ] [acc null])
    (if (rope-leaf? ρ)
        (cons (rope-leaf-chunk ρ) acc)
        (loop (rope-node-left ρ) (loop (rope-node-right ρ) acc)))))
