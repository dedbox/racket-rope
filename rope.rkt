#lang racket/base

(require racket/match)

(provide (all-defined-out))

;; A rope is either a leaf holding a small collection of elements, or a concatenation of two
;; sub-ropes. Both variants cache their aggregate element count and width so those queries are O(1).
(struct rope-leaf (count width raw)        #:transparent)
(struct rope-node (count width left right) #:transparent)

(struct rope-ops
  (limit                                ; max element count per leaf
   raw-empty                            ; construct a raw empty chunk
   raw-length                           ; element count of a raw chunk
   raw-width                            ; character width of a raw chunk
   raw-slice                            ; extract a raw sub-chunk
   raw-append                           ; concatenate many raw chunks (used only during a rebalance)
   raw-ref)                             ; used by the cursor's peek
  #:transparent)

(struct rope-ops-impl rope-ops (leaf node) #:transparent)

;;; O(1).
(define (make-empty-rope ops)
  ((rope-ops-impl-leaf ops) 0 0 ((rope-ops-raw-empty ops))))

;;; O(1).
(define (make-rope-leaf ops raw)
  ((rope-ops-impl-leaf ops) ((rope-ops-raw-length ops) raw) ((rope-ops-raw-width ops) raw) raw))

;;; O(1).
(define (make-rope-node ops count width left right)
  ((rope-ops-impl-node ops) count width left right))

;;; ---------------------------------------------------------------------------------------------
;;; Properties
;;; ---------------------------------------------------------------------------------------------

;;; O(1).
(define (rope-count rope) (match rope [(rope-leaf c _ _) c] [(rope-node c _ _ _) c]))
(define (rope-width rope) (match rope [(rope-leaf _ w _) w] [(rope-node _ w _ _) w]))

;;; O(1).
(define (rope-length rope)
  (rope-width rope))

;;; O(1).
(define (rope-depth rope)
  (match rope
    [(rope-leaf _ _ _)   0]
    [(rope-node _ _ l r) (add1 (max (rope-depth l) (rope-depth r)))]))

;; Fibonacci bound, offset by 2 per Boehm/Atkinson/Plass. O(1).
(define (fib-bound depth)
  (define table
    #(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
        75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352
        24157817 39088169 63245986 102334155))
  (vector-ref table (min depth (sub1 (vector-length table)))))

;;; O(1).
(define (rope-balanced? rope)
  (or (rope-empty? rope)
      (>= (rope-count rope) (fib-bound (rope-depth rope)))))

;;; O(1).
(define (rope-empty? rope)
  (zero? (rope-length rope)))

;;; ---------------------------------------------------------------------------------------------
;;; Operations
;;; ---------------------------------------------------------------------------------------------

;; Collect leaves left-to-right. O(# leaves). Used only on rebalance.
(define (rope-flatten rope)
  (let loop ([rope rope] [acc null])
    (match rope
      [(rope-leaf _ _ raw) (cons raw acc)]
      [(rope-node _ _ l r) (loop l (loop r acc))])))

;;; Naive concatenation. O(1).
(define (rope-concat ops l r)
  (make-rope-node ops
                  (+ (rope-count l) (rope-count r))
                  (+ (rope-width l) (rope-width r))
                  l r))

;; Balancing concatenation: concatenate naively, then repair balance if the Fibonacci invariant is
;; violated.
;;
;; This makes rope-append1 amortized O(log n): a rebuild is triggered only when depth has drifted
;; ahead of what the element count justifies, which cannot happen more than O(log n) times across
;; O(n) appends.
(define (rope-append1 ops l r)
  (cond
    [(zero? (rope-count l)) r]
    [(zero? (rope-count r)) l]
    [else
     (define combined (rope-concat ops l r))
     (if (rope-balanced? combined) combined (raw->rope ops (rope->raw ops combined)))]))

;; O(log n * |ropes|)
(define (rope-append ops ropes)
  (for/fold ([l (make-empty-rope ops)])
            ([r (in-list ropes)])
    (rope-append1 ops l r)))

;; Splits at an element index. O(log n) amortized: one descent, plus one rope-append1 per level on
;; the way back up.
(define (rope-split ops rope i)
  (match rope
    [(rope-leaf cnt _ raw)
     (define slice (rope-ops-raw-slice ops))
     (values (make-rope-leaf ops (slice raw 0 i))
             (make-rope-leaf ops (slice raw i cnt)))]
    [(rope-node _ _ l r)
     (define lc (rope-count l))
     (if (<= i lc)
         (let-values ([(ll lr) (rope-split ops l i)])
           (values ll (rope-append1 ops lr r)))
         (let-values ([(rl rr) (rope-split ops r (- i lc))])
           (values (rope-append1 ops l rl) rr)))]))

;; Finds the leftmost element index containing offset `ofs`. O(log n).
(define (rope-offset-index ops rope ofs)
  (match rope
    [(rope-leaf _ _ raw)
     #:when (= ((rope-ops-raw-length ops) raw) 1)
     0]
    [(rope-leaf cnt _ raw)
     ;; Rope ops do not give a raw's per-element width directly, but we can take the raw width of a
     ;; single-element slice.
     (define (elem-width i)
       ((rope-ops-raw-width ops) ((rope-ops-raw-slice ops) raw i (add1 i))))
     (let loop ([i 0] [acc 0])
       (if (= i cnt)
           (sub1 i)
           (let ([iw (elem-width i)])
             (if (< ofs (+ acc iw)) i (loop (add1 i) (+ acc iw))))))]
    [(rope-node _ _ l r)
     (define lw (rope-width l))
     (if (< ofs lw)
         (rope-offset-index ops l ofs)
         (+ (rope-count l) (rope-offset-index ops r (- ofs lw))))]))

;; Replace `old-len` elements starting at `start` with `new-raw`. O(log n + |new-raw|).
(define (rope-splice ops rope start old-len new-raw)
  (define-values (before rest) (rope-split ops rope start))
  (define-values (_gone after) (rope-split ops rest old-len))
  (rope-append1 ops (rope-append1 ops before (raw->rope ops new-raw)) after))

;; Extracts `len` elements starting at `start`. O(log n + # leaves in slice) amortized.
(define (rope-slice ops rope start len)
  (define-values (_before rest) (rope-split ops rope start))
  (define-values (slice _after) (rope-split ops rest len))
  slice)

;;; ---------------------------------------------------------------------------------------------
;;; Conversions
;;; ---------------------------------------------------------------------------------------------

;; O(n) bottom-up balanced build.
(define (raw->rope ops raw)
  (define n ((rope-ops-raw-length ops) raw))
  (if (<= n (rope-ops-limit ops))
      (make-rope-leaf ops raw)
      (let ([mid   (quotient n 2)]
            [slice (rope-ops-raw-slice ops)])
        (rope-concat ops
                     (raw->rope ops (slice raw 0 mid))
                     (raw->rope ops (slice raw mid n))))))

;; O(# leaves)
(define (rope->raw ops rope)
  ((rope-ops-raw-append ops) (rope-flatten rope)))

