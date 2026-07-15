#lang racket/base

(require racket/contract
         racket/generic
         racket/match)

(provide
 gen:ropeable
 (contract-out
  ;; Ropes
  (struct rope      ())
  (struct rope-leaf ([count exact-nonnegative-integer?]
                     [width exact-nonnegative-integer?]
                     [raw   any/c]))
  (struct rope-node ([count exact-nonnegative-integer?]
                     [width exact-nonnegative-integer?]
                     [left  rope?]
                     [right rope?]))
  ;; Rope Operations
  [make-rope-node (rope? rope? . -> . rope-node?)]
  [rope-count     (rope? . -> . exact-nonnegative-integer?)]
  [rope-width     (rope? . -> . exact-nonnegative-integer?)]
  [rope-length    (rope? . -> . exact-nonnegative-integer?)]
  [rope-depth     (rope? . -> . exact-nonnegative-integer?)]
  [rope-empty?    (rope? . -> . boolean?)]
  [rope-balanced? (rope? . -> . boolean?)]
  [rope-flatten   (rope? . -> . list?)]
  [rope-concat    (rope? rope? . -> . rope?)]
  ;; Raw Generics
  [raw?       (ropeable? any/c . -> . boolean?)]
  [raw-limit  (ropeable? . -> . exact-nonnegative-integer?)]
  [raw-empty  (ropeable? . -> . any/c)]
  [raw-count  (ropeable? any/c . -> . exact-nonnegative-integer?)]
  [raw-width  (ropeable? any/c . -> . exact-nonnegative-integer?)]
  [raw-slice  (ropeable? any/c exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)]
  [raw-append (ropeable? any/c ... . -> . any/c)]
  [raw-ref    (ropeable? any/c exact-nonnegative-integer? . -> . any/c)]
  ;; Rope Generics
  [ropeable?         (any/c . -> . boolean?)]
  [make-rope-leaf    (ropeable? any/c . -> . rope-leaf?)]
  [make-empty-rope   (ropeable? . -> . rope?)]
  [rope-append1      (ropeable? rope? rope? . -> . rope?)]
  [rope-append       (ropeable? rope? ... . -> . rope?)]
  [rope-offset-index (ropeable? rope? exact-nonnegative-integer? . -> . exact-nonnegative-integer?)]
  [raw->rope         (ropeable? any/c . -> . rope?)]
  [rope->raw         (ropeable? rope? . -> . any/c)]
  ;; Cursors
  (struct cursor ([raw   any/c]
                  [pos   exact-nonnegative-integer?]
                  [after rope?]))
  [cursor-at-end? (ropeable? cursor? . -> . boolean?)]
  [cursor-peek    (ropeable? cursor? . -> . any/c)]
  [cursor-advance (ropeable? cursor? . -> . cursor?)]
  [cursor-drop    (ropeable? cursor? exact-nonnegative-integer? . -> . cursor?)]
  [rope->cursor   (ropeable? rope? . -> . cursor?)]
  [cursor->rope   (ropeable? cursor? . -> . rope?)]
  ;; Cursor-Based Rope Operations
  [rope-foldl (ropeable? procedure? any/c rope? rope? ... . -> . any/c)]
  [rope-foldr (ropeable? procedure? any/c rope? rope? ... . -> . any/c)]))

;;; ---------------------------------------------------------------------------------------------
;;; Rope
;;; ---------------------------------------------------------------------------------------------

;; A rope is either a leaf holding a small collection of elements, or a concatenation of two
;; sub-ropes. Both variants cache their aggregate element count and width so those queries are O(1).
(struct rope () #:transparent)
(struct rope-leaf rope (count width raw)        #:transparent)
(struct rope-node rope (count width left right) #:transparent)

(define (make-rope-node left right)
  (rope-node (+ (rope-count left) (rope-count right))
             (+ (rope-width left) (rope-width right))
             left right))

;; O(1)
(define (rope-count rope) (match rope [(rope-leaf c _ _) c] [(rope-node c _ _ _) c]))
(define (rope-width rope) (match rope [(rope-leaf _ w _) w] [(rope-node _ w _ _) w]))

;; O(1)
(define (rope-length rope)
  (rope-width rope))

;; O(1)
(define (rope-depth rope)
  (match rope
    [(rope-leaf _ _ _)   0]
    [(rope-node _ _ l r) (add1 (max (rope-depth l) (rope-depth r)))]))

;; O(1)
(define (rope-empty? rope)
  (zero? (rope-length rope)))

;; Fibonacci bound, offset by 2 per Boehm/Atkinson/Plass. O(1).
(define (fib-bound depth)
  (define table
    #(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
        75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352
        24157817 39088169 63245986 102334155))
  (vector-ref table (min depth (sub1 (vector-length table)))))

;; O(1)
(define (rope-balanced? rope)
  (or (rope-empty? rope)
      (>= (rope-count rope) (fib-bound (rope-depth rope)))))

;; Collect leaves left-to-right. O(# leaves)
(define (rope-flatten rope)
  (let loop ([rope rope] [acc null])
    (match rope
      [(rope-leaf _ _ raw) (cons raw acc)]
      [(rope-node _ _ l r) (loop l (loop r acc))])))

;; Naive concatenation. O(1)
(define (rope-concat left right)
  (make-rope-node left right))

;;; ---------------------------------------------------------------------------------------------
;;; Cursor
;;; ---------------------------------------------------------------------------------------------

;; A cursor contains the current chunk being consumed, a position within it, and the rope of
;; everything strictly after the current chunk.
(struct cursor (raw pos after) #:transparent)

;;; ---------------------------------------------------------------------------------------------
;;; Ropeable
;;; ---------------------------------------------------------------------------------------------

(define-generics ropeable
  #:requires (raw-limit raw-empty raw-count raw-width raw-slice raw-append raw-ref)
  ;; Raw
  [raw-limit  ropeable]                 ; max element count per leaf
  [raw?       ropeable obj]             ; raw chunk predicate
  [raw-empty  ropeable]                 ; construct a raw empty chunk
  [raw-count  ropeable raw]             ; element count of a raw chunk
  [raw-width  ropeable raw]             ; total width width of a raw chunk
  [raw-slice  ropeable raw pos end]     ; extract a raw sub-chunk
  [raw-append ropeable . raws]          ; concatenate raw chunks
  [raw-ref    ropeable raw pos]         ; used by cursor-peek
  ;; Rope
  [make-rope-leaf    ropeable raw]
  [make-empty-rope   ropeable]
  [rope-append1      ropeable left right]
  [rope-append       ropeable . ropes]
  [rope-offset-index ropeable rope ofs]
  [raw->rope         ropeable raw]
  [rope->raw         ropeable rope]
  ;; Cursor
  [cursor-at-end? ropeable cur]
  [cursor-peek    ropeable cur]
  [cursor-advance ropeable cur]
  [cursor-drop    ropeable cur k]
  [rope->cursor   ropeable rope]
  [cursor->rope   ropeable cur]
  ;; Cursor-Based Rope Operations
  [rope-foldl ropeable proc init rope0 . ropes]
  [rope-foldr ropeable proc init rope0 . ropes]

  #:fallbacks
  [(define/generic leaf:limit raw-limit)
   (define/generic raw:empty  raw-empty)
   (define/generic raw:count  raw-count)
   (define/generic raw:width  raw-width)
   (define/generic raw:slice  raw-slice)
   (define/generic raw:append raw-append)
   (define/generic raw:ref    raw-ref)

   (define (make-rope-leaf gen raw)
     (rope-leaf (raw:count gen raw) (raw:width gen raw) raw))

   (define (make-empty-rope gen)
     (make-rope-leaf gen (raw:empty gen)))

   ;; Balancing concatenation: concatenate naively, then repair balance if the Fibonacci invariant
   ;; is violated.
   ;;
   ;; This makes rope-append1 amortized O(log n): a rebuild is triggered only when depth has drifted
   ;; ahead of what the element count justifies, which cannot happen more than O(log n) times across
   ;; O(n) appends.
   (define (rope-append1 gen left right)
     (cond
       [(zero? (rope-count left))  right]
       [(zero? (rope-count right)) left]
       [else
        (define combined (rope-concat left right))
        (if (rope-balanced? combined) combined (raw->rope gen (rope->raw gen combined)))]))

   ;; O(log n * |ropes|)
   (define (rope-append gen . ropes)
     (for/fold ([l (make-empty-rope gen)])
               ([r (in-list ropes)])
       (rope-append1 gen l r)))

   ;; Splits at an element index. O(log n) amortized: one descent, plus one rope-append1 per level
   ;; on the way back up.
   (define (rope-split gen rope i)
     (match rope
       [(rope-leaf cnt _ raw)
        (define slice (raw:slice gen))
        (values (make-rope-leaf gen (slice raw 0 i))
                (make-rope-leaf gen (slice raw i cnt)))]
       [(rope-node _ _ l r)
        (define lc (rope-count l))
        (if (<= i lc)
            (let-values ([(ll lr) (rope-split gen l i)])
              (values ll (rope-append1 gen lr r)))
            (let-values ([(rl rr) (rope-split gen r (- i lc))])
              (values (rope-append1 gen l rl) rr)))]))

   ;; Finds the leftmost element index containing offset `ofs`. O(log n).
   (define (rope-offset-index gen rope ofs)
     (match rope
       [(rope-leaf _ _ raw)
        #:when (= (raw:count gen raw) 1)
        0]
       [(rope-leaf cnt _ raw)
        ;; Rope gen do not give a raw's per-element width directly, but we can take the raw width of a
        ;; single-element slice.
        (define (elem-width i)
          (raw:width gen (raw:slice gen raw i (add1 i))))
        (let loop ([i 0] [acc 0])
          (if (= i cnt)
              (sub1 i)
              (let ([iw (elem-width i)])
                (if (< ofs (+ acc iw)) i (loop (add1 i) (+ acc iw))))))]
       [(rope-node _ _ l r)
        (define lw (rope-width l))
        (if (< ofs lw)
            (rope-offset-index gen l ofs)
            (+ (rope-count l) (rope-offset-index gen r (- ofs lw))))]))

   ;; Replace `old-len` elements starting at `start` with `new-raw`. O(log n + |new-raw|).
   (define (rope-splice gen rope start old-len new-raw)
     (define-values (before rest) (rope-split gen rope start))
     (define-values (_gone after) (rope-split gen rest old-len))
     (rope-append1 gen (rope-append1 gen before (raw->rope gen new-raw)) after))

   ;; Extracts `len` elements starting at `start`. O(log n + # leaves in slice) amortized.
   (define (rope-slice gen rope start len)
     (define-values (_before rest) (rope-split gen rope start))
     (define-values (slice _after) (rope-split gen rest len))
     slice)

   ;; O(n) bottom-up balanced build.
   (define (raw->rope gen raw)
     (define n (raw:count gen raw))
     (if (<= n (leaf:limit gen))
         (make-rope-leaf gen raw)
         (let ([mid   (quotient n 2)])
           (rope-concat (raw->rope gen (raw:slice gen raw 0 mid))
                        (raw->rope gen (raw:slice gen raw mid n))))))

   ;; O(# leaves)
   (define (rope->raw gen rope)
     (apply raw:append gen (rope-flatten rope)))

   ;; O(1)
   (define (cursor-at-end? gen cur)
     (match-define (cursor raw pos after) cur)
     (and (>= pos (raw:count gen raw))
          (zero? (rope-count after))))

   ;; Returns the element under the cursor. O(1) except when crossing a leaf boundary (amortized
   ;; O(1) over the leaf).
   ;;
   ;; When the cursor's position is past the end of its leaf, returns the first element of the next
   ;; leaf, if it exists, or false otherwise.
   (define (cursor-peek gen cur)
     (match-define (cursor raw pos after) cur)
     (cond
       [(< pos (raw:count gen raw)) (raw:ref gen raw pos)]
       [(zero? (rope-count after)) #f]
       [else (cursor-peek gen (rope->cursor gen after))])) ; boundary crossing

   ;; Shifts the cursor's position to the right by one. O(1) except when crossing a leaf boundary
   ;; (amortized O(1) over the leaf)
   (define (cursor-advance gen cur)
     (match-define (cursor raw pos after) cur)
     (define pos+ (add1 pos))
     (if (< pos+ (raw:count gen raw))
         (cursor raw pos+ after)
         (rope->cursor gen after)))     ; boundary crossing

   ;; Skip k tokens at once. O(log n), independent of k
   (define (cursor-drop gen cur k)
     (if (zero? k)
         cur
         (let-values ([(_skipped after) (rope-split gen (cursor->rope gen cur) k)])
           (rope->cursor gen after))))

   ;; Descends to the leftmost leaf. O(depth) = O(log n)
   (define (rope->cursor gen rope)
     (match rope
       [(rope-leaf _ _ raw)
        (cursor raw 0 (make-empty-rope gen))]
       [(rope-node _ _ l r)
        (match-define (cursor raw pos after) (rope->cursor gen l))
        (cursor raw pos (rope-append1 gen after r))]))

   ;; Reconstitutes everything from the cursor on as a rope. This is needed to implement a large
   ;; skip in O(log n) rather than O(k) individual steps.
   (define (cursor->rope gen cur)
     (match-define (cursor raw pos rest) cur)
     (define n (raw:count gen raw))
     (rope-append1 gen (make-rope-leaf gen (raw:slice gen raw pos n)) rest))

   ;; O(n)
   (define (rope-foldl gen proc init rope0 . ropes)
     ;; All ropes must have the same number of elements.
     (define len1 (rope-count rope0))
     (for ([rope (in-list ropes)])
       (define len2 (rope-count rope))
       (unless (= len1 len2)
         (raise-arguments-error 'rope-foldl
                                "all ropes must have the same length"
                                "first rope length" len1
                                "other rope length" len2
                                "procedure" proc)))

     (define (inner-foldl init curs)
       (if (cursor-at-end? gen (car curs))
           init
           (let ([head (map (λ (c) (cursor-peek    gen c)) curs)]
                 [tail (map (λ (c) (cursor-advance gen c)) curs)])
             (inner-foldl (apply proc init head) tail))))

     (inner-foldl init (map (λ (r) (rope->cursor gen r)) (cons rope0 ropes))))

   ;; O(n)
   (define (rope-foldr gen proc init rope0 . ropes)
     ;; All ropes must have the same number of elements.
     (define len1 (rope-count rope0))
     (for ([rope (in-list ropes)])
       (define len2 (rope-count rope))
       (unless (= len1 len2)
         (raise-arguments-error 'rope-foldr
                                "all ropes must have the same length"
                                "first rope length" len1
                                "other rope length" len2
                                "procedure" proc)))

     (define (inner-foldr init curs)
       (if (cursor-at-end? gen (car curs))
           init
           (let ([head (map (λ (c) (cursor-peek    gen c)) curs)]
                 [tail (map (λ (c) (cursor-advance gen c)) curs)])
             (apply proc (inner-foldr init tail) head))))

     (inner-foldr init (map (λ (r) (rope->cursor gen r)) (cons rope0 ropes))))])
