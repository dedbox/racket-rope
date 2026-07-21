#lang racket/base

(require racket/contract
         racket/generic
         racket/match)

(provide
 gen:ropeable
 gen:rope-equatable
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
  ;; Rupe Equatability
  [rope=?    (rope? rope? . -> . boolean?)]
  [rope-hash (rope? . -> . pair?)]
  ;; Rope Operations
  [rope-count     (rope? . -> . exact-nonnegative-integer?)]
  [rope-width     (rope? . -> . exact-nonnegative-integer?)]
  [rope-length    (rope? . -> . exact-nonnegative-integer?)]
  [rope-depth     (rope? . -> . exact-nonnegative-integer?)]
  [rope-empty?    (rope? . -> . boolean?)]
  [rope-balanced? (rope? . -> . boolean?)]
  [rope-flatten   (rope? . -> . list?)]
  ;; Raw Generics
  [raw?        (ropeable? any/c . -> . boolean?)]
  [raw-limit   (ropeable? . -> . exact-nonnegative-integer?)]
  [raw-empty   (ropeable? . -> . any/c)]
  [raw-count   (ropeable? any/c . -> . exact-nonnegative-integer?)]
  [raw-width   (ropeable? any/c . -> . exact-nonnegative-integer?)]
  [raw-slice   (ropeable? any/c exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c)]
  [raw-append  (ropeable? any/c ... . -> . any/c)]
  [raw-ref     (ropeable? any/c exact-nonnegative-integer? . -> . any/c)]
  [raw-compare (ropeable? any/c any/c . -> . (or/c '< '= '>))]
  ;; Rope Generics
  [ropeable?         (any/c . -> . boolean?)]
  [rope-leaf-ctor    (ropeable? . -> . procedure?)]
  [rope-node-ctor    (ropeable? . -> . procedure?)]
  [make-rope-leaf    (ropeable? any/c . -> . rope-leaf?)]
  [make-rope-node    (ropeable? rope? rope? . -> . rope-node?)]
  [make-empty-rope   (ropeable? . -> . rope?)]
  [rope-concat       (ropeable? rope? rope? . -> . rope?)]
  [rope-append1      (ropeable? rope? rope? . -> . rope?)]
  [rope-append       (ropeable? rope? ... . -> . rope?)]
  [rope-split        (ropeable? rope? exact-nonnegative-integer? . -> . (values rope? rope?))]
  [rope-offset-index (ropeable? rope? exact-nonnegative-integer? . -> . exact-nonnegative-integer?)]
  [rope-splice       (ropeable? rope? exact-nonnegative-integer?
                                exact-nonnegative-integer? any/c . -> . rope?)]
  [rope-slice        (ropeable? rope? exact-nonnegative-integer?
                                exact-nonnegative-integer? . -> . rope?)]
  [raw->rope         (ropeable? any/c . -> . rope?)]
  [rope->raw         (ropeable? rope? . -> . any/c)]
  [rope-compare-with (ropeable? procedure? rope? rope? . -> . (or/c '< '= '>))]
  [rope-compare      (ropeable? rope? rope? . -> . (or/c '< '= '>))]
  [rope<?            (ropeable? rope? rope? . -> . boolean?)]
  [rope<=?           (ropeable? rope? rope? . -> . boolean?)]
  [rope>?            (ropeable? rope? rope? . -> . boolean?)]
  [rope>=?           (ropeable? rope? rope? . -> . boolean?)]
  ;; Cursors
  (struct cursor ([raw   any/c]
                  [pos   exact-nonnegative-integer?]
                  [after rope?]))
  [cursor-at-end? (ropeable? cursor? . -> . boolean?)]
  [cursor-peek    (ropeable? cursor? . -> . any/c)]
  [cursor-advance (ropeable? cursor? . -> . cursor?)]
  [cursor-drop    (ropeable? cursor? exact-nonnegative-integer? . -> . cursor?)]
  [cursor-take    (ropeable? cursor? exact-nonnegative-integer? . -> . rope?)]
  [rope->cursor   (ropeable? rope? . -> . cursor?)]
  [cursor->rope   (ropeable? cursor? . -> . rope?)]
  ;; Cursor-Based Rope Operations
  [rope-foldl (ropeable? procedure? any/c rope? rope? ... . -> . any/c)]
  [rope-foldr (ropeable? procedure? any/c rope? rope? ... . -> . any/c)]))

;;; --------------------------------------------------------------------------
;;; Rope Equatable
;;; --------------------------------------------------------------------------

(define-generics rope-equatable
  [rope=?    rope-equatable other]
  [rope-hash rope-equatable]
  #:fallbacks
  [;; A rope built directly from the leaf/node constructors has no
   ;; type-specific notion of content equality to hash against. This recovers
   ;; plain structural equality: same shape, raw payloads compared with
   ;; equal?, recursively for nodes. define-rope-type overrides this with a
   ;; content-based (shape-independent) implementation; see rope-poly-hash and
   ;; *-rope-content=? in define-rope-type.rkt.
   (define (rope=? a b)
     (and (rope? b)
          (= (rope-count a) (rope-count b))
          (= (rope-width a) (rope-width b))
          (match* (a b)
            [((rope-leaf _ _ ra)    (rope-leaf _ _ rb))    (equal? ra rb)]
            [((rope-node _ _ la ra) (rope-node _ _ lb rb)) (and (rope=? la lb) (rope=? ra rb))]
            [(_ _) #f])))
   (define (rope-hash a)
     (match a
       [(rope-leaf _ _ r) (cons (equal-hash-code r) (equal-secondary-hash-code r))]
       [(rope-node _ _ l r)
        (match-define (cons hl sl) (rope-hash l))
        (match-define (cons hr sr) (rope-hash r))
        (cons (+ (* 31 hl) hr) (+ (* 31 sl) sr))]))])

;;; --------------------------------------------------------------------------
;;; Rope
;;; --------------------------------------------------------------------------

;; A rope is either a leaf holding a small chunk directly, or a node
;; concatenating two sub-ropes. Both cache their aggregate element count and
;; width, so those queries are O(1).
;;
;; equal?/equal-hash-code will dispatch through gen:rope-equatable rather than
;; #:transparent's default structural comparison: a rope's *content* (the
;; sequence of elements it denotes) should compare equal regardless of how its
;; tree happens to be shaped. Declaring gen:equal+hash here, on the common
;; supertype, means subtypes (rope-leaf, rope-node, and every define-rope-type
;; instance) inherit this dispatch and cannot silently fall back to plain
;; structural equality. gen:rope-equatable's #:fallbacks (below) keep the
;; default behavior for ropes that don't implement rope=?/rope-hash
;; themselves.
(struct rope ()
  #:methods gen:rope-equatable []
  #:methods gen:equal+hash
  [(define (equal-proc a b _) (rope=? a b))
   (define (hash-proc  a _)   (car (rope-hash a)))
   (define (hash2-proc a _)   (cdr (rope-hash a)))])

(struct rope-leaf rope (count width raw)        #:transparent)
(struct rope-node rope (count width left right) #:transparent)

;; O(1)
(define (rope-count r) (if (rope-leaf? r) (rope-leaf-count r) (rope-node-count r)))
(define (rope-width r) (if (rope-leaf? r) (rope-leaf-width r) (rope-node-width r)))

;; O(1). Alias for rope-width, for symmetry with string-length/bytes-length.
(define (rope-length r) (rope-width r))

;; O(1)
(define (rope-depth r)
  (if (rope-leaf? r)
      0
      (add1 (max (rope-depth (rope-node-left r)) (rope-depth (rope-node-right r))))))

;; O(1)
(define (rope-empty? r) (zero? (rope-length r)))

;; Fibonacci bound, offset by 2 per Boehm/Atkinson/Plass: a balanced rope of
;; depth d must hold at least fib(d+2) elements. Precomputed once at module
;; load.
(define FIB-BOUND-TABLE
  #(1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368
      75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465 14930352
      24157817 39088169 63245986 102334155))

(define (fib-bound depth)
  (vector-ref FIB-BOUND-TABLE (min depth (sub1 (vector-length FIB-BOUND-TABLE)))))

;; O(1)
(define (rope-balanced? r)
  (or (rope-empty? r) (>= (rope-count r) (fib-bound (rope-depth r)))))

;; Collect leaves left-to-right. O(# leaves)
(define (rope-flatten r)
  (let loop ([r r] [acc null])
    (if (rope-leaf? r)
        (cons (rope-leaf-raw r) acc)
        (loop (rope-node-left r) (loop (rope-node-right r) acc)))))

;;; --------------------------------------------------------------------------
;;; Cursor
;;; --------------------------------------------------------------------------

;; The current chunk being read, a position within it, and a rope of
;; everything strictly after it.
(struct cursor (raw pos after) #:transparent)

;;; --------------------------------------------------------------------------
;;; Rope Comparisons
;;; --------------------------------------------------------------------------

;; Lexicographic comparison, parameterized on the raw-chunk comparator, so
;; alternate orderings (e.g. case-insensitive) can reuse the same walk without
;; a second gen:ropeable instance. O(log n + d) amortized, where d is the
;; number of elements scanned before the first difference (or n on a tie),
;; each amortized O(1) even across leaf boundaries.
(define (rope-compare-with self proc a b)
  (let loop ([ca (rope->cursor self a)] [cb (rope->cursor self b)])
    (cond
      [(and (cursor-at-end? self ca) (cursor-at-end? self cb)) '=]
      [(cursor-at-end? self ca) '<]
      [(cursor-at-end? self cb) '>]
      [else
       (match-define (cursor ra pa afa) ca)
       (match-define (cursor rb pb afb) cb)
       (define na (- (raw-count self ra) pa))
       (define nb (- (raw-count self rb) pb))
       (define k  (min na nb))
       (define c  (proc (raw-slice self ra pa (+ pa k))
                        (raw-slice self rb pb (+ pb k))))
       (if (not (eq? c '=))
           c
           (loop (if (= k na) (rope->cursor self afa) (cursor ra (+ pa k) afa))
                 (if (= k nb) (rope->cursor self afb) (cursor rb (+ pb k) afb))))])))

(define (rope-compare self a b)
  (rope-compare-with self (λ (x y) (raw-compare self x y)) a b))

(define (rope<?  self a b) (eq? (rope-compare self a b) '<))
(define (rope>?  self a b) (eq? (rope-compare self a b) '>))
(define (rope<=? self a b) (not (eq? (rope-compare self a b) '>)))
(define (rope>=? self a b) (not (eq? (rope-compare self a b) '<)))

;; Collapses the #:fallbacks block's `(define/generic alias generic-name)`
;; boilerplate.
(define-syntax-rule (define-generic-aliases [alias generic-name] ...)
  (begin (define/generic alias generic-name) ...))

(define-generics ropeable
  #:requires (raw-limit raw-empty raw-count raw-width raw-slice raw-append raw-ref
                        rope-leaf-ctor rope-node-ctor)
  ;; Raw
  [raw-limit   ropeable]                ; max element count per leaf
  [raw?        ropeable obj]            ; raw chunk predicate
  [raw-empty   ropeable]                ; construct a raw empty chunk
  [raw-count   ropeable raw]            ; element count of a raw chunk
  [raw-width   ropeable raw]            ; total width width of a raw chunk
  [raw-slice   ropeable raw pos end]    ; extract a raw sub-chunk
  [raw-append  ropeable . raws]         ; concatenate raw chunks
  [raw-ref     ropeable raw pos]        ; used by cursor-peek
  [raw-compare ropeable raw1 raw2]      ; lexicographic comparison (optional - no fallback)
  ;; Rope
  [rope-leaf-ctor    ropeable]
  [rope-node-ctor    ropeable]
  [make-rope-leaf    ropeable raw]
  [make-rope-node    ropeable left right]
  [make-empty-rope   ropeable]
  [rope-concat       ropeable left right]
  [rope-append1      ropeable left right]
  [rope-append       ropeable . ropes]
  [rope-split        ropeable rope i]
  [rope-offset-index ropeable rope ofs]
  [rope-splice       ropeable rope start old-len new-raw]
  [rope-slice        ropeable rope start len]
  [raw->rope         ropeable raw]
  [rope->raw         ropeable rope]
  ;; Cursor
  [cursor-at-end? ropeable cur]
  [cursor-peek    ropeable cur]
  [cursor-advance ropeable cur]
  [cursor-drop    ropeable cur k]
  [cursor-take    ropeable cur k]
  [rope->cursor   ropeable rope]
  [cursor->rope   ropeable cur]
  ;; Cursor-Based Rope Operations
  [rope-foldl ropeable proc init rope0 . ropes]
  [rope-foldr ropeable proc init rope0 . ropes]

  #:fallbacks
  [(define-generic-aliases
     [raw:limit      raw-limit]
     [raw:empty      raw-empty]
     [raw:count      raw-count]
     [raw:width      raw-width]
     [raw:slice      raw-slice]
     [raw:append     raw-append]
     [raw:ref        raw-ref]
     [rope:leaf-ctor rope-leaf-ctor]
     [rope:node-ctor rope-node-ctor])

   (define (make-rope-leaf self raw)
     ((rope:leaf-ctor self) (raw:count self raw) (raw:width self raw) raw))

   (define (make-rope-node self left right)
     ((rope:node-ctor self) (+ (rope-count left) (rope-count right))
                            (+ (rope-width left) (rope-width right))
                            left right))

   (define (make-empty-rope self)
     (make-rope-leaf self (raw:empty self)))

   ;; Naive concatenation. O(1)
   (define (rope-concat self left right)
     (make-rope-node self left right))

   ;; Concatenate naively, then repair balance if the Fibonacci invariant is
   ;; violated. A rebuild is triggered only when depth has drifted ahead of
   ;; what the element count justifies, which cannot happen more than O(log n)
   ;; times across O(n) appends — hence amortized O(log n).
   (define (rope-append1 self left right)
     (cond
       [(zero? (rope-count left))  right]
       [(zero? (rope-count right)) left]
       [else
        (define combined (rope-concat self left right))
        (if (rope-balanced? combined) combined (raw->rope self (rope->raw self combined)))]))

   ;; O(log n * |ropes|)
   (define (rope-append self . ropes)
     (for/fold ([l (make-empty-rope self)]) ([r (in-list ropes)])
       (rope-append1 self l r)))

   ;; Splits at an element index. O(log n) amortized: one descent, plus one
   ;; rope-append1 per level on the way back up.
   (define (rope-split self rope i)
     (cond
       [(rope-leaf? rope)
        (define cnt (rope-leaf-count rope))
        (define raw (rope-leaf-raw   rope))
        (values (make-rope-leaf self (raw:slice self raw 0 i))
                (make-rope-leaf self (raw:slice self raw i cnt)))]
       [else
        (define l  (rope-node-left rope))
        (define r  (rope-node-right rope))
        (define lc (rope-count l))
        (if (<= i lc)
            (let-values ([(ll lr) (rope-split self l i)])
              (values ll (rope-append1 self lr r)))
            (let-values ([(rl rr) (rope-split self r (- i lc))])
              (values (rope-append1 self l rl) rr)))]))

   ;; Finds the leftmost element index containing offset `ofs`. O(log n).
   (define (rope-offset-index self rope ofs)
     (cond
       [(and (rope-leaf? rope) (= (raw:count self (rope-leaf-raw rope)) 1)) 0]
       [(rope-leaf? rope)
        (define cnt (rope-leaf-count rope))
        (define raw (rope-leaf-raw   rope))
        ;; ropeable gives no direct per-element width, but the width of a single-element slice
        ;; serves the same purpose.
        (define (elem-width i) (raw:width self (raw:slice self raw i (add1 i))))
        (let loop ([i 0] [acc 0])
          (if (= i cnt)
              (sub1 i)
              (let ([iw (elem-width i)])
                (if (< ofs (+ acc iw)) i (loop (add1 i) (+ acc iw))))))]
       [else
        (define l  (rope-node-left rope))
        (define lw (rope-width l))
        (if (< ofs lw)
            (rope-offset-index self l ofs)
            (+ (rope-count l) (rope-offset-index self (rope-node-right rope) (- ofs lw))))]))

   ;; Replace `old-len` elements starting at `start` with `new-raw`. O(log n +
   ;; |new-raw|).
   (define (rope-splice self rope start old-len new-raw)
     (define-values (before rest) (rope-split self rope start))
     (define-values (_gone after) (rope-split self rest old-len))
     (rope-append1 self (rope-append1 self before (raw->rope self new-raw)) after))

   ;; Extracts `len` elements starting at `start`. O(log n + # leaves in
   ;; slice) amortized.
   (define (rope-slice self rope start len)
     (define-values (_before rest) (rope-split self rope start))
     (define-values (slice _after) (rope-split self rest len))
     slice)

   ;; O(n) bottom-up balanced build.
   (define (raw->rope self raw)
     (define n (raw:count self raw))
     (if (<= n (raw:limit self))
         (make-rope-leaf self raw)
         (let ([mid   (quotient n 2)])
           (rope-concat self
                        (raw->rope self (raw:slice self raw 0 mid))
                        (raw->rope self (raw:slice self raw mid n))))))

   ;; O(# leaves)
   (define (rope->raw self rope)
     (apply raw:append self (rope-flatten rope)))

   ;; O(1)
   (define (cursor-at-end? self cur)
     (match-define (cursor raw pos after) cur)
     (and (>= pos (raw:count self raw)) (zero? (rope-count after))))

   ;; Returns the element under the cursor, or the first element of the next
   ;; leaf if the cursor's position has run past its own leaf, or #f if there
   ;; is none. O(1) except when crossing a leaf boundary (amortized O(1) over
   ;; the leaf).
   (define (cursor-peek self cur)
     (match-define (cursor raw pos after) cur)
     (cond
       [(< pos (raw:count self raw)) (raw:ref self raw pos)]
       [(zero? (rope-count after)) #f]
       [else (cursor-peek self (rope->cursor self after))])) ; boundary crossing

   ;; Shifts the cursor's position right by one. Same complexity as
   ;; cursor-peek.
   (define (cursor-advance self cur)
     (match-define (cursor raw pos after) cur)
     (define pos+ (add1 pos))
     (if (< pos+ (raw:count self raw))
         (cursor raw pos+ after)
         (rope->cursor self after)))    ; boundary crossing

   ;; Skip k elements at once, by splitting rather than stepping. O(log n),
   ;; independent of k.
   (define (cursor-drop self cur k)
     (if (zero? k)
         cur
         (let-values ([(_skipped after) (rope-split self (cursor->rope self cur) k)])
           (rope->cursor self after))))

   ;; Extracts a `k`-element rope starting at `cur`. O(log n), independent of
   ;; k.
   (define (cursor-take self cur k)
     (if (zero? k)
         (make-empty-rope self)
         (let-values ([(before _after) (rope-split self (cursor->rope self cur) k)])
           before)))

   ;; Descends to the leftmost leaf. O(depth) = O(log n)
   (define (rope->cursor self rope)
     (if (rope-leaf? rope)
         (cursor (rope-leaf-raw rope) 0 (make-empty-rope self))
         (let ()
           (match-define (cursor raw pos after) (rope->cursor self (rope-node-left rope)))
           (cursor raw pos (rope-append1 self after (rope-node-right rope))))))

   ;; Reconstitutes the cursor and everything after it as a single rope —
   ;; needed to implement a large skip in O(log n) rather than O(k) individual
   ;; steps.
   (define (cursor->rope self cur)
     (match-define (cursor raw pos rest) cur)
     (define n (raw:count self raw))
     (rope-append1 self (make-rope-leaf self (raw:slice self raw pos n)) rest))

   ;; O(n). Folds proc left to right over one or more ropes at the same time.
   ;; All ropes must have equal count.
   (define (rope-foldl self proc init rope0 . ropes)
     (check-equal-lengths! 'rope-foldl proc rope0 ropes)
     (let loop ([init init] [curs (map (λ (r) (rope->cursor self r)) (cons rope0 ropes))])
       (if (cursor-at-end? self (car curs))
           init
           (let ([head (map (λ (c) (cursor-peek    self c)) curs)]
                 [tail (map (λ (c) (cursor-advance self c)) curs)])
             (loop (apply proc init head) tail)))))

   ;; O(n). Like rope-foldl, but right to left.
   (define (rope-foldr self proc init rope0 . ropes)
     (check-equal-lengths! 'rope-foldr proc rope0 ropes)
     (let loop ([init init] [curs (map (λ (r) (rope->cursor self r)) (cons rope0 ropes))])
       (if (cursor-at-end? self (car curs))
           init
           (let ([head (map (λ (c) (cursor-peek    self c)) curs)]
                 [tail (map (λ (c) (cursor-advance self c)) curs)])
             (apply proc (loop init tail) head)))))])

;; Shared precondition for rope-foldl/rope-foldr: every rope argument must
;; have the same count.
(define (check-equal-lengths! who proc rope0 ropes)
  (define len1 (rope-count rope0))
  (for ([r (in-list ropes)])
    (define len2 (rope-count r))
    (unless (= len1 len2)
      (raise-arguments-error who "all ropes must have the same length"
                             "first rope length" len1
                             "other rope length" len2
                             "procedure" proc))))
