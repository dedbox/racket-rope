#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/sequence
         rope2/cursor
         rope2/generic/define
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; Chunk Operations
;; -----------------------------------------------------------------------------

(define-type-op (rope-chunk? _ x) (*-chunk? x))
(define-type-op (rope-chunk-limit _) (*-chunk-limit))
(define-type-op (rope-chunk-empty _) (*-chunk-empty))
(define-type-op (rope-chunk-length _ c) (*-chunk-length c))
(define-type-op (rope-chunk-width _ c) (*-chunk-width c))
(define-type-op (rope-chunk-ref _ c i) (*-chunk-ref c i))
(define-type-op (rope-chunk-slice _ c i k) (*-chunk-slice c i k))
(define-type-op (rope-chunk-append _ cs) (apply *-chunk-append cs))

;; -----------------------------------------------------------------------------
;; Element Operations
;; -----------------------------------------------------------------------------

(define-type-op (rope-elem-width _ c i) (*-elem-width c i))

;; -----------------------------------------------------------------------------
;; Tree Construction
;; -----------------------------------------------------------------------------

(define-type-op (make-rope-leaf _ c₀)
  (let ([c c₀])
    (define-values (h p) (*-chunk-hash c))
    (*-make-leaf (*-chunk-length c) (*-chunk-width c) h p *-rope-content=? c)))

(define-type-op (make-rope-node _ l₀ r₀)
  (let ([l l₀] [r r₀])
    (define-values (h p) (*-node-hash l r))
    (*-make-node (+ (rope-length l) (rope-length r))
                 (+ (rope-width l) (rope-width r))
                 h p *-rope-content=?
                 (add1 (max (rope-depth l) (rope-depth r)))
                 l r)))

(define-type-op (make-empty-rope τ) (make-rope-leaf τ (*-chunk-empty)))

;; -----------------------------------------------------------------------------
;; Hashing
;; -----------------------------------------------------------------------------

(define-type-op (rope-elem-hash _ x) (*-elem-hash x))
(define-type-op (rope-chunk-hash _ c) (*-chunk-hash c))
(define-type-op (rope-node-hash _ l r) (*-node-hash l r))
(define-type-op (rope-hash _ a₀)
  (let ([a a₀])
    (if (rope-leaf? a)
        (*-chunk-hash (rope-leaf-chunk a))
        (*-node-hash (rope-node-left a) (rope-node-right a)))))

;; -----------------------------------------------------------------------------
;; Equality
;; -----------------------------------------------------------------------------

(define-type-op (rope-chunk=? _ c d) (*-chunk=? c d))
(define-type-op (rope-chunk-overlap=? _ c d ic id k) (*-chunk-overlap=? c d ic id k))
(define-type-op (rope-elem=? _ x y) (*-elem=? x y))
(define-type-op (rope-content=? _ c d) (*-rope-content=? c d))

;; -----------------------------------------------------------------------------
;; Conversions
;; -----------------------------------------------------------------------------

(define-type-op (chunk->rope τ c₀)
  (let* ([c c₀] [total (*-chunk-length c)])
    (if (<= total (*-chunk-limit))
        (make-rope-leaf τ c)
        (let loop ([i 0] [k total])
          (if (<= k (*-chunk-limit))
              (make-rope-leaf τ (*-chunk-slice c i k))
              (let ([mid (quotient k 2)])
                (define l (loop i mid))
                (define r (loop (+ i mid) (- k mid)))
                (rope-concat τ l r)))))))

(define-type-op (rope->chunk _ a) (apply *-chunk-append (rope-chunks a)))

;; -----------------------------------------------------------------------------
;; Basic Operations
;; -----------------------------------------------------------------------------

;; O(1)
(define-type-op (rope-concat τ l₀ r₀)
  (let ([l l₀] [r r₀])
    (cond
      [(zero? (rope-length l)) r]
      [(zero? (rope-length r)) l]
      [else (make-rope-node τ l r)])))

;; O(log n) amortized
(define-type-op (rope-append2 τ a b) (rope-ensure-balance τ (rope-concat τ a b)))

;; O(log n) amortized
(define-type-op (rope-append τ as)
  (rope-ensure-balance τ
    (for/fold ([l (make-empty-rope τ)])
              ([r (in-list as)])
      (rope-concat τ l r))))

;; Splits at an element index, returning the two halves [0, i) and [i, n).
;; O(log n) amortized
(define-type-op (rope-split τ a₀ i₀)
  (let-values
      ([(l r)
        (let loop ([a a₀] [i i₀])
          (cond
            [(rope-leaf? a)
             (define chunk (rope-leaf-chunk a))
             (cond
               [(= i 0)
                (values (make-empty-rope τ) a)]
               [(= i (rope-length a))
                (values a (make-empty-rope τ))]
               [else
                (values (make-rope-leaf τ (*-chunk-slice chunk 0 i))
                        (make-rope-leaf τ (*-chunk-slice chunk i (- (rope-length a) i))))])]
            [else
             (define l (rope-node-left a))
             (define r (rope-node-right a))
             (define n (rope-length l))
             (cond
               [(<= i n)
                (define-values (ll lr) (loop l i))
                (values ll (rope-concat τ lr r))]
               [else
                (define-values (rl rr) (loop r (- i n)))
                (values (rope-concat τ l rl) rr)])]))])
    (values (rope-ensure-balance τ l)
            (rope-ensure-balance τ r))))

;; O(log n)
(define-type-op (rope-ref _ a₀ i₀)
  (let loop ([a a₀] [i i₀])
    (cond
      [(rope-leaf? a) (*-chunk-ref (rope-leaf-chunk a) i)]
      [else
       (define n (rope-length (rope-node-left a)))
       (if (< i n)
           (loop (rope-node-left a) i)
           (loop (rope-node-right a) (- i n)))])))

;; Finds the left-most element index containing offset p0, clamped to the end
;; of the rope. O(1) if elem-width is a numeric literal, otherwise O(log n)
(define-type-op (rope-offset-index τ a₀ p₀)
  (let* ([a a₀]
         [p p₀]
         [n (rope-length a)])
    (if (number? *-elem-width)
        (min (quotient p *-elem-width) (sub1 n))
        (let loop ([a a] [p p])
          (if (rope-leaf? a)
              (let chunk-loop ([i 0] [q p])
                (if (= i n)
                    (sub1 n)
                    (let ([k (*-elem-width (rope-leaf-chunk a) i)])
                      (if (< q k) i (chunk-loop (add1 i) (- q k))))))
              (let ([l (rope-node-left a)])
                (if (< p (rope-width l))
                    (loop l p)
                    (+ (rope-length l) (loop (rope-node-right a) (- p (rope-width l)))))))))))

;; Efficient dual-split variant that throws away the interval [i, i + k).
;; Delays the actual splits until it finds the sub-tree(s) containing the
;; endpoints of the interval, limiting the number of rebalances to three.
;; O(log n) amortized
(define-type-op (rope-cut τ a₀ i₀′ k₀′)
  (let-values
      ([(l r)
        (let* ([i₀ i₀′] [k₀ k₀′] [j₀ (+ i₀ k₀)])
          (let loop ([a a₀] [i (min i₀ j₀)] [j (max i₀ j₀)])
            (cond
              [(rope-leaf? a)
               (define chunk (rope-leaf-chunk a))
               (cond
                 [(and (= i 0) (= j i))
                  (values (make-empty-rope τ) a)]
                 [(and (= i (rope-length a)) (= j i))
                  (values a (make-empty-rope τ))]
                 [else
                  (values (make-rope-leaf τ (*-chunk-slice chunk 0 i))
                          (make-rope-leaf τ (*-chunk-slice chunk j (- (rope-length a) j))))])]
              [else
               (define l (rope-node-left a))
               (define r (rope-node-right a))
               (define n (rope-length l))
               (cond
                 ;; end of interval must be in the left sub-tree
                 [(<= j n)
                  (define-values (ll lr) (loop l i j))
                  (values ll (rope-concat τ lr r))]
                 ;; start of interval must be in the right sub-tree
                 [(>= i n)
                  (define-values (rl rr) (loop r (- i n) (- j n)))
                  (values (rope-concat τ l rl) rr)]
                 ;; interval spans both sub-trees
                 [else
                  (define-values (ll _lr) (rope-split τ l i))
                  (define-values (_rl rr) (rope-split τ r (- j n)))
                  (values ll rr)])])))])
    (values (rope-ensure-balance τ l)
            (rope-ensure-balance τ r))))

;; The complement of rope-cut. Keeps only the interval [i, i + k). O(log n)
;; amortized
(define-type-op (rope-slice τ a₀ i₀′ k₀′)
  (rope-ensure-balance τ
    (let* ([i₀ i₀′] [k₀ k₀′] [j₀ (+ i₀ k₀)])
      (let loop ([a a₀] [i (min i₀ j₀)] [j (max i₀ j₀)])
        (cond
          [(rope-leaf? a)
           (make-rope-leaf τ (*-chunk-slice (rope-leaf-chunk a) i (- j i)))]
          [else
           (define l (rope-node-left a))
           (define r (rope-node-right a))
           (define n (rope-length l))
           (cond
             [(<= j n) (loop l i j)]
             [(>= i n) (loop r (- i n) (- j n))]
             [else
              (define-values (_ll lr) (rope-split τ l i))
              (define-values (rl _rr) (rope-split τ r (- j n)))
              (rope-concat τ lr rl)])])))))

;; Replaces the interval [i, i + k) with b. O(log n) amortized
(define-type-op (rope-splice τ a i k b)
  (rope-ensure-balance τ
    (let-values ([(l r) (rope-cut τ a i k)])
      (rope-concat τ (rope-concat τ l b) r))))

;; -----------------------------------------------------------------------------
;; Tree Balancing
;; -----------------------------------------------------------------------------

(define-type-op (rope-defrag τ a) (chunk->rope τ (rope->chunk τ a)))

(define-type-op (rope-ensure-balance τ a)
  (if (rope-mostly-balanced? a) a (rope-rebalance τ a)))

;; Efficient forest-based rope rebuild. O(log n) amortized
(define-type-op (rope-rebalance τ a₀)
  (let ([a a₀])
    (define slots (make-vector (add1 max-fib-index) #f))

    (define (target-slot len)
      ;; A rope with len elements is too large for the current slot if len
      ;; falls beyond the interval [Fₙ, Fₙ₊₁). Thus, we move on to the next
      ;; slot if len ≥ Fₙ₊₁.
      ;;
      ;; Since i = n - 2 ⇒ n = i + 2 (see the comment in rope.rkt), we have
      ;;
      ;;    len ≥ F₍ᵢ₊₂₎₊₁ = Fᵢ₊₃.
      ;;
      (let loop ([i 0])
        (if (>= len (fib-bound (+ i 3))) (loop (add1 i)) i)))

    (define (insert! a)
      (define n (target-slot (rope-length a)))
      ;; Consolidate any occupied slots [0, n) into one prefix, oldest-first.
      (define pfx
        (for/fold ([pfx #f]) ([i (in-range n)])
          (define cur (vector-ref slots i))
          (when cur (vector-set! slots i #f))
          (cond [(not cur) pfx]
                [(not pfx) cur]
                [else (rope-concat τ cur pfx)])))
      (define r (if pfx (rope-concat τ pfx a) a))
      ;; Cascade upward from slot n.
      (let cascade ([i n] [cur r])
        (define next (vector-ref slots i))
        (if next
            (begin (vector-set! slots i #f)
                   (cascade (add1 i) (rope-concat τ next cur)))
            (vector-set! slots i cur))))

    (define (traverse a)
      (if (or (rope-leaf? a) (rope-strictly-balanced? a)) ; must use strict balance here
          (insert! a)
          (begin (traverse (rope-node-left a))
                 (traverse (rope-node-right a)))))

    ;; Concatenate on the left, from smallest to largest slot.
    (define (collapse)
      (for/fold ([result #f]) ([i (in-range max-fib-index)])
        (define slot-i (vector-ref slots i))
        (cond
          [(not slot-i) result]
          [(not result) slot-i]
          [else (rope-concat τ slot-i result)])))

    (if (rope-mostly-balanced? a)
        a
        (begin (traverse a) (collapse)))))

;; -----------------------------------------------------------------------------
;; immutable cursors
;; -----------------------------------------------------------------------------

;; O(depth). O(1) if the rope is not edited
(define-type-op (cursor->rope τ cur) (cursor-source cur))

;; O(1)
(define-type-op (cursor-peek _ cur₀)
  (let ([cur cur₀])
    (*-chunk-ref (rope-leaf-chunk (cursor-leaf cur)) (cursor-rel-idx cur))))

;; O(depth)
(define-type-op (cursor-split τ cur₀)
  (let ([cur cur₀])
    (define a (cursor-leaf cur))
    (define i (cursor-rel-idx cur))
    (define c (rope-leaf-chunk a))
    (let loop ([l (make-rope-leaf τ (*-chunk-slice c 0 i))]
               [r (make-rope-leaf τ (*-chunk-slice c i (- (rope-length a) i)))]
               [path (cursor-path cur)])
      (if (null? path)
          (values (rope-ensure-balance τ l)
                  (rope-ensure-balance τ r))
          (let ([cb (car path)])
            (if (eq? (crumb-side cb) 'left)
                (loop l (rope-concat τ r (crumb-right cb)) (cdr path))
                (loop (rope-concat τ (crumb-left cb) l) r (cdr path))))))))

;; -----------------------------------------------------------------------------
;; mutable cursors
;; -----------------------------------------------------------------------------

(define-type-op (mutable-cursor->rope τ cur)
  (mutable-cursor-source cur))

(define-type-op (mutable-cursor-peek _ cur0)
  (let ([cur cur0])
    (*-chunk-ref (rope-leaf-chunk (mutable-cursor-leaf cur)) (mutable-cursor-rel-idx cur))))

;; -----------------------------------------------------------------------------
;; folds
;; -----------------------------------------------------------------------------

(define (check-same-rope-lengths! name proc a₀ as)
  (define n (rope-length a₀))
  (for ([a (in-list as)])
    (define m (rope-length a))
    (unless (= m n)
      (raise-arguments-error name "all ropes must have the same length"
                             "first rope length" n
                             "other rope length" m
                             "procedure" proc))))

;; O(n)
(define-type-op (rope-foldl τ proc₀ init a₀′ as₀ ...)
  (let ([proc proc₀] [a₀ a₀′] [as (list as₀ ...)])
    (check-same-rope-lengths! 'rope-foldl proc a₀ as)
    (define curs (map rope->mutable-cursor (cons a₀ as)))
    (let loop ([result init] [count (rope-length a₀)])
      (if (zero? count)
          result
          (let ([head (for/list ([cur (in-list curs)])
                        (begin0 (mutable-cursor-peek τ cur) (cursor-advance! cur)))])
            (loop (apply proc (append head (list result))) (sub1 count)))))))

;; O(n)
(define-type-op (rope-foldr τ proc₀ init a₀′ as₀ ...)
  (let ([proc proc₀] [a₀ a₀′] [as (list as₀ ...)])
    (check-same-rope-lengths! 'rope-foldr proc a₀ as)
    (define curs (map rope->mutable-cursor (cons a₀ as)))
    (let loop ([result init] [count (rope-length a₀)])
      (if (zero? count)
          result
          (let ([head (for/list ([cur (in-list curs)])
                        (begin0 (mutable-cursor-peek τ cur) (cursor-advance! cur)))])
            (apply proc (append head (list (loop result (sub1 count))))))))))

;; -----------------------------------------------------------------------------
;; sequences
;; -----------------------------------------------------------------------------

(define-type-op (in-rope-runtime τ a₀ i₀ j₀ k₀)
  (let ([a a₀] [i i₀] [j j₀] [k k₀])
    (when (zero? k)
      (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
    (define k>0? (> k 0))
    (define stop (or j (if k>0? (rope-length a) -1)))
    (if ((if k>0? < >) stop i)
        (in-list null)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:pos->element       (λ (cur) (mutable-cursor-peek τ cur))
            #:next-pos           (λ (cur) (cursor-advance! cur k))
            #:init-pos           (rope->mutable-cursor a i)
            #:continue-with-pos? (λ (cur) (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) stop)))))))))

(define-syntax-parse-rule (in-rope-fallback τ:id a:expr
                            (~optional i:expr #:defaults ([i #'0]))
                            (~optional j:expr #:defaults ([j #'#f]))
                            (~optional k:expr #:defaults ([k #'1])))
  (in-rope-runtime τ a i j k))

(define-sequence-syntax in-rope
  (λ () #'in-rope-fallback)
  (λ (stx)
    (syntax-parse stx
      [[(x:id) (_ τ:id a₀:expr
                  (~optional i₀:expr #:defaults ([i₀ #'0]))
                  (~optional j₀:expr #:defaults ([j₀ #'#f]))
                  (~optional k₀:expr #:defaults ([k₀ #'1])))]
       #'[(x)
          (:do-in
           ;; Outer bindings (Evaluated exactly once before the loop begins)
           ([(a i k stop k>0?)
             (let ([a a₀] [i i₀] [j j₀] [k k₀])
               (define k>0? (> k 0))
               (define stop (or j (if k>0? (rope-length a) -1)))
               (values a i k stop k>0?))])
           ;; Outer checks (Validation rules)
           (begin
             (when (zero? k)
               (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
             (define cur (and ((if k>0? < >) i stop) (rope->mutable-cursor a i))))
           ;; Loop bindings
           ()
           ;; Positional guard (Checks if iteration should continue)
           (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) stop))
           ;; Inner bindings (Extracts the current element)
           ([(x) (mutable-cursor-peek τ cur)])
           ;; Pre-guard
           #t
           ;; Post-guard
           (cursor-advance! cur k)
           ;; Loop updates
           [])]])))

(define-syntax (define-rope-sequence stx)
  (syntax-parse stx
    [(_ seq-id:id type-id:id)
     #'(define-sequence-syntax seq-id
         (λ () #'(λ (a [i 0] [j #f] [k 1]) (in-rope-fallback type-id a i j k)))
         (λ (inner-stx)
           (syntax-parse inner-stx
             [[(id:id) (_ a:expr
                          (~optional i:expr #:defaults ([i #'0]))
                          (~optional j:expr #:defaults ([j #'#f]))
                          (~optional k:expr #:defaults ([k #'1])))]
              #'[(id) (in-rope type-id a i j k)]])))]))

(define-type-op (in-cursor-runtime τ cur₀′ di₀ dj₀ k₀)
  (let ([cur₀ cur₀′] [di di₀] [dj dj₀] [k k₀])
    (when (zero? k)
      (raise-argument-error 'in-cursor "(and/c exact-integer? (not/c zero?))" k))
    (define k>0? (> k 0))
    (define i (+ (cursor-abs-idx cur₀) di))
    (define j (+ (cursor-abs-idx cur₀) (or dj (if k>0? (rope-length (cursor-source cur₀)) -1))))
    (if ((if k>0? < >) j i)
        (in-list null)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:pos->element       (λ (cur) (mutable-cursor-peek τ cur))
            #:next-pos           (λ (cur) (cursor-advance! cur k))
            #:init-pos           (cursor-advance! (cursor->mutable-cursor cur₀) di₀)
            #:continue-with-pos? (λ (cur) (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) j)))))))))

(define-syntax-parse-rule (in-cursor-fallback τ:id cur:expr
                            (~optional i:expr #:defaults ([i #'0]))
                            (~optional j:expr #:defaults ([j #'#f]))
                            (~optional k:expr #:defaults ([k #'1])))
  (in-cursor-runtime τ cur i j k))

(define-sequence-syntax in-cursor
  (λ () #'in-cursor-fallback)
  (λ (stx)
    (syntax-parse stx
      [[(x:id) (_ τ:id cur₀′:expr
                  (~optional di₀:expr #:defaults ([di₀ #'0]))
                  (~optional dj₀:expr #:defaults ([dj₀ #'#f]))
                  (~optional k₀:expr #:defaults ([k₀  #'1])))]
       #'[(x)
          (:do-in
           ;; Outer bindings (Evaluated exactly once before the loop begins)
           ([(cur₀ di i j k k>0?)
             (let ([cur₀ cur₀′] [di di₀] [dj dj₀] [k k₀])
               (define k>0? (> k 0))
               (values cur₀ di
                       (+ (cursor-abs-idx cur₀) di)
                       (if dj
                           (+ (cursor-abs-idx cur₀) dj)
                           (if k>0? (rope-length (cursor-source cur₀)) -1))
                       k k>0?))])
           ;; Outer checks (Validation rules)
           (begin
             (when (zero? k)
               (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
             (define cur (and ((if k>0? < >) i j)
                              (cursor-advance! (cursor->mutable-cursor cur₀) di))))
           ;; Loop bindings
           ()
           ;; Positional guard (Checks if iteration should continue)
           (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) j))
           ;; Inner bindings (Extracts the current element)
           ([(x) (mutable-cursor-peek τ cur)])
           ;; Pre-guard
           #t
           ;; Post-guard
           (cursor-advance! cur k)
           ;; Loop updates
           [])]])))

(define-syntax (define-cursor-sequence stx)
  (syntax-parse stx
    [(_ seq-id:id type-id:id)
     #'(define-sequence-syntax seq-id
         (λ () #'(λ (a [i 0] [j #f] [k 1]) (in-cursor-fallback type-id a i j k)))
         (λ (inner-stx)
           (syntax-parse inner-stx
             [[(id:id) (_ a:expr
                          (~optional i:expr #:defaults ([i #'0]))
                          (~optional j:expr #:defaults ([j #'#f]))
                          (~optional k:expr #:defaults ([k #'1])))]
              #'[(id) (in-cursor type-id a i j k)]])))]))
