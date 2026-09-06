#lang racket/base

;; rope/generic-ops.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor
                     syntax/parse)
         racket/sequence
         rope2/cursor
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(begin-for-syntax
  (define-splicing-syntax-class op-args
    #:description "operation arguments"
    ;; Arguments ending with ...
    (pattern (~seq arg:id ... last-arg:id (~datum ...))
             #:with (inner-arg ...) (generate-temporaries #'(arg ...))
             #:with inner-last      (generate-temporary #'last-arg)
             #:with (inner-pattern ...) #'(inner-arg ... inner-last (... ...))
             ;; The left and right sides of the inner #:with clause
             #:with rebind-pattern  #'(arg ... last-arg (... ...))
             #:with rebind-value    #'(inner-arg ... inner-last (... ...)))
    ;; Fixed arity arguments
    (pattern (~seq arg:id ...)
             #:with (inner-arg ...)     (generate-temporaries #'(arg ...))
             #:with (inner-pattern ...) #'(inner-arg ...)
             #:with rebind-pattern      #'(arg ...)
             #:with rebind-value        #'(inner-arg ...))))

(define-syntax-parse-rule (define-rope-operation (op-id:id type-id:id args:op-args) template:expr)
  ;; The first argument of the outer macro (type-id) binds a rope type name at
  ;; definition time, so it can be passed on to other generic rope operations
  ;; from inside the template.
  ;;
  ;; The first argument of the inner macro (ρ) is an expansion-time binder
  ;; that determines which rope type descriptor's components should be
  ;; implicitly bound inside the template.
  ;;
  ;; When define-rope-operation is expanded, the following pattern directive
  ;; sets the name of the inner macro's first argument to whatever type-id is
  ;; bound to.
  #:with ρ (format-id this-syntax (symbol->string (syntax-e #'type-id)))

  ;; Identifiers that are implicitly bound inside the template are declared
  ;; here to inherit the scope of the outer macro invocation.
  #:do [(define (mk-op name) (format-id this-syntax name))]

  ;; per-chunk primitives
  #:with chunk?              (mk-op "chunk?")
  #:with chunk-limit         (mk-op "chunk-limit")
  #:with chunk-empty         (mk-op "chunk-empty")
  #:with chunk-length        (mk-op "chunk-length")
  #:with chunk-width         (mk-op "chunk-width")
  #:with chunk-ref           (mk-op "chunk-ref")
  #:with chunk-slice         (mk-op "chunk-slice")
  #:with chunk-append        (mk-op "chunk-append")
  #:with chunk-compare       (mk-op "chunk-compare")
  #:with chunk-overlap=?     (mk-op "chunk-overlap=?")
  #:with chunk-hash          (mk-op "chunk-hash")

  ;; per-element primitives
  #:with elem-width          (mk-op "elem-width")
  #:with elem-hash           (mk-op "elem-hash")

  ;; per-rope primitives
  #:with leaf-constructor    (mk-op "leaf-constructor")
  #:with node-constructor    (mk-op "node-constructor")
  #:with node-hash           (mk-op "node-hash")
  #:with rope-hashing        (mk-op "rope-hashing")
  #:with content=?           (mk-op "content=?")

  ;; Passing arbitrary user-supplied arguments directly to the inner macro
  ;; definition is not safe because syntax/parse binds _ as the no-bind
  ;; catch-all pattern, so any user-supplied arg named _ will become a
  ;; catch-all pattern for the inner macro. To prevent this, we embed
  ;; temporary identifiers into the inner macro's pattern and then bind them
  ;; back to the original identifiers on the inside.
  #:with inner-ρ (generate-temporary #'type-id)

  (define-syntax-parse-rule (op-id inner-ρ args.inner-pattern ...)
    #:do [(define (raise-op-error msg stx)
            (raise-syntax-error 'op-id msg this-syntax stx))

          (define desc-id (format-id #'inner-ρ "rope:~a" #'inner-ρ))
          (define desc    (syntax-local-value desc-id (λ () #f)))
          (unless desc
            (raise-op-error "expected a rope type descriptor" #'inner-ρ))]

    ;; per-chunk primitives
    #:with chunk?           (rope-type-descriptor-chunk?           desc)
    #:with chunk-limit      (rope-type-descriptor-chunk-limit      desc)
    #:with chunk-empty      (rope-type-descriptor-chunk-empty      desc)
    #:with chunk-length     (rope-type-descriptor-chunk-length     desc)
    #:with chunk-width      (rope-type-descriptor-chunk-width      desc)
    #:with chunk-ref        (rope-type-descriptor-chunk-ref        desc)
    #:with chunk-slice      (rope-type-descriptor-chunk-slice      desc)
    #:with chunk-append     (rope-type-descriptor-chunk-append     desc)
    #:with chunk-compare    (rope-type-descriptor-chunk-compare    desc)
    #:with chunk-overlap=?  (rope-type-descriptor-chunk-overlap=?  desc)
    #:with chunk-hash       (rope-type-descriptor-rope-chunk-hash  desc)

    ;; per-element primitives
    #:with elem-width       (rope-type-descriptor-elem-width       desc)
    #:with elem-hash        (rope-type-descriptor-elem-hash        desc)

    ;; per-rope primitives
    #:with leaf-constructor (rope-type-descriptor-leaf-constructor desc)
    #:with node-constructor (rope-type-descriptor-node-constructor desc)
    #:with node-hash        (rope-type-descriptor-rope-node-hash   desc)
    #:with rope-hashing     (rope-type-descriptor-make-rope-hash   desc)
    #:with content=?        (rope-type-descriptor-content=?        desc)

    ;; Rebind the temporary identifiers to the corresponding originals.
    #:with ρ                   #'inner-ρ
    #:with args.rebind-pattern #'args.rebind-value

    template))

;; -----------------------------------------------------------------------------
;; per-chunk primitives
;; -----------------------------------------------------------------------------

(define-rope-operation (rope-chunk?          _ x)     (chunk?          x))
(define-rope-operation (rope-chunk-limit     _)       (chunk-limit))
(define-rope-operation (rope-chunk-empty     _)       (chunk-empty))
(define-rope-operation (rope-chunk-length    _ c)     (chunk-length    c))
(define-rope-operation (rope-chunk-width     _ c)     (chunk-width     c))
(define-rope-operation (rope-chunk-ref       _ c i)   (chunk-ref       c i))
(define-rope-operation (rope-chunk-slice     _ c i k) (chunk-slice     c i k))
(define-rope-operation (rope-chunk-append    _ cs)    (chunk-append    cs))
(define-rope-operation (rope-chunk-compare   _ c d)   (chunk-compare   c d))
(define-rope-operation (rope-chunk-overlap=? _ c d)   (chunk-overlap=? c d))
(define-rope-operation (rope-chunk-hash      _ c)     (chunk-hash      c))

;; -----------------------------------------------------------------------------
;; per-element primitives
;; -----------------------------------------------------------------------------

(define-rope-operation (rope-elem-width      _ c i)   (elem-width      c i))
(define-rope-operation (rope-elem-hash       _ e)     (elem-hash       e))

;; -----------------------------------------------------------------------------
;; content-based hashing * equality
;; -----------------------------------------------------------------------------

(define-rope-operation (rope-node-hash       _ l r)   (node-hash       l r))
(define-rope-operation (make-rope-hash       _ a)     (rope-hashing    a))
(define-rope-operation (rope-content=?       _ a b)   (content=?       a b))

;; -----------------------------------------------------------------------------
;; smart constructors
;; -----------------------------------------------------------------------------

(define-rope-operation (make-rope-leaf ρ c0)
  (let ([c c0])
    (let-values ([(h p) (rope-chunk-hash ρ c)])
      (leaf-constructor (chunk-length c) (chunk-width c) h p content=? c))))

(define-rope-operation (make-rope-node ρ l0 r0)
  (let ([l l0] [r r0])
    (let-values ([(h p) (rope-node-hash ρ l r)])
      (node-constructor (+ (rope-length l) (rope-length r))
                        (+ (rope-width l) (rope-width r))
                        h p
                        content=?
                        (add1 (max (rope-depth l) (rope-depth r)))
                        l r))))

(define-rope-operation (make-empty-rope ρ) (make-rope-leaf ρ (chunk-empty)))

;; -----------------------------------------------------------------------------
;; conversions
;; -----------------------------------------------------------------------------

(define-rope-operation (chunk->rope ρ c0)
  (let* ([c c0] [total (chunk-length c)])
    (if (<= total (chunk-limit))
        (make-rope-leaf ρ c)
        (let loop ([i 0] [k total])
          (if (<= k (chunk-limit))
              (make-rope-leaf ρ (chunk-slice c i k))
              (let ([mid (quotient k 2)])
                (define l (loop i mid))
                (define r (loop (+ i mid) (- k mid)))
                (rope-concat ρ l r)))))))

(define-rope-operation (rope->chunk _ a) (chunk-append (rope-chunks a)))

;; -----------------------------------------------------------------------------
;; basic operations
;; -----------------------------------------------------------------------------

;; O(1)
(define-rope-operation (rope-concat  ρ l0 r0)
  (let ([l l0] [r r0])
    (cond
      [(zero? (rope-length l)) r]
      [(zero? (rope-length r)) l]
      [else (make-rope-node ρ l r)])))

;; O(log n) amortized
(define-rope-operation (rope-append2 ρ a b) (rope-ensure-balance ρ (rope-concat ρ a b)))

;; O(log n) amortized
(define-rope-operation (rope-append ρ as)
  (rope-ensure-balance ρ
    (for/fold ([l (make-empty-rope ρ)])
              ([r (in-list as)])
      (rope-concat ρ l r))))

;; Splits at an element index, returning the two halves [0, i) and [i, n).
;; O(log n) amortized
(define-rope-operation (rope-split ρ a0 i0)
  (let-values
      ([(l r)
        (let loop ([a a0] [i i0])
          (cond
            [(rope-leaf? a)
             (define chunk (rope-leaf-chunk a))
             (cond
               [(= i 0)               (values (make-empty-rope ρ) a)]
               [(= i (rope-length a)) (values a (make-empty-rope ρ))]
               [else (values (make-rope-leaf ρ (chunk-slice chunk 0 i))
                             (make-rope-leaf ρ (chunk-slice chunk i (- (rope-length a) i))))])]
            [else
             (define l (rope-node-left a))
             (define r (rope-node-right a))
             (define n (rope-length l))
             (cond
               [(<= i n)
                (define-values (ll lr) (loop l i))
                (values ll (rope-concat ρ lr r))]
               [else
                (define-values (rl rr) (loop r (- i n)))
                (values (rope-concat ρ l rl) rr)])]))])
    (values (rope-ensure-balance ρ l)
            (rope-ensure-balance ρ r))))

;; O(log n)
(define-rope-operation (rope-ref _ a0 i0)
  (let loop ([a a0] [i i0])
    (cond
      [(rope-leaf? a) (chunk-ref (rope-leaf-chunk a) i)]
      [else
       (define n (rope-length (rope-node-left a)))
       (if (< i n)
           (loop (rope-node-left a) i)
           (loop (rope-node-right a) (- i n)))])))

;; Finds the left-most element index containing offset p0, clamped to the end
;; of the rope. O(1) if elem-width is a numeric literal, otherwise O(log n)
(define-rope-operation (rope-offset-index ρ a0 p0)
  (let* ([a a0]
         [p p0]
         [n (rope-length a)])
    (if (number? elem-width)
        (min (quotient p elem-width) (sub1 n))
        (let loop ([a a] [p p])
          (if (rope-leaf? a)
              (let chunk-loop ([i 0] [q p])
                (if (= i n)
                    (sub1 n)
                    (let ([k (elem-width (rope-leaf-chunk a) i)])
                      (if (< q k) i (chunk-loop (add1 i) (- q k))))))
              (let ([l (rope-node-left a)])
                (if (< p (rope-width l))
                    (loop l p)
                    (+ (rope-length l) (loop (rope-node-right a) (- p (rope-width l)))))))))))

;; Efficient dual-split variant that throws away the interval [i, i + k).
;; Delays the actual splits until it finds the sub-tree(s) containing the
;; endpoints of the interval, limiting the number of rebalances to three.
;; O(log n) amortized
(define-rope-operation (rope-cut ρ a0 i00 k00)
  (let-values
      ([(l r)
        (let* ([i0 i00] [k0 k00] [j0 (+ i0 k0)])
          (let loop ([a a0] [i (min i0 j0)] [j (max i0 j0)])
            (cond
              [(rope-leaf? a)
               (define chunk (rope-leaf-chunk a))
               (cond
                 [(and (= i 0) (= j i))               (values (make-empty-rope ρ) a)]
                 [(and (= i (rope-length a)) (= j i)) (values a (make-empty-rope ρ))]
                 [else (values (make-rope-leaf ρ (chunk-slice chunk 0 i))
                               (make-rope-leaf ρ (chunk-slice chunk j (- (rope-length a) j))))])]
              [else
               (define l (rope-node-left a))
               (define r (rope-node-right a))
               (define n (rope-length l))
               (cond
                 ;; end of interval must be in the left sub-tree
                 [(<= j n)
                  (define-values (ll lr) (loop l i j))
                  (values ll (rope-concat ρ lr r))]
                 ;; start of interval must be in the right sub-tree
                 [(>= i n)
                  (define-values (rl rr) (loop r (- i n) (- j n)))
                  (values (rope-concat ρ l rl) rr)]
                 ;; interval spans both sub-trees
                 [else
                  (define-values (ll _lr) (rope-split ρ l i))
                  (define-values (_rl rr) (rope-split ρ r (- j n)))
                  (values ll rr)])])))])
    (values (rope-ensure-balance ρ l)
            (rope-ensure-balance ρ r))))

;; The complement of rope-cut. Keeps only the interval [i, i + k). O(log n)
;; amortized
(define-rope-operation (rope-slice ρ a0 i00 k00)
  (rope-ensure-balance ρ
    (let* ([i0 i00] [k0 k00] [j0 (+ i0 k0)])
      (let loop ([a a0] [i (min i0 j0)] [j (max i0 j0)])
        (cond
          [(rope-leaf? a)
           (make-rope-leaf ρ (chunk-slice (rope-leaf-chunk a) i (- j i)))]
          [else
           (define l (rope-node-left a))
           (define r (rope-node-right a))
           (define n (rope-length l))
           (cond
             [(<= j n) (loop l i j)]
             [(>= i n) (loop r (- i n) (- j n))]
             [else
              (define-values (_ll lr) (rope-split ρ l i))
              (define-values (rl _rr) (rope-split ρ r (- j n)))
              (rope-concat ρ lr rl)])])))))

;; Replaces the interval [i, i + k) with b. O(log n) amortized
(define-rope-operation (rope-splice ρ a i k b)
  (rope-ensure-balance ρ
    (let-values ([(l r) (rope-cut ρ a i k)])
      (rope-concat ρ (rope-concat ρ l b) r))))

;; -----------------------------------------------------------------------------
;; balancing operations
;; -----------------------------------------------------------------------------

(define-rope-operation (rope-defrag ρ a) (chunk->rope ρ (rope->chunk ρ a)))

(define-rope-operation (rope-ensure-balance ρ a)
  (if (rope-mostly-balanced? a) a (rope-rebalance ρ a)))

;; Efficient forest-based rope rebuild. O(log n) amortized
(define-rope-operation (rope-rebalance ρ a0)
  (let ([a a0])
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
                [else (rope-concat ρ cur pfx)])))
      (define r (if pfx (rope-concat ρ pfx a) a))
      ;; Cascade upward from slot n.
      (let cascade ([i n] [cur r])
        (define next (vector-ref slots i))
        (if next
            (begin (vector-set! slots i #f)
                   (cascade (add1 i) (rope-concat ρ next cur)))
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
          [else (rope-concat ρ slot-i result)])))

    (if (rope-mostly-balanced? a)
        a
        (begin (traverse a) (collapse)))))

;; -----------------------------------------------------------------------------
;; immutable cursors
;; -----------------------------------------------------------------------------

;; O(depth). O(1) if the rope is not edited
(define-rope-operation (cursor->rope ρ cur)
  (cursor-source cur))

;; O(1)
(define-rope-operation (cursor-peek _ cur0)
  (let ([cur cur0])
    (chunk-ref (rope-leaf-chunk (cursor-leaf cur)) (cursor-rel-idx cur))))

;; O(depth)
(define-rope-operation (cursor-split ρ cur0)
  (let ([cur cur0])
    (define a (cursor-leaf cur))
    (define i (cursor-rel-idx cur))
    (define c (rope-leaf-chunk a))
    (let loop ([l (make-rope-leaf ρ (chunk-slice c 0 i))]
               [r (make-rope-leaf ρ (chunk-slice c i (- (rope-length a) i)))]
               [path (cursor-path cur)])
      (if (null? path)
          (values (rope-ensure-balance ρ l)
                  (rope-ensure-balance ρ r))
          (let ([cb (car path)])
            (if (eq? (crumb-side cb) 'left)
                (loop l (rope-concat ρ r (crumb-right cb)) (cdr path))
                (loop (rope-concat ρ (crumb-left cb) l) r (cdr path))))))))

;; -----------------------------------------------------------------------------
;; mutable cursors
;; -----------------------------------------------------------------------------

(define-rope-operation (mutable-cursor->rope ρ cur)
  (mutable-cursor-source cur))

(define-rope-operation (mutable-cursor-peek _ cur0)
  (let ([cur cur0])
    (chunk-ref (rope-leaf-chunk (mutable-cursor-leaf cur)) (mutable-cursor-rel-idx cur))))

;; -----------------------------------------------------------------------------
;; folds
;; -----------------------------------------------------------------------------

(define (check-same-rope-lengths! name proc a0 as)
  (define n (rope-length a0))
  (for ([a (in-list as)])
    (define m (rope-length a))
    (unless (= m n)
      (raise-arguments-error name "all ropes must have the same length"
                             "first rope length" n
                             "other rope length" m
                             "procedure" proc))))

;; O(n)
(define-rope-operation (rope-foldl ρ proc0 init a00 as0 ...)
  (let ([proc proc0] [a0 a00] [as (list as0 ...)])
    (check-same-rope-lengths! 'rope-foldl proc a0 as)
    (define curs (map rope->mutable-cursor (cons a0 as)))
    (let loop ([result init] [count (rope-length a0)])
      (if (zero? count)
          result
          (let ([head (for/list ([cur (in-list curs)])
                        (begin0 (mutable-cursor-peek ρ cur) (cursor-advance! cur)))])
            (loop (apply proc (append head (list result))) (sub1 count)))))))

;; O(n)
(define-rope-operation (rope-foldr ρ proc0 init a00 as0 ...)
  (let ([proc proc0] [a0 a00] [as (list as0 ...)])
    (check-same-rope-lengths! 'rope-foldr proc a0 as)
    (define curs (map rope->mutable-cursor (cons a0 as)))
    (let loop ([result init] [count (rope-length a0)])
      (if (zero? count)
          result
          (let ([head (for/list ([cur (in-list curs)])
                        (begin0 (mutable-cursor-peek ρ cur) (cursor-advance! cur)))])
            (apply proc (append head (list (loop result (sub1 count))))))))))

;; -----------------------------------------------------------------------------
;; sequences
;; -----------------------------------------------------------------------------

(define-rope-operation (in-rope-runtime ρ a0 i0 j0 k0)
  (let ([a a0] [i i0] [j j0] [k k0])
    (when (zero? k)
      (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
    (define k>0? (> k 0))
    (define stop (or j (if k>0? (rope-length a) -1)))
    (if ((if k>0? < >) stop i)
        (in-list null)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:pos->element       (λ (cur) (mutable-cursor-peek ρ cur))
            #:next-pos           (λ (cur) (cursor-advance! cur k))
            #:init-pos           (rope->mutable-cursor a i)
            #:continue-with-pos? (λ (cur) (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) stop)))))))))

(define-syntax-parse-rule (in-rope-fallback ρ:id a:expr
                            (~optional i:expr #:defaults ([i #'0]))
                            (~optional j:expr #:defaults ([j #'#f]))
                            (~optional k:expr #:defaults ([k #'1])))
  (in-rope-runtime ρ a i j k))

(define-sequence-syntax in-rope
  (λ () #'in-rope-fallback)
  (λ (stx)
    (syntax-parse stx
      [[(x:id) (_ ρ:id a0:expr
                  (~optional i0:expr #:defaults ([i0 #'0]))
                  (~optional j0:expr #:defaults ([j0 #'#f]))
                  (~optional k0:expr #:defaults ([k0 #'1])))]
       #'[(x)
          (:do-in
           ;; Outer bindings (Evaluated exactly once before the loop begins)
           ([(a i k stop k>0?)
             (let ([a a0] [i i0] [j j0] [k k0])
               (define k>0? (> k 0))
               (define stop (or j (if k>0? (rope-length a) -1)))
               (values a i k stop k>0?))])
           ;; Outer checks (Validation rules)
           (begin
             (when (zero? k)
               (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
             (define cur (rope->mutable-cursor a i)))
           ;; Loop bindings
           ()
           ;; Positional guard (Checks if iteration should continue)
           (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) stop))
           ;; Inner bindings (Extracts the current element)
           ([(x) (mutable-cursor-peek ρ cur)])
           ;; Pre-guard
           #t
           ;; Post-guard
           (cursor-advance! cur k)
           ;; Loop updates (Advances the cursor and index for the next iteration)
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

(define-rope-operation (in-cursor-runtime ρ cur00 di0 dj0 k0)
  (let ([cur0 cur00] [di di0] [dj dj0] [k k0])
    (when (zero? k)
      (raise-argument-error 'in-cursor "(and/c exact-integer? (not/c zero?))" k))
    (define k>0? (> k 0))
    (define i (+ (cursor-abs-idx cur0) di))
    (define j (+ (cursor-abs-idx cur0) (or dj (if k>0? (rope-length (cursor-source cur0)) -1))))
    (if ((if k>0? < >) j i)
        (in-list null)
        (make-do-sequence
         (λ ()
           (initiate-sequence
            #:pos->element       (λ (cur) (mutable-cursor-peek ρ cur))
            #:next-pos           (λ (cur) (cursor-advance! cur k))
            #:init-pos           (cursor-advance! (cursor->mutable-cursor cur0) di0)
            #:continue-with-pos? (λ (cur) (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) j)))))))))

(define-syntax-parse-rule (in-cursor-fallback ρ:id cur:expr
                            (~optional i:expr #:defaults ([i #'0]))
                            (~optional j:expr #:defaults ([j #'#f]))
                            (~optional k:expr #:defaults ([k #'1])))
  (in-cursor-runtime ρ cur i j k))

(define-sequence-syntax in-cursor
  (λ () #'in-cursor-fallback)
  (λ (stx)
    (syntax-parse stx
      [[(x:id) (_ ρ:id cur00:expr
                  (~optional di0:expr #:defaults ([di0 #'0]))
                  (~optional dj0:expr #:defaults ([dj0 #'#f]))
                  (~optional k0:expr #:defaults  ([k0  #'1])))]
       #'[(x)
          (:do-in
           ;; Outer bindings (Evaluated exactly once before the loop begins)
           ([(cur0 di i j k k>0?)
             (let ([cur0 cur00] [di di0] [dj dj0] [k k0])
               (define k>0? (> k 0))
               (values cur0 di
                       (+ (cursor-abs-idx cur0) di)
                       (if dj
                           (+ (cursor-abs-idx cur0) dj)
                           (if k>0? (rope-length (cursor-source cur0)) -1))
                       k k>0?))])
           ;; Outer checks (Validation rules)
           (begin
             (when (zero? k)
               (raise-argument-error 'in-rope "(and/c exact-integer? (not/c zero?))" k))
             (define cur (cursor-advance! (cursor->mutable-cursor cur0) di)))
           ;; Loop bindings
           ()
           ;; Positional guard (Checks if iteration should continue)
           (and cur ((if k>0? < >) (mutable-cursor-abs-idx cur) j))
           ;; Inner bindings (Extracts the current element)
           ([(x) (mutable-cursor-peek ρ cur)])
           ;; Pre-guard
           #t
           ;; Post-guard
           (cursor-advance! cur k)
           ;; Loop updates (Advances the cursor and index for the next iteration)
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
