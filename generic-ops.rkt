#lang racket/base

;; rope/generic-ops.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor
                     syntax/parse)
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-operation (op-id:id type-id:id arg:id ...) template:expr)
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
  #:with inner-ρ         (generate-temporary #'type-id)
  #:with (inner-arg ...) (generate-temporaries #'(arg ...))

  (define-syntax-parse-rule (op-id inner-ρ inner-arg ...)
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
    #:with ρ         #'inner-ρ
    #:with (arg ...) #'(inner-arg ...)

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

(define-rope-operation (make-rope-leaf ρ c)
  (let-values ([(h p) (rope-chunk-hash ρ c)])
    (leaf-constructor (chunk-length c) (chunk-width c) h p content=? c)))

(define-rope-operation (make-rope-node ρ l r)
  (let-values ([(h p) (rope-node-hash ρ l r)])
    (node-constructor (+ (rope-length l) (rope-length r))
                      (+ (rope-width l) (rope-width r))
                      h p
                      content=?
                      (add1 (max (rope-depth l) (rope-depth r)))
                      l r)))

(define-rope-operation (make-empty-rope ρ) (make-rope-leaf ρ (chunk-empty)))

;; -----------------------------------------------------------------------------
;; conversions
;; -----------------------------------------------------------------------------

(define-rope-operation (chunk->rope ρ c)
  (let ([total (chunk-length c)])
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

(define-rope-operation (rope-concat  ρ l r)
  (cond
    [(zero? (rope-length l)) r]
    [(zero? (rope-length r)) l]
    [else (make-rope-node ρ l r)]))

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
  (let ([n (rope-length a0)])
    (if (number? elem-width)
        (min (quotient p0 elem-width) (sub1 n))
        (let loop ([a a0] [p p0])
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
(define-rope-operation (rope-cut ρ a0 i0 k0)
  (let-values
      ([(l r)
        (let loop ([a a0] [i i0] [j (+ i0 k0)])
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
               ;; (end of) interval must be in the left sub-tree
               [(<= j n)
                (define-values (ll lr) (loop l i j))
                (values ll (rope-concat ρ lr r))]
               ;; (start of) interval must be in the right sub-tree
               [(>= i n)
                (define-values (rl rr) (loop r (- i n) (- j n)))
                (values (rope-concat ρ l rl) rr)]
               ;; interval touches both sub-trees
               [else
                (define-values (ll _lr) (rope-split ρ l i))
                (define-values (_rl rr) (rope-split ρ r (- j n)))
                (values ll rr)])]))])
    (values (rope-ensure-balance ρ l)
            (rope-ensure-balance ρ r))))

;; The complement of rope-cut. Keeps only the interval [i, i + k). O(log n)
;; amortized
(define-rope-operation (rope-slice ρ a0 i0 k0)
  (rope-ensure-balance ρ
    (let loop ([a a0] [i i0] [j (+ i0 k0)])
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
            (rope-concat ρ lr rl)])]))))

;; Replaces the interval [i, i + k) with b. O(log n) amortized
(define-rope-operation (rope-splice ρ a0 i k b)
  (rope-ensure-balance ρ
    (let-values ([(l r) (rope-cut ρ a0 i k)])
      (rope-concat ρ (rope-concat ρ l b) r))))

;; -----------------------------------------------------------------------------
;; balancing operations
;; -----------------------------------------------------------------------------

(define-rope-operation (rope-defrag ρ a) (chunk->rope ρ (rope->chunk ρ a)))

(define-rope-operation (rope-ensure-balance ρ a)
  (if (rope-mostly-balanced? a) a (rope-rebalance ρ a)))

;; Efficient forest-based rope rebuild. O(log n) amortized
(define-rope-operation (rope-rebalance ρ a0)
  (let ()
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

    (define (insert slots0 a)
      (define n (target-slot (rope-length a)))
      ;; Consolidate any occupied slots [0, n) into one prefix, oldest-first.
      (define-values (pfx slots)
        (for/fold ([pfx #f] [slots slots0])
                  ([i (in-range n)])
          (define cur (hash-ref slots i #f))
          (values (cond [(not cur) pfx]
                        [(not pfx) cur]
                        [else (rope-concat ρ cur pfx)])
                  (if cur (hash-remove slots i) slots))))
      (define r (if pfx (rope-concat ρ pfx a) a))
      ;; Cascade upward from slot n.
      (let cascade ([i n] [cur r] [slots slots])
        (define next (hash-ref slots i #f))
        (if next
            (cascade (add1 i) (rope-concat ρ next cur) (hash-remove slots i))
            (hash-set slots i cur))))

    (define (traverse a slots)
      (cond
        ;; must use strict balance here
        [(or (rope-leaf? a) (rope-strictly-balanced? a))
         (insert slots a)]
        [else
         (traverse (rope-node-right a) (traverse (rope-node-left a) slots))]))

    ;; Concatenate on the left, from smallest to largest slot.
    (define (collapse slots)
      (for/fold ([result #f])
                ([i (in-range max-fib-index)])
        (define slot-i (hash-ref slots i #f))
        (cond
          [(not slot-i) result]
          [(not result) slot-i]
          [else (rope-concat ρ slot-i result)])))

    (if (rope-mostly-balanced? a0) a0 (collapse (traverse a0 (hasheqv))))))
