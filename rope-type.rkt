#lang racket/base

;; rope/rope-type.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/private/rope-type-classes
                     rope2/rope-type-descriptor
                     syntax/parse)
         racket/sequence
         rope2/cursor
         rope2/generic-ops
         rope2/private/hash
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-type type-id:id
                            #:chunk?       chunk?:id+fun1
                            #:chunk-limit  chunk-limit:nat+id+fun0
                            #:chunk-empty  chunk-empty:lit+id+fun0
                            #:chunk-length chunk-length:id+fun1
                            #:chunk-ref    chunk-ref:id+fun2
                            #:chunk-slice  chunk-slice:id+fun3
                            #:chunk-append chunk-append:id+fun1
                            (~optional (~seq #:chunk-compare   chunk-compare:id+fun2))
                            (~optional (~seq #:chunk-overlap=? chunk-overlap=?:id+fun5))
                            #:elem-width   elem-width:nat+id+fun2
                            (~optional (~seq #:elem-hash elem-hash:id+fun1)))
  #:do [(define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))]

  ;; rope type descriptor
  #:with (~var rope:*)              (mk* "rope:~a")

  ;; per-chunk primitives
  #:with *-rope-chunk?              (mk* "~a-rope-chunk?")
  #:with *-rope-chunk-limit         (mk* "~a-rope-chunk-limit")
  #:with *-rope-chunk-empty         (mk* "~a-rope-chunk-empty")
  #:with *-rope-chunk-length        (mk* "~a-rope-chunk-length")
  #:with *-rope-chunk-width         (mk* "~a-rope-chunk-width")
  #:with *-rope-chunk-ref           (mk* "~a-rope-chunk-ref")
  #:with *-rope-chunk-slice         (mk* "~a-rope-chunk-slice")
  #:with *-rope-chunk-append        (mk* "~a-rope-chunk-append")
  #:with *-rope-chunk-compare       (mk* "~a-rope-chunk-compare")
  #:with *-rope-chunk-overlap=?     (mk* "~a-rope-chunk-overlap=?")

  ;; per-element primitives
  #:with *-rope-elem-width          (mk* "~a-rope-elem-width")
  #:with *-rope-elem-hash           (mk* "~a-rope-elem-hash")

  ;; per-rope primitives
  #:with *-rope-chunk-hash          (mk* "~a-rope-chunk-hash")
  #:with *-rope-node-hash           (mk* "~a-rope-node-hash")

  ;; rope structs
  #:with *-rope-equal+hash-impl     (mk* "~a-rope-equal+hash-impl")
  #:with *-rope-leaf                (mk* "~a-rope-leaf")
  #:with *-rope-node                (mk* "~a-rope-node")

  ;; rope predicates
  #:with *-rope-leaf?               (mk* "~a-rope-leaf?")
  #:with *-rope-node?               (mk* "~a-rope-node?")
  #:with *-rope?                    (mk* "~a-rope?")

  ;; content-based hashing & equality
  #:with make-*-rope-hash           (mk* "make-~a-rope-hash")
  #:with *-rope-content=?           (mk* "~a-rope-content=?")

  ;; smart constructors
  #:with make-*-rope-leaf           (mk* "make-~a-rope-leaf")
  #:with make-*-rope-node           (mk* "make-~a-rope-node")
  #:with make-empty-*-rope          (mk* "make-empty-~a-rope")

  ;; conversions
  #:with *-chunk->rope              (mk* "~a-chunk->rope")
  #:with *-rope->chunk              (mk* "~a-rope->chunk")

  ;; basic operations
  #:with *-rope-concat              (mk* "~a-rope-concat")
  #:with *-rope-append2             (mk* "~a-rope-append2")
  #:with *-rope-append              (mk* "~a-rope-append")
  #:with *-rope-split               (mk* "~a-rope-split")
  #:with *-rope-ref                 (mk* "~a-rope-ref")
  #:with *-rope-offset-index        (mk* "~a-rope-offset-index")
  #:with *-rope-cut                 (mk* "~a-rope-cut")
  #:with *-rope-slice               (mk* "~a-rope-slice")
  #:with *-rope-splice              (mk* "~a-rope-splice")

  ;; immutable cursors
  #:with cursor->*-rope             (mk* "cursor->~a-rope")
  #:with *-cursor-peek              (mk* "~a-cursor-peek")
  #:with *-cursor-split             (mk* "~a-cursor-split")

  ;; mutable cursors
  #:with mutable-cursor->*-rope     (mk* "mutable-cursor->~a-rope")
  #:with *-mutable-cursor-peek      (mk* "~a-mutable-cursor-peek")

  ;; folds
  #:with *-rope-foldl               (mk* "~a-rope-foldl")
  #:with *-rope-foldr               (mk* "~a-rope-foldr")

  ;; sequences
  #:with in-*-rope                  (mk* "in-~a-rope")
  #:with in-*-cursor                (mk* "in-~a-cursor")

  (begin

    ;; -------------------------------------------------------------------------
    ;; rope type descriptor
    ;; -------------------------------------------------------------------------

    (define-syntax rope:*
      (rope-type-descriptor #'*-rope-chunk?
                            #'*-rope-chunk-limit
                            #'*-rope-chunk-empty
                            #'*-rope-chunk-length
                            #'*-rope-chunk-width
                            #'*-rope-chunk-ref
                            #'*-rope-chunk-slice
                            #'*-rope-chunk-append
                            #'*-rope-chunk-compare
                            #'*-rope-chunk-overlap=?
                            #'*-rope-elem-width
                            #'*-rope-elem-hash
                            #'*-rope-leaf
                            #'*-rope-node
                            #'*-rope-chunk-hash
                            #'*-rope-node-hash
                            #'make-*-rope-hash
                            #'*-rope-content=?
                            ))

    ;; -------------------------------------------------------------------------
    ;; per-chunk primitives
    ;; -------------------------------------------------------------------------

    (define (*-rope-chunk-limit)        (chunk-limit.callable))
    (define (*-rope-chunk-empty)        (chunk-empty.callable))
    (define (*-rope-chunk?       x)     (chunk?       x))
    (define (*-rope-chunk-length c)     (chunk-length c))
    (define (*-rope-chunk-ref    c i)   (chunk-ref    c i))
    (define (*-rope-chunk-slice  c i k) (chunk-slice  c i k))
    (define (*-rope-chunk-append cs)    (chunk-append cs))

    (define *-rope-chunk-width
      (if (number? elem-width)
          (λ (c) (* (chunk-length c) elem-width))
          (λ (c) (for/sum ([i (in-range (chunk-length c))]) (elem-width c i)))))

    (define *-rope-chunk-compare
      (~? (λ (a b) (chunk-compare a b))
          (λ _ (error '*-rope-chunk-compare "operation not defined"))))

    ;; The default overlap equality check loops over the elements in a chunk.
    ;; This is generally faster for strings but slower for bytes.
    (define *-rope-chunk-overlap=?
      (λ (ac bc ap bp k)
        (~? (chunk-overlap=? ac bc ap bp k)
            (let loop ([i 0])
              (or (= i k) (and (equal? (chunk-ref ac (+ ap i))
                                       (chunk-ref bc (+ bp i)))
                               (loop (add1 i))))))))

    ;; -------------------------------------------------------------------------
    ;; per-element primitives
    ;; -------------------------------------------------------------------------

    (define (*-rope-elem-width c i) (elem-width c i))
    (define (*-rope-elem-hash  c)   (~? (elem-hash c) (equal-hash-code c)))

    ;; -------------------------------------------------------------------------
    ;; rope structs & predicates
    ;; -------------------------------------------------------------------------

    (define *-rope-equal+hash-impl
      (list (λ (a b _) (*-rope-content=? a b))
            (λ (a _) (rope-hash1 a))
            (λ (a _) (rope-hash2 a))))

    (struct *-rope-leaf rope-leaf () #:transparent)
    (struct *-rope-node rope-node () #:transparent)

    (define (*-rope? x) (or (*-rope-leaf? x) (*-rope-node? x)))

    ;; -------------------------------------------------------------------------
    ;; smart constructors
    ;; -------------------------------------------------------------------------

    (define (make-*-rope-leaf c)   (make-rope-leaf  type-id c))
    (define (make-*-rope-node l r) (make-rope-node  type-id l r))
    (define (make-empty-*-rope)    (make-empty-rope type-id))

    ;; -------------------------------------------------------------------------
    ;; conversions
    ;; -------------------------------------------------------------------------

    (define (*-chunk->rope c) (chunk->rope type-id c))
    (define (*-rope->chunk a) (rope->chunk type-id a))

    ;; -------------------------------------------------------------------------
    ;; content-based hashing & equality
    ;; -------------------------------------------------------------------------

    (define (*-rope-chunk-hash c)
      ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
      (define n   (chunk-length c))
      (define n/4 (quotient n 4))
      (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
        (cond
          [(< j n/4)
           (define base (* j 4))
           (define e0 (bitwise-and (*-rope-elem-hash (chunk-ref c base))          M))
           (define e1 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 1)))  M))
           (define e2 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 2)))  M))
           (define e3 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 3)))  M))
           (loop (+ j 1)
                 (fxmodulo-M (+ h0 (* e0 q)))
                 (fxmodulo-M (+ h1 (* e1 q)))
                 (fxmodulo-M (+ h2 (* e2 q)))
                 (fxmodulo-M (+ h3 (* e3 q)))
                 (fxmodulo-M (* q X⁴)))]
          [else
           (define h (fxmodulo-M (+ h0 (* X (fxmodulo-M (+ h1 (* X (fxmodulo-M (+ h2 (* X h3))))))))))
           (let tail ([i (* n/4 4)] [h h] [p q])
             (if (= i n)
                 (values h p)
                 (let ([e (bitwise-and (*-rope-elem-hash (chunk-ref c i)) M)])
                   (tail (+ i 1)
                         (fxmodulo-M (+ h (* e p)))
                         (fxmodulo-M (* p X))))))])))

    (define (*-rope-node-hash l r)
      (define hl (rope-hash1 l))
      (define pl (rope-hash2 l))
      (define hr (rope-hash1 r))
      (define pr (rope-hash2 r))
      (values (fxmodulo-M (+ hl (* pl hr)))
              (fxmodulo-M (* pl pr))))

    (define (make-*-rope-hash a)
      (if (rope-leaf? a)
          (*-rope-chunk-hash (rope-leaf-chunk a))
          (*-rope-node-hash (rope-node-left a) (rope-node-right a))))

    (define (*-rope-content=? a b)
      (define (advance chunk pos stack)
        (let loop ([chunk chunk] [pos pos] [stack stack])
          (cond
            [(and chunk (< pos (chunk-length chunk)))
             (values chunk pos stack)]
            [(null? stack)
             (values #f 0 null)]
            [(rope-leaf? (car stack))
             (loop (rope-leaf-chunk (car stack)) 0 (cdr stack))]
            [else
             (loop chunk pos (list* (rope-node-left (car stack))
                                    (rope-node-right (car stack))
                                    (cdr stack)))])))

      (or (eq? a b)
          (and (= (rope-length a) (rope-length b))
               (equal? (rope-hash1 a) (rope-hash1 b))
               (equal? (rope-hash2 a) (rope-hash2 b))
               (let walk ([ca-chunk #f] [ca-pos 0] [ca-stack (list a)]
                                        [cb-chunk #f] [cb-pos 0] [cb-stack (list b)])
                 (define-values (ca* pa* sa*) (advance ca-chunk ca-pos ca-stack))
                 (define-values (cb* pb* sb*) (advance cb-chunk cb-pos cb-stack))
                 (cond
                   [(and (not ca*) (not cb*)) #t]
                   [(or (not ca*) (not cb*)) #f]
                   [else
                    (define k (min (- (chunk-length ca*) pa*)
                                   (- (chunk-length cb*) pb*)))
                    (and (*-rope-chunk-overlap=? ca* cb* pa* pb* k)
                         (walk ca* (+ pa* k) sa*
                               cb* (+ pb* k) sb*))])))))

    ;; -------------------------------------------------------------------------
    ;; basic operations
    ;; -------------------------------------------------------------------------

    (define (*-rope-concat       a b)     (rope-concat       type-id a b))
    (define (*-rope-append2      a b)     (rope-append2      type-id a b))
    (define (*-rope-append       as)      (rope-append       type-id as))
    (define (*-rope-split        a i)     (rope-split        type-id a i))
    (define (*-rope-ref          a i)     (rope-ref          type-id a i))
    (define (*-rope-offset-index a p)     (rope-offset-index type-id a p))
    (define (*-rope-cut          a i k)   (rope-cut          type-id a i k))
    (define (*-rope-slice        a i k)   (rope-slice        type-id a i k))
    (define (*-rope-splice       a i k b) (rope-splice       type-id a i k b))

    ;; -------------------------------------------------------------------------
    ;; cursors
    ;; -------------------------------------------------------------------------

    ;; immutable cursors
    (define (cursor->*-rope cur) (cursor->rope type-id cur))
    (define (*-cursor-peek  cur) (cursor-peek  type-id cur))
    (define (*-cursor-split cur) (cursor-split type-id cur))

    ;; mutable cursors
    (define (mutable-cursor->*-rope cur) (mutable-cursor->rope type-id cur))
    (define (*-mutable-cursor-peek  cur) (mutable-cursor-peek  type-id cur))

    ;; -------------------------------------------------------------------------
    ;; Folds
    ;; -------------------------------------------------------------------------

    (define (*-rope-foldl proc init a) (rope-foldl type-id proc init a))
    (define (*-rope-foldr proc init a) (rope-foldr type-id proc init a))

    ;; -------------------------------------------------------------------------
    ;; sequences
    ;; -------------------------------------------------------------------------

    (define (in-*-rope a [i 0] [j #f] [k 1])
      (in-rope type-id a i j k))

    (define (in-*-cursor cur [i 0] [j #f] [k 1])
      (in-cursor type-id cur i j k))

    ))
