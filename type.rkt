#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     rope2/stxclasses
                     syntax/parse
                     "./private/type.rkt"
                     "./private/star.rkt")
         rope2/generic
         rope2/rope
         syntax/parse/define
         "./private/hash.rkt")

(provide (for-syntax (all-defined-out))
         (all-defined-out))

(begin-for-syntax
  (define current-τ (make-parameter #f))

  (define-syntax-class type-op
    #:attributes (sym id)
    (pattern x:id
             #:with sym (syntax/loc this-syntax 'x)
             #:with id (star-expand (current-τ) (syntax-e (current-τ)) #'x))))

(define-syntax-parse-rule (define-rope-type τ:id
                            (~alt
                             (~once (~seq #:chunk?       chunk?:id+fun1))
                             (~once (~seq #:chunk-limit  chunk-limit:nat+id+fun0))
                             (~once (~seq #:chunk-empty  chunk-empty:lit+id+fun0))
                             (~once (~seq #:chunk-length chunk-length:id+fun1))
                             (~once (~seq #:chunk-ref    chunk-ref:id+fun2))
                             (~once (~seq #:chunk-slice  chunk-slice:id+fun3))
                             (~once (~seq #:chunk-append chunk-append:id+fun1))
                             (~once (~seq #:elem-width   elem-width:nat+id+fun2))
                             ;; content-based equality primitives
                             (~optional (~seq #:chunk=?         chunk=?-arg:id+fun2))
                             (~optional (~seq #:chunk-overlap=? chunk-overlap=?-arg:id+fun5))
                             (~optional (~seq #:elem=?          elem=?-arg:id+fun2))
                             (~optional (~seq #:elem-hash       elem-hash-arg:id+fun1)))
                            ...)

  #:do [(define τ-stx (attribute τ))
        (define ops (box null))
        (define (add-op str)
          (define id (datum->syntax τ-stx (string->symbol str) τ-stx τ-stx))
          (set-box! ops (cons id (unbox ops)))
          id)
        (current-τ τ-stx)]

  #:with (~var rope:*) (format-id τ-stx "rope:~a" (syntax-e #'τ))

  ;; primitives
  #:with *-chunk?:type-op       (add-op "*-chunk?")
  #:with *-chunk-limit:type-op  (add-op "*-chunk-limit")
  #:with *-chunk-empty:type-op  (add-op "*-chunk-empty")
  #:with *-chunk-length:type-op (add-op "*-chunk-length")
  #:with *-chunk-width:type-op  (add-op "*-chunk-width")
  #:with *-chunk-ref:type-op    (add-op "*-chunk-ref")
  #:with *-chunk-slice:type-op  (add-op "*-chunk-slice")
  #:with *-chunk-append:type-op (add-op "*-chunk-append")
  #:with *-elem-width:type-op   (add-op "*-elem-width")

  ;; tree construction
  #:with *-rope-leaf:type-op  (add-op "*-rope-leaf")
  #:with *-rope-node:type-op  (add-op "*-rope-node")
  #:with *-rope-leaf?:type-op (add-op "*-rope-leaf?")
  #:with *-rope-node?:type-op (add-op "*-rope-node?")
  #:with *-rope?:type-op      (add-op "*-rope?")

  ;; smart constructors
  #:with make-*-rope-leaf:type-op  (add-op "make-*-rope-leaf")
  #:with make-*-rope-node:type-op  (add-op "make-*-rope-node")
  #:with make-empty-*-rope:type-op (add-op "make-empty-*-rope")

  ;; hashing
  #:with *-elem-hash:type-op  (add-op "*-elem-hash")
  #:with *-chunk-hash:type-op (add-op "*-chunk-hash")
  #:with *-node-hash:type-op  (add-op "*-node-hash")
  #:with *-rope-hash:type-op  (add-op "*-rope-hash")

  ;; content-based equality
  #:with *-chunk=?:type-op         (add-op "*-chunk=?")
  #:with *-elem=?:type-op          (add-op "*-elem=?")
  #:with *-chunk-overlap=?:type-op (add-op "*-chunk-overlap=?")
  #:with *-rope=?:type-op          (add-op "*-rope=?")

  ;; conversions
  #:with source->*-rope:type-op (add-op "source->*-rope")
  #:with *-rope->source:type-op (add-op "*-rope->source")

  ;; core operations
  #:with *-rope-concat:type-op       (add-op "*-rope-concat")
  #:with *-rope-append2:type-op      (add-op "*-rope-append2")
  #:with *-rope-append:type-op       (add-op "*-rope-append")
  #:with *-rope-split:type-op        (add-op "*-rope-split")
  #:with *-rope-ref:type-op          (add-op "*-rope-ref")
  #:with *-rope-offset-index:type-op (add-op "*-rope-offset-index")
  #:with *-rope-cut:type-op          (add-op "*-rope-cut")
  #:with *-rope-slice:type-op        (add-op "*-rope-slice")
  #:with *-rope-splice:type-op       (add-op "*-rope-splice")

  ;; immutable cursors
  #:with cursor->*-rope:type-op (add-op "cursor->*-rope")
  #:with *-cursor-peek:type-op  (add-op "*-cursor-peek")
  #:with *-cursor-split:type-op (add-op "*-cursor-split")

  ;; mutable cursors
  #:with mutable-*-cursor->rope:type-op (add-op "mutable-*-cursor->rope")
  #:with mutable-*-cursor-peek:type-op  (add-op "mutable-*-cursor-peek")

  ;; folds
  #:with *-rope-foldl:type-op (add-op "*-rope-foldl")
  #:with *-rope-foldr:type-op (add-op "*-rope-foldr")

  ;; sequences
  #:with in-*-rope:type-op   (add-op "in-*-rope")
  #:with in-*-cursor:type-op (add-op "in-*-cursor")

  ;; descriptor bindings
  #:with (op:type-op ...) (unbox ops)

  (begin
    (define-syntax rope:*
      (rope-type (list (cons op.sym #'op.id) ...)))

    ;; -------------------------------------------------------------------------
    ;; Primitives
    ;; -------------------------------------------------------------------------

    (define (*-chunk?.id x) (chunk? x))
    (define (*-chunk-limit.id) (chunk-limit.callable))
    (define (*-chunk-empty.id) (chunk-empty.callable))
    (define (*-chunk-length.id c) (chunk-length c))
    (define (*-chunk-ref.id c i) (chunk-ref c i))
    (define (*-chunk-slice.id c i k) (chunk-slice c i k))
    (define (*-chunk-append.id . cs) (chunk-append cs))

    (define *-chunk-width.id
      (if (number? elem-width)
          (λ (c) (* (*-chunk-length.id c) elem-width))
          (λ (c) (for/sum ([i (in-range (*-chunk-length.id c))])
                   (*-elem-width.id c i)))))

    (define (*-elem-width.id c i) (elem-width.callable c i))

    ;; -------------------------------------------------------------------------
    ;; Tree Construction
    ;; -------------------------------------------------------------------------

    (struct *-rope-leaf.id rope-leaf () #:transparent)
    (struct *-rope-node.id rope-node () #:transparent)

    (define (*-rope?.id x) (or (*-rope-leaf?.id x) (*-rope-node?.id x)))

    ;; -------------------------------------------------------------------------
    ;; Smart Constructors
    ;; -------------------------------------------------------------------------

    (define (make-*-rope-leaf.id c) (make-rope-leaf τ c))
    (define (make-*-rope-node.id l r) (make-rope-node τ l r))
    (define (make-empty-*-rope.id) (make-empty-rope τ))

    ;; -------------------------------------------------------------------------
    ;; Hashing
    ;; -------------------------------------------------------------------------

    (define (*-elem-hash.id x) ((~? elem-hash-arg equal-hash-code) x))

    (define (*-chunk-hash.id c)
      ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
      (define n   (*-chunk-length.id c))
      (define n/4 (quotient n 4))
      (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
        (cond
          [(< j n/4)
           (define base (* j 4))
           (define e0 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c base)) M))
           (define e1 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 1))) M))
           (define e2 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 2))) M))
           (define e3 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 3))) M))
           (loop (+ j 1)
                 (modulo-M (+ h0 (* e0 q))) (modulo-M (+ h1 (* e1 q)))
                 (modulo-M (+ h2 (* e2 q))) (modulo-M (+ h3 (* e3 q)))
                 (modulo-M (* q X⁴)))]
          [else
           (define h (modulo-M (+ h0 (* X (modulo-M (+ h1 (* X (modulo-M (+ h2 (* X h3))))))))))
           (let tail ([i (* n/4 4)] [h h] [p q])
             (cond
               [(= i n) (values h p)]
               [else
                (define e (bitwise-and (*-elem-hash.id (*-chunk-ref.id c i)) M))
                (tail (+ i 1) (modulo-M (+ h (* e p))) (modulo-M (* p X)))]))])))

    (define (*-node-hash.id l r)
      (define hl (rope-hash-h l))
      (define pl (rope-hash-p l))
      (define hr (rope-hash-h r))
      (define pr (rope-hash-p r))
      (values (modulo-M (+ hl (* pl hr))) (modulo-M (* pl pr))))

    (define (*-rope-hash.id a) (rope-hash τ a))

    ;; -------------------------------------------------------------------------
    ;; Content-Based Equality
    ;; -------------------------------------------------------------------------

    (define (*-chunk=?.id c d) ((~? chunk=?-arg equal?) c d))
    (define (*-elem=?.id x y) ((~? elem=?-arg equal?) x y))

    (define (*-chunk-overlap=?.id c d ic id k)
      (~? (chunk-overlap=?-arg c d ic id k)
          (for/and ([i (in-range k)])
            (*-elem=?.id (*-chunk-ref.id c (+ ic i))
                         (*-chunk-ref.id d (+ id i))))))

    (define (*-rope=?.id a b)
      (define overlap=?
        (~? *-chunk-overlap=?.id
            (λ (c d ic id k)
              (for/and ([i (in-range k)])
                (*-elem=?.id (*-chunk-ref.id c (+ ic i))
                             (*-chunk-ref.id d (+ id i)))))))

      (define (chunk-done? c i) (or (not c) (>= i (*-chunk-length.id c))))

      (define (skip-shared ca ia stack-a cb ib stack-b)
        (if (and (chunk-done? ca ia)
                 (chunk-done? cb ib)
                 (pair? stack-a)
                 (pair? stack-b)
                 (eq? (car stack-a) (car stack-b)))
            (skip-shared #f 0 (cdr stack-a) #f 0 (cdr stack-b))
            (values ca ia stack-a cb ib stack-b)))

      (define (advance c i stack)
        (let loop ([c c] [i i] [stack stack])
          (cond [(and c (< i (*-chunk-length.id c))) (values c i stack)]
                [(null? stack) (values #f 0 null)]
                [(rope-leaf? (car stack))
                 (loop (rope-leaf-chunk (car stack)) 0 (cdr stack))]
                [else (loop c i (list* (rope-node-left (car stack))
                                       (rope-node-right (car stack))
                                       (cdr stack)))])))

      (or (eq? a b)
          (and (= (rope-length a) (rope-length b))
               (equal? (rope-hash-h a) (rope-hash-h b))
               (equal? (rope-hash-p a) (rope-hash-p b))
               (let walk ([ca #f] [ia 0] [stack-a (list a)] [cb #f] [ib 0] [stack-b (list b)])
                 (define-values (ca* ia* stack-a* cb* ib* stack-b*)
                   (skip-shared ca ia stack-a cb ib stack-b))
                 (define-values (ca** ia** stack-a**) (advance ca* ia* stack-a*))
                 (define-values (cb** ib** stack-b**) (advance cb* ib* stack-b*))
                 (cond [(and (not ca**) (not cb**)) #t]
                       [(or  (not ca**) (not cb**)) #f]
                       [else
                        (define len-a (*-chunk-length.id ca**))
                        (define len-b (*-chunk-length.id cb**))
                        (if (and (= len-a len-b) (= ia** ib**))
                            (*-chunk=?.id ca** cb**)
                            (let ([k (min (- len-a ia**) (- len-b ib**))])
                              (and (overlap=? ca** cb** ia** ib** k)
                                   (walk ca** (+ ia** k) stack-a**
                                         cb** (+ ib** k) stack-b**))))])))))

    ;; -------------------------------------------------------------------------
    ;; Conversions
    ;; -------------------------------------------------------------------------

    (define (source->*-rope.id c) (source->rope τ c))
    (define (*-rope->source.id a) (rope->source τ a))

    ;; -------------------------------------------------------------------------
    ;; Basic Operations
    ;; -------------------------------------------------------------------------

    (define (*-rope-concat.id a b) (rope-concat τ a b))
    (define (*-rope-append2.id a b) (rope-append2 τ a b))
    (define (*-rope-append.id as) (rope-append τ as))
    (define (*-rope-split.id a i) (rope-split τ a i))
    (define (*-rope-ref.id a i) (rope-ref τ a i))
    (define (*-rope-offset-index.id a p) (rope-offset-index τ a p))
    (define (*-rope-cut.id a i k) (rope-cut τ a i k))
    (define (*-rope-slice.id a i k) (rope-slice τ a i k))
    (define (*-rope-splice.id a i k b) (rope-splice τ a i k b))

    ;; -------------------------------------------------------------------------
    ;; Immutable Cursors
    ;; -------------------------------------------------------------------------

    (define (cursor->*-rope.id cur) (cursor->rope τ cur))
    (define (*-cursor-peek.id cur) (cursor-peek τ cur))
    (define (*-cursor-split.id cur) (cursor-split τ cur))

    ;; -------------------------------------------------------------------------
    ;; Mutable Cursors
    ;; -------------------------------------------------------------------------

    (define (mutable-*-cursor->rope.id cur) (mutable-cursor->rope τ cur))
    (define (mutable-*-cursor-peek.id cur) (mutable-cursor-peek τ cur))

    ;; -------------------------------------------------------------------------
    ;; Folds
    ;; -------------------------------------------------------------------------

    (define (*-rope-foldl.id proc init a) (rope-foldl τ proc init a))
    (define (*-rope-foldr.id proc init a) (rope-foldr τ proc init a))

    ;; -------------------------------------------------------------------------
    ;; Sequences
    ;; -------------------------------------------------------------------------

    (define-rope-sequence in-*-rope.id τ)
    (define-cursor-sequence in-*-cursor.id τ)))
