#lang racket/base

;; rope/type.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/private/type/stxclasses
                     rope2/private/type/descriptor
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
                            (~optional (~seq #:chunk=?               chunk=?:id+fun2))
                            (~optional (~seq #:chunk-compare         chunk-compare:id+fun2))
                            (~optional (~seq #:chunk-overlap=?       chunk-overlap=?:id+fun5))
                            (~optional (~seq #:chunk-compare-overlap chunk-compare-overlap:id+fun5))
                            #:elem-width   elem-width:nat+id+fun2
                            (~optional (~seq #:elem-hash elem-hash:id+fun1))
                            (~optional (~seq #:elem<? elem<?:id+fun2))
                            (~optional (~seq #:elem>? elem>?:id+fun2)))
  #:do [(define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))]

  ;; rope type descriptor
  #:with (~var rope:*)                (mk* "rope:~a")

  ;; per-chunk primitives
  #:with *-rope-chunk?                (mk* "~a-rope-chunk?")
  #:with *-rope-chunk-limit           (mk* "~a-rope-chunk-limit")
  #:with *-rope-chunk-empty           (mk* "~a-rope-chunk-empty")
  #:with *-rope-chunk-length          (mk* "~a-rope-chunk-length")
  #:with *-rope-chunk-width           (mk* "~a-rope-chunk-width")
  #:with *-rope-chunk-ref             (mk* "~a-rope-chunk-ref")
  #:with *-rope-chunk-slice           (mk* "~a-rope-chunk-slice")
  #:with *-rope-chunk-append          (mk* "~a-rope-chunk-append")
  #:with *-rope-chunk=?               (mk* "~a-rope-chunk=?")
  #:with *-rope-chunk-compare         (mk* "~a-rope-chunk-compare")
  #:with *-rope-chunk-compare-overlap (mk* "~a-rope-chunk-compare-overlap")
  #:with *-rope-chunk-overlap=?       (mk* "~a-rope-chunk-overlap=?")

  ;; per-element primitives
  #:with *-rope-elem-width            (mk* "~a-rope-elem-width")
  #:with *-rope-elem-hash             (mk* "~a-rope-elem-hash")
  #:with *-rope-elem<?                (mk* "~a-rope-elem<?")
  #:with *-rope-elem>?                (mk* "~a-rope-elem>?")

  ;; per-rope primitives
  #:with *-rope-chunk-hash            (mk* "~a-rope-chunk-hash")
  #:with *-rope-node-hash             (mk* "~a-rope-node-hash")

  ;; rope structs
  #:with *-rope-equal+hash-impl       (mk* "~a-rope-equal+hash-impl")
  #:with *-rope-leaf                  (mk* "~a-rope-leaf")
  #:with *-rope-node                  (mk* "~a-rope-node")

  ;; rope predicates
  #:with *-rope-leaf?                 (mk* "~a-rope-leaf?")
  #:with *-rope-node?                 (mk* "~a-rope-node?")
  #:with *-rope?                      (mk* "~a-rope?")

  ;; content-based hashing & equality
  #:with make-*-rope-hash             (mk* "make-~a-rope-hash")
  #:with *-rope-content=?             (mk* "~a-rope-content=?")

  ;; smart constructors
  #:with make-*-rope-leaf             (mk* "make-~a-rope-leaf")
  #:with make-*-rope-node             (mk* "make-~a-rope-node")
  #:with make-empty-*-rope            (mk* "make-empty-~a-rope")

  ;; conversions
  #:with *-chunk->rope                (mk* "~a-chunk->rope")
  #:with *-rope->chunk                (mk* "~a-rope->chunk")

  ;; basic operations
  #:with *-rope-concat                (mk* "~a-rope-concat")
  #:with *-rope-append2               (mk* "~a-rope-append2")
  #:with *-rope-append                (mk* "~a-rope-append")
  #:with *-rope-split                 (mk* "~a-rope-split")
  #:with *-rope-ref                   (mk* "~a-rope-ref")
  #:with *-rope-offset-index          (mk* "~a-rope-offset-index")
  #:with *-rope-cut                   (mk* "~a-rope-cut")
  #:with *-rope-slice                 (mk* "~a-rope-slice")
  #:with *-rope-splice                (mk* "~a-rope-splice")

  ;; immutable cursors
  #:with cursor->*-rope               (mk* "cursor->~a-rope")
  #:with *-cursor-peek                (mk* "~a-cursor-peek")
  #:with *-cursor-split               (mk* "~a-cursor-split")

  ;; mutable cursors
  #:with mutable-cursor->*-rope       (mk* "mutable-cursor->~a-rope")
  #:with *-mutable-cursor-peek        (mk* "~a-mutable-cursor-peek")

  ;; folds
  #:with *-rope-foldl                 (mk* "~a-rope-foldl")
  #:with *-rope-foldr                 (mk* "~a-rope-foldr")

  ;; sequences
  #:with in-*-rope                    (mk* "in-~a-rope")
  #:with in-*-cursor                  (mk* "in-~a-cursor")

  ;; comparison relations
  #:with *-rope-compare-with          (mk* "~a-rope-compare-with")
  #:with *-rope-compare               (mk* "~a-rope-compare")

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
                            ;; #'*-rope-chunk=?
                            #'*-rope-chunk-compare
                            #'*-rope-chunk-compare-overlap
                            ;; #'*-rope-chunk-overlap=?
                            #'*-rope-elem-width
                            ;; #'*-rope-elem-hash
                            #'*-rope-elem<?
                            #'*-rope-elem>?
                            #'*-rope-leaf
                            #'*-rope-node
                            ;; #'*-rope-chunk-hash
                            ;; #'*-rope-node-hash
                            ;; #'make-*-rope-hash
                            ;; #'*-rope-content=?
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
    ;; (define (*-rope-chunk=?      c d)   ((~? chunk=? equal?) c d))

    ;; (define *-rope-chunk-width
    ;;   (if (number? elem-width)
    ;;       (λ (c) (* (chunk-length c) elem-width))
    ;;       (λ (c) (for/sum ([i (in-range (chunk-length c))]) (elem-width c i)))))

    ;; (define *-rope-chunk-compare
    ;;   (~? (λ (c d) (chunk-compare c d))
    ;;       (λ _ (error '*-rope-chunk-compare "operation not defined"))))

    ;; (define *-rope-chunk-compare-overlap
    ;;   (λ (ca cb ia ib k)
    ;;     (~? (chunk-compare-overlap ca cb ia ib k)
    ;;         (~@ (define (elem-loop-compare)
    ;;               (~? (let loop ([i 0])
    ;;                     (cond [(= i k) '=]
    ;;                           [(elem<? (chunk-ref ca (+ ia i)) (chunk-ref cb (+ ib i))) '<]
    ;;                           [(elem>? (chunk-ref ca (+ ia i)) (chunk-ref cb (+ ib i))) '>]
    ;;                           [else (loop (add1 i))]))
    ;;                   (error '*-rope-chunk-compare-overlap "operation not defined")))
    ;;             (if (and (= ia 0) (= ib 0) (= k (chunk-length ca)) (= k (chunk-length cb)))
    ;;                 (~? (chunk-compare ca cb) (elem-loop-compare))
    ;;                 (elem-loop-compare))))))

    ;; (define *-rope-chunk-overlap=?
    ;;   (λ (ca cb ia ib k)
    ;;     (~? (chunk-overlap=? ca cb ia ib k)
    ;;         (for/and ([i (in-range k)])
    ;;           (equal? (chunk-ref ca (+ ia i)) (chunk-ref cb (+ ib i)))))))

    ;; -------------------------------------------------------------------------
    ;; per-element primitives
    ;; -------------------------------------------------------------------------

    ;; (define (*-rope-elem-width c i) (elem-width c i))
    ;; ;; (define (*-rope-elem-hash  c)   (~? (elem-hash c) (equal-hash-code c)))
    ;; (define (*-rope-elem<?     c d) (~? (elem<? c d) (error '*-rope-elem<? "operation not defined")))
    ;; (define (*-rope-elem>?     c d) (~? (elem>? c d) (error '*-rope-elem>? "operation not defined")))

    ;; -------------------------------------------------------------------------
    ;; rope structs & predicates
    ;; -------------------------------------------------------------------------

    ;; (define *-rope-equal+hash-impl
    ;;   (list (λ (a b _) (*-rope-content=? a b))
    ;;         (λ (a _) (rope-hash1 a))
    ;;         (λ (a _) (rope-hash2 a))))

    (struct *-rope-leaf rope-leaf () #:transparent)
    (struct *-rope-node rope-node () #:transparent)

    (define (*-rope? x) (or (*-rope-leaf? x) (*-rope-node? x)))

    ;; -------------------------------------------------------------------------
    ;; smart constructors
    ;; -------------------------------------------------------------------------

    ;; (define (make-*-rope-leaf c)   (make-rope-leaf  type-id c))
    ;; (define (make-*-rope-node l r) (make-rope-node  type-id l r))
    ;; (define (make-empty-*-rope)    (make-empty-rope type-id))

    ;; -------------------------------------------------------------------------
    ;; conversions
    ;; -------------------------------------------------------------------------

    ;; (define (*-chunk->rope c) (chunk->rope type-id c))
    ;; (define (*-rope->chunk a) (rope->chunk type-id a))

    ;; -------------------------------------------------------------------------
    ;; content-based hashing & equality
    ;; -------------------------------------------------------------------------

    ;; (define (*-rope-chunk-hash c)
    ;;   ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
    ;;   (define n   (chunk-length c))
    ;;   (define n/4 (quotient n 4))
    ;;   (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
    ;;     (cond
    ;;       [(< j n/4)
    ;;        (define base (* j 4))
    ;;        (define e0 (bitwise-and (*-rope-elem-hash (chunk-ref c base))          M))
    ;;        (define e1 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 1)))  M))
    ;;        (define e2 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 2)))  M))
    ;;        (define e3 (bitwise-and (*-rope-elem-hash (chunk-ref c (+ base 3)))  M))
    ;;        (loop (+ j 1)
    ;;              (fxmodulo-M (+ h0 (* e0 q)))
    ;;              (fxmodulo-M (+ h1 (* e1 q)))
    ;;              (fxmodulo-M (+ h2 (* e2 q)))
    ;;              (fxmodulo-M (+ h3 (* e3 q)))
    ;;              (fxmodulo-M (* q X⁴)))]
    ;;       [else
    ;;        (define h (fxmodulo-M (+ h0 (* X (fxmodulo-M (+ h1 (* X (fxmodulo-M (+ h2 (* X h3))))))))))
    ;;        (let tail ([i (* n/4 4)] [h h] [p q])
    ;;          (if (= i n)
    ;;              (values h p)
    ;;              (let ([e (bitwise-and (*-rope-elem-hash (chunk-ref c i)) M)])
    ;;                (tail (+ i 1)
    ;;                      (fxmodulo-M (+ h (* e p)))
    ;;                      (fxmodulo-M (* p X))))))])))

    ;; (define (*-rope-node-hash l r)
    ;;   (define hl (rope-hash1 l))
    ;;   (define pl (rope-hash2 l))
    ;;   (define hr (rope-hash1 r))
    ;;   (define pr (rope-hash2 r))
    ;;   (values (fxmodulo-M (+ hl (* pl hr)))
    ;;           (fxmodulo-M (* pl pr))))

    ;; (define (make-*-rope-hash a)
    ;;   (if (rope-leaf? a)
    ;;       (*-rope-chunk-hash (rope-leaf-chunk a))
    ;;       (*-rope-node-hash (rope-node-left a) (rope-node-right a))))

    ;; (define (*-rope-content=? a b)
    ;;   (define (chunk-done? c i)
    ;;     (or (not c) (>= i (chunk-length c))))
    ;;   (define (skip-shared ca ia stack-a cb ib stack-b)
    ;;     (if (and (chunk-done? ca ia)
    ;;              (chunk-done? cb ib)
    ;;              (pair? stack-a)
    ;;              (pair? stack-b)
    ;;              (eq? (car stack-a) (car stack-b)))
    ;;         (skip-shared #f 0 (cdr stack-a) #f 0 (cdr stack-b))
    ;;         (values ca ia stack-a cb ib stack-b)))
    ;;   (define (advance c i stack)
    ;;     (let loop ([c c] [i i] [stack stack])
    ;;       (cond
    ;;         [(and c (< i (chunk-length c)))
    ;;          (values c i stack)]
    ;;         [(null? stack)
    ;;          (values #f 0 null)]
    ;;         [(rope-leaf? (car stack))
    ;;          (loop (rope-leaf-chunk (car stack)) 0 (cdr stack))]
    ;;         [else
    ;;          (loop c i (list* (rope-node-left (car stack))
    ;;                           (rope-node-right (car stack))
    ;;                           (cdr stack)))])))
    ;;   (or (eq? a b)
    ;;       (and (= (rope-length a) (rope-length b))
    ;;            (equal? (rope-hash1 a) (rope-hash1 b))
    ;;            (equal? (rope-hash2 a) (rope-hash2 b))
    ;;            (let walk ([ca #f] [ia 0] [stack-a (list a)] [cb #f] [ib 0] [stack-b (list b)])
    ;;              (define-values (ca* ia* stack-a* cb* ib* stack-b*)
    ;;                (skip-shared ca ia stack-a cb ib stack-b))
    ;;              (define-values (ca** ia** stack-a**) (advance ca* ia* stack-a*))
    ;;              (define-values (cb** ib** stack-b**) (advance cb* ib* stack-b*))
    ;;              (cond
    ;;                [(and (not ca**) (not cb**)) #t]
    ;;                [(or  (not ca**) (not cb**)) #f]
    ;;                [else
    ;;                 (define len-a (chunk-length ca**))
    ;;                 (define len-b (chunk-length cb**))
    ;;                 (if (and (= len-a len-b) (= ia** ib**))
    ;;                     (*-rope-chunk=? ca** cb**)
    ;;                     (let ([k (min (- len-a ia**) (- len-b ib**))])
    ;;                       (and (*-rope-chunk-overlap=? ca** cb** ia** ib** k)
    ;;                            (walk ca** (+ ia** k) stack-a**
    ;;                                  cb** (+ ib** k) stack-b**))))])))))

    ;; -------------------------------------------------------------------------
    ;; basic operations
    ;; -------------------------------------------------------------------------

    ;; (define (*-rope-concat       a b)     (rope-concat       type-id a b))
    ;; (define (*-rope-append2      a b)     (rope-append2      type-id a b))
    ;; (define (*-rope-append       as)      (rope-append       type-id as))
    ;; (define (*-rope-split        a i)     (rope-split        type-id a i))
    ;; (define (*-rope-ref          a i)     (rope-ref          type-id a i))
    ;; (define (*-rope-offset-index a p)     (rope-offset-index type-id a p))
    ;; (define (*-rope-cut          a i k)   (rope-cut          type-id a i k))
    ;; (define (*-rope-slice        a i k)   (rope-slice        type-id a i k))
    ;; (define (*-rope-splice       a i k b) (rope-splice       type-id a i k b))

    ;; -------------------------------------------------------------------------
    ;; cursors
    ;; -------------------------------------------------------------------------

    ;; immutable cursors
    ;; (define (cursor->*-rope cur) (cursor->rope type-id cur))
    ;; (define (*-cursor-peek  cur) (cursor-peek  type-id cur))
    ;; (define (*-cursor-split cur) (cursor-split type-id cur))

    ;; mutable cursors
    ;; (define (mutable-cursor->*-rope cur) (mutable-cursor->rope type-id cur))
    ;; (define (*-mutable-cursor-peek  cur) (mutable-cursor-peek  type-id cur))

    ;; -------------------------------------------------------------------------
    ;; folds
    ;; -------------------------------------------------------------------------

    ;; (define (*-rope-foldl proc init a) (rope-foldl type-id proc init a))
    ;; (define (*-rope-foldr proc init a) (rope-foldr type-id proc init a))

    ;; -------------------------------------------------------------------------
    ;; sequences
    ;; -------------------------------------------------------------------------

    ;; (define-rope-sequence   in-*-rope   type-id)
    ;; (define-cursor-sequence in-*-cursor type-id)

    ;; -------------------------------------------------------------------------
    ;; comparison relations
    ;; -------------------------------------------------------------------------

    ;; (define (*-rope-compare-with proc a b) (rope-compare-with type-id proc a b))
    ;; (define (*-rope-compare      a b)      (rope-compare      type-id a b))

    ;; TODO eliminate per-sep cursor-advance! in sequence iterators (and folds?)
    ;; TODO cursor-based variants (how to handle unequal lengths? probably same as in-cursor?)
    ;; TODO rope-prefix / rope-suffix
    ;; TODO longest common prefix / length
    ;; TODO partial orders
    ;; TODO gen:order (data/order) ???

    ;; TODO later: substring search (rope-contains?)

    ))
