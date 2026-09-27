#lang racket/base

;; (require (for-syntax racket/base
;;                      racket/syntax
;;                      rope2/descriptors
;;                      rope2/stxclasses
;;                      syntax/parse)
;;          ;; racket/sequence
;;          ;; rope2/cursor
;;          rope2/generic
;;          rope2/rope
;;          syntax/parse/define
;;          "./private/hash.rkt")

;; (provide (all-defined-out))

;; (define-syntax-parse-rule (define-rope-type type-id:id
;;                             (~alt
;;                              (~once (~seq #:chunk?       chunk?:id+fun1))
;;                              (~once (~seq #:chunk-limit  chunk-limit:nat+id+fun0))
;;                              (~once (~seq #:chunk-empty  chunk-empty:lit+id+fun0))
;;                              (~once (~seq #:chunk-length chunk-length:id+fun1))
;;                              (~once (~seq #:chunk-ref    chunk-ref:id+fun2))
;;                              (~once (~seq #:chunk-slice  chunk-slice:id+fun3))
;;                              (~once (~seq #:chunk-append chunk-append:id+fun1))
;;                              (~once (~seq #:elem-width   elem-width:nat+id+fun2))
;;                              ;; equality instance members
;;                              (~optional (~seq #:chunk=?         chunk=?-arg:id+fun2))
;;                              (~optional (~seq #:chunk-overlap=? chunk-overlap=?-arg:id+fun5))
;;                              (~optional (~seq #:elem=?          elem=?-arg:id+fun2))
;;                              (~optional (~seq #:elem-hash       elem-hash-arg:id+fun1)))
;;                             ...)
;;   #:do [(define prim*s (box null))      ; type primitives
;;         (define def-op*s (box null))    ; type ops defined here
;;         (define wrap-op*s (box null))   ; type ops wrapping generic defs
;;         (define equal-op*s (box null))  ; equality class members

;;         (define (add-raw-op* op*s sym id) (define pair (cons sym id)) (cons pair op*s) pair)
;;         (define (add-raw-wrap-op* sym id) (add-raw-op* wrap-op*s sym id))

;;         (define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))

;;         (define star-id-rx #px"\\*(?=-(rope|node|leaf|chunk|elem|cursor)\\b)")
;;         (define (add-op* op*s str)
;;           (define sym (string->symbol str))
;;           (define id (mk* (regexp-replace* star-id-rx str "~a")))
;;           (add-raw-op* op*s sym id))

;;         (define (add-prim* str) (add-op* prim*s str))
;;         (define (add-def-op* str) (add-op* def-op*s str))
;;         (define (add-wrap-op* str) (add-op* wrap-op*s str))
;;         (define (add-equal-op* str) (add-op* equal-op*s str))]

;;   ;; descriptor
;;   #:with (~var rope:*)          (mk* "rope:~a")
;;   #:with (~var rope:*:equality) (mk* "rope:~a:equality")

;;   ;; primitives
;;   #:with *-chunk?:op*               (add-prim* "*-chunk?")
;;   #:with *-chunk-limit:op*          (add-prim* "*-chunk-limit")
;;   #:with *-chunk-empty:op*          (add-prim* "*-chunk-empty")
;;   #:with *-chunk-length:op*         (add-prim* "*-chunk-length")
;;   #:with *-chunk-width:op*          (add-prim* "*-chunk-width")
;;   #:with *-chunk-ref:op*            (add-prim* "*-chunk-ref")
;;   #:with *-chunk-slice:op*          (add-prim* "*-chunk-slice")
;;   #:with *-chunk-append:op*         (add-prim* "*-chunk-append")
;;   #:with *-elem-width:op*           (add-prim* "*-elem-width")

;;   ;; tree construction
;;   #:with *-rope-leaf:op*            (add-def-op* "*-rope-leaf")
;;   #:with *-rope-node:op*            (add-def-op* "*-rope-node")
;;   #:with *-rope-leaf?:op*           (add-def-op* "*-rope-leaf?")
;;   #:with *-rope-node?:op*           (add-def-op* "*-rope-node?")
;;   #:with *-rope?:op*                (add-def-op* "*-rope?")

;;   ;; smart constructors
;;   #:with make-*-rope-leaf:op*       (add-wrap-op* "make-*-rope-leaf")
;;   #:with make-*-rope-node:op*       (add-wrap-op* "make-*-rope-node")
;;   #:with make-empty-*-rope:op*      (add-wrap-op* "make-empty-*-rope")

;;   ;; equality members: hashing
;;   #:with *-elem-hash:op*            (add-equal-op* "*-elem-hash")
;;   #:with *-chunk-hash:op*           (add-equal-op* "*-chunk-hash")
;;   #:with *-node-hash:op*            (add-equal-op* "*-node-hash")
;;   #:with *-rope-hash:op*            (add-wrap-op* "*-rope-hash")

;;   ;; equality members: content-based equality
;;   #:with *-chunk=?:op*              (add-equal-op* "*-chunk=?")
;;   #:with *-elem=?:op*               (add-equal-op* "*-elem=?")
;;   #:with *-chunk-overlap=?:op*      (add-equal-op* "*-chunk-overlap=?")
;;   #:with *-rope=?:op*               (add-equal-op* "*-rope=?")

;;   ;; conversions
;;   #:with *->rope:op*                (add-raw-wrap-op* '*->rope (mk* "~a->rope"))
;;   #:with rope->*:op*                (add-raw-wrap-op* 'rope->* (mk* "rope->~a"))

;;   ;; basic operations
;;   #:with *-rope-concat:op*          (add-wrap-op* "*-rope-concat")
;;   #:with *-rope-append2:op*         (add-wrap-op* "*-rope-append2")
;;   #:with *-rope-append:op*          (add-wrap-op* "*-rope-append")
;;   #:with *-rope-split:op*           (add-wrap-op* "*-rope-split")
;;   #:with *-rope-ref:op*             (add-wrap-op* "*-rope-ref")
;;   #:with *-rope-offset-index:op*    (add-wrap-op* "*-rope-offset-index")
;;   #:with *-rope-cut:op*             (add-wrap-op* "*-rope-cut")
;;   #:with *-rope-slice:op*           (add-wrap-op* "*-rope-slice")
;;   #:with *-rope-splice:op*          (add-wrap-op* "*-rope-splice")

;;   ;; immutable cursors
;;   #:with cursor->*-rope:op*         (add-wrap-op* "cursor->*-rope")
;;   #:with *-cursor-peek:op*          (add-wrap-op* "*-cursor-peek")
;;   #:with *-cursor-split:op*         (add-wrap-op* "*-cursor-split")

;;   ;; mutable cursors
;;   #:with mutable-*-cursor->rope:op* (add-wrap-op* "mutable-*-cursor->rope")
;;   #:with mutable-*-cursor-peek:op*  (add-wrap-op* "mutable-*-cursor-peek")

;;   ;; folds
;;   #:with *-rope-foldl:op*           (add-wrap-op* "*-rope-foldl")
;;   #:with *-rope-foldr:op*           (add-wrap-op* "*-rope-foldr")

;;   ;; sequences
;;   #:with in-*-rope:op*              (add-wrap-op* "in-*-rope")
;;   #:with in-*-cursor:op*            (add-wrap-op* "in-*-cursor")

;;   ;; descriptor components
;;   #:with ([p-sym . p-stx] ...) (reverse prim*s)
;;   #:with ([d-sym . d-stx] ...) (reverse def-op*s)
;;   #:with ([w-sym . w-stx] ...) (reverse wrap-op*s)
;;   #:with ([e-sym . e-stx] ...) (reverse equal-op*s)

;;   (begin

;;     ;; -------------------------------------------------------------------------
;;     ;; Rope Type Descriptor
;;     ;; -------------------------------------------------------------------------

;;     (define-syntax rope:*
;;       (rope-type-descriptor (list (cons p-sym p-stx) ...)
;;                             (list (cons d-sym d-stx) ...)
;;                             (list (cons w-sym w-stx) ...)))

;;     ;; -------------------------------------------------------------------------
;;     ;; Equality Instance Descriptor
;;     ;; -------------------------------------------------------------------------

;;     (define-syntax rope:*:equality
;;       (rope-instance-descriptor (list (cons e-sym e-stx) ...)))

;;     ;; -------------------------------------------------------------------------
;;     ;; Primitive Operations
;;     ;; -------------------------------------------------------------------------

;;     (define (*-chunk?.id x) (chunk? x))
;;     (define (*-chunk-limit.id) (chunk-limit.callable))
;;     (define (*-chunk-empty.id) (chunk-empty.callable))
;;     (define (*-chunk-length.id c) (chunk-length c))
;;     (define (*-chunk-ref.id c i) (chunk-ref c i))
;;     (define (*-chunk-slice.id c i k) (chunk-slice c i k))
;;     (define (*-chunk-append.id . cs) (chunk-append cs))

;;     (define *-chunk-width.id
;;       (if (number? elem-width)
;;           (λ (c) (* (*-chunk-length.id c) elem-width))
;;           (λ (c) (for/sum ([i (in-range (*-chunk-length.id c))]) (*-elem-width.id c i)))))

;;     (define (*-elem-width.id c i) (elem-width.callable c i))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Tree Construction
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (struct *-rope-leaf.id rope-leaf () #:transparent)
;;     ;; (struct *-rope-node.id rope-node () #:transparent)

;;     ;; (define (*-rope?.id x) (or (*-rope-leaf?.id x) (*-rope-node?.id x)))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Smart Constructors
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (make-*-rope-leaf.id c) (make-rope-leaf type-id c))
;;     ;; (define (make-*-rope-node.id l r) (make-rope-node type-id l r))
;;     ;; (define (make-empty-*-rope.id) (make-empty-rope type-id))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Hashing
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (*-elem-hash.id x) ((~? elem-hash-arg equal-hash-code) x))

;;     ;; (define (*-chunk-hash.id c)
;;     ;;   ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
;;     ;;   (define n   (*-chunk-length.id c))
;;     ;;   (define n/4 (quotient n 4))
;;     ;;   (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
;;     ;;     (cond
;;     ;;       [(< j n/4)
;;     ;;        (define base (* j 4))
;;     ;;        (define e0 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c base))        M))
;;     ;;        (define e1 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 1)))  M))
;;     ;;        (define e2 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 2)))  M))
;;     ;;        (define e3 (bitwise-and (*-elem-hash.id (*-chunk-ref.id c (+ base 3)))  M))
;;     ;;        (loop (+ j 1)
;;     ;;              (modulo-M (+ h0 (* e0 q)))
;;     ;;              (modulo-M (+ h1 (* e1 q)))
;;     ;;              (modulo-M (+ h2 (* e2 q)))
;;     ;;              (modulo-M (+ h3 (* e3 q)))
;;     ;;              (modulo-M (* q X⁴)))]
;;     ;;       [else
;;     ;;        (define h (modulo-M (+ h0 (* X (modulo-M (+ h1 (* X (modulo-M (+ h2 (* X h3))))))))))
;;     ;;        (let tail ([i (* n/4 4)] [h h] [p q])
;;     ;;          (if (= i n)
;;     ;;              (values h p)
;;     ;;              (let ([e (bitwise-and (*-elem-hash.id (*-chunk-ref.id c i)) M)])
;;     ;;                (tail (+ i 1)
;;     ;;                      (modulo-M (+ h (* e p)))
;;     ;;                      (modulo-M (* p X))))))])))

;;     ;; (define (*-node-hash.id l r)
;;     ;;   (define hl (rope-hash-h l))
;;     ;;   (define pl (rope-hash-p l))
;;     ;;   (define hr (rope-hash-h r))
;;     ;;   (define pr (rope-hash-p r))
;;     ;;   (values (modulo-M (+ hl (* pl hr)))
;;     ;;           (modulo-M (* pl pr))))

;;     ;; (define (*-rope-hash.id a) (rope-hash type-id a))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Content-Based Equality
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (*-chunk=?.id c d) ((~? chunk=?-arg equal?) c d))
;;     ;; (define (*-elem=?.id x y) ((~? elem=?-arg equal?) x y))

;;     ;; (define (*-chunk-overlap=?.id c d ic id k)
;;     ;;   (~? (chunk-overlap=?-arg c d ic id k)
;;     ;;       (for/and ([i (in-range k)])
;;     ;;         (*-elem=?.id (*-chunk-ref.id c (+ ic i))
;;     ;;                      (*-chunk-ref.id d (+ id i))))))

;;     ;; (define (*-rope=?.id a b)
;;     ;;   (define overlap=?
;;     ;;     (~? *-chunk-overlap=?.id
;;     ;;         (λ (c d ic id k)
;;     ;;           (for/and ([i (in-range k)])
;;     ;;             (*-elem=?.id (*-chunk-ref.id c (+ ic i))
;;     ;;                          (*-chunk-ref.id d (+ id i)))))))

;;     ;;   (define (chunk-done? c i)
;;     ;;     (or (not c) (>= i (*-chunk-length.id c))))

;;     ;;   (define (skip-shared ca ia stack-a cb ib stack-b)
;;     ;;     (if (and (chunk-done? ca ia)
;;     ;;              (chunk-done? cb ib)
;;     ;;              (pair? stack-a)
;;     ;;              (pair? stack-b)
;;     ;;              (eq? (car stack-a) (car stack-b)))
;;     ;;         (skip-shared #f 0 (cdr stack-a) #f 0 (cdr stack-b))
;;     ;;         (values ca ia stack-a cb ib stack-b)))

;;     ;;   (define (advance c i stack)
;;     ;;     (let loop ([c c] [i i] [stack stack])
;;     ;;       (cond
;;     ;;         [(and c (< i (*-chunk-length.id c)))
;;     ;;          (values c i stack)]
;;     ;;         [(null? stack)
;;     ;;          (values #f 0 null)]
;;     ;;         [(rope-leaf? (car stack))
;;     ;;          (loop (rope-leaf-chunk (car stack)) 0 (cdr stack))]
;;     ;;         [else
;;     ;;          (loop c i (list* (rope-node-left (car stack))
;;     ;;                           (rope-node-right (car stack))
;;     ;;                           (cdr stack)))])))

;;     ;;   (or (eq? a b)
;;     ;;       (and (= (rope-length a) (rope-length b))
;;     ;;            (equal? (rope-hash-h a) (rope-hash-h b))
;;     ;;            (equal? (rope-hash-p a) (rope-hash-p b))
;;     ;;            (let walk ([ca #f] [ia 0] [stack-a (list a)] [cb #f] [ib 0] [stack-b (list b)])
;;     ;;              (define-values (ca* ia* stack-a* cb* ib* stack-b*)
;;     ;;                (skip-shared ca ia stack-a cb ib stack-b))
;;     ;;              (define-values (ca** ia** stack-a**) (advance ca* ia* stack-a*))
;;     ;;              (define-values (cb** ib** stack-b**) (advance cb* ib* stack-b*))
;;     ;;              (cond
;;     ;;                [(and (not ca**) (not cb**)) #t]
;;     ;;                [(or  (not ca**) (not cb**)) #f]
;;     ;;                [else
;;     ;;                 (define len-a (*-chunk-length.id ca**))
;;     ;;                 (define len-b (*-chunk-length.id cb**))
;;     ;;                 (if (and (= len-a len-b) (= ia** ib**))
;;     ;;                     (*-chunk=?.id ca** cb**)
;;     ;;                     (let ([k (min (- len-a ia**) (- len-b ib**))])
;;     ;;                       (and (overlap=? ca** cb** ia** ib** k)
;;     ;;                            (walk ca** (+ ia** k) stack-a**
;;     ;;                                  cb** (+ ib** k) stack-b**))))])))))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Conversions
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (*->rope.id c) (chunk->rope type-id c))
;;     ;; (define (rope->*.id a) (rope->chunk type-id a))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Basic Operations
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (*-rope-concat.id a b) (rope-concat type-id a b))
;;     ;; (define (*-rope-append2.id a b) (rope-append2 type-id a b))
;;     ;; (define (*-rope-append.id as) (rope-append type-id as))
;;     ;; (define (*-rope-split.id a i) (rope-split type-id a i))
;;     ;; (define (*-rope-ref.id a i) (rope-ref type-id a i))
;;     ;; (define (*-rope-offset-index.id a p) (rope-offset-index type-id a p))
;;     ;; (define (*-rope-cut.id a i k) (rope-cut type-id a i k))
;;     ;; (define (*-rope-slice.id a i k) (rope-slice type-id a i k))
;;     ;; (define (*-rope-splice.id a i k b) (rope-splice type-id a i k b))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Immutable Cursors
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (cursor->*-rope.id cur) (cursor->rope type-id cur))
;;     ;; (define (*-cursor-peek.id  cur) (cursor-peek  type-id cur))
;;     ;; (define (*-cursor-split.id cur) (cursor-split type-id cur))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Mutable Cursors
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (mutable-*-cursor->rope.id cur) (mutable-cursor->rope type-id cur))
;;     ;; (define (mutable-*-cursor-peek.id  cur) (mutable-cursor-peek  type-id cur))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Folds
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define (*-rope-foldl.id proc init a) (rope-foldl type-id proc init a))
;;     ;; (define (*-rope-foldr.id proc init a) (rope-foldr type-id proc init a))

;;     ;; ;; -------------------------------------------------------------------------
;;     ;; ;; Sequences
;;     ;; ;; -------------------------------------------------------------------------

;;     ;; (define-rope-sequence   in-*-rope.id   type-id)
;;     ;; (define-cursor-sequence in-*-cursor.id type-id)

;;     ))
