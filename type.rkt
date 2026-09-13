#lang racket/base

;; rope/type.rkt

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "./private/class/descriptor.rkt"
                     "./private/stxclasses.rkt"
                     "./private/type/descriptor.rkt")
         ;; racket/sequence
         ;; rope2/cursor
         rope2/generic
         rope2/rope
         syntax/parse/define
         "./private/hash.rkt")

(provide (all-defined-out))

;; =============================================================================
;; Core rope functionality that does not depend on hashing or equality.
;; =============================================================================

(define-syntax-parse-rule (define-rope-type type-id:id
                            (~alt
                             ;; chunk primitives
                             (~once (~seq #:chunk?       chunk?:id+fun1))
                             (~once (~seq #:chunk-limit  chunk-limit:nat+id+fun0))
                             (~once (~seq #:chunk-empty  chunk-empty:lit+id+fun0))
                             (~once (~seq #:chunk-length chunk-length:id+fun1))
                             (~once (~seq #:chunk-ref    chunk-ref:id+fun2))
                             (~once (~seq #:chunk-slice  chunk-slice:id+fun3))
                             (~once (~seq #:chunk-append chunk-append:id+fun1))
                             ;; element primitives
                             (~once (~seq #:elem-width   elem-width:nat+id+fun2)))
                            ...)
  #:do [(define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))
        (syntax-local-lift-module-end-declaration #`(finish-rope-type #,(attribute type-id)))]

  ;; rope type descriptor
  #:with (~var rope:*)  (mk* "rope:~a")

  ;; chunk primitives
  #:with *-chunk?       (mk* "~a-chunk?")
  #:with *-chunk-limit  (mk* "~a-chunk-limit")
  #:with *-chunk-empty  (mk* "~a-chunk-empty")
  #:with *-chunk-length (mk* "~a-chunk-length")
  #:with *-chunk-ref    (mk* "~a-chunk-ref")
  #:with *-chunk-slice  (mk* "~a-chunk-slice")
  #:with *-chunk-append (mk* "~a-chunk-append")

  ;; derived chunk operations
  #:with *-chunk-width  (mk* "~a-chunk-width")

  ;; element primitives
  #:with *-elem-width   (mk* "~a-elem-width")

  ;; tree construction
  #:with *-rope-leaf    (mk* "~a-rope-leaf")
  #:with *-rope-node    (mk* "~a-rope-node")
  #:with *-rope-leaf?   (mk* "~a-rope-leaf?")
  #:with *-rope-node?   (mk* "~a-rope-node?")
  #:with *-rope?        (mk* "~a-rope?")

  ;; internal hashing / equality
  #:with *-chunk-hash     (mk* "~a-chunk-hash")
  #:with *-node-hash      (mk* "~a-node-hash")
  #:with *-rope-content=? (mk* "~a-rope-content=?")

  (begin

    ;; -------------------------------------------------------------------------
    ;; Rope Type Descriptor
    ;; -------------------------------------------------------------------------

    (define-syntax rope:* (rope-type-descriptor
                           ;; chunk operations
                           #'*-chunk?
                           #'*-chunk-limit
                           #'*-chunk-empty
                           #'*-chunk-length
                           #'*-chunk-width
                           #'*-chunk-ref
                           #'*-chunk-slice
                           #'*-chunk-append
                           ;; element operations
                           #'*-elem-width
                           ;; smart constructors
                           #'*-rope-leaf
                           #'*-rope-node
                           ;; internal hashing/equality
                           #'*-chunk-hash
                           #'*-node-hash
                           #'*-rope-content=?))

    ;; -------------------------------------------------------------------------
    ;; Chunk Operations
    ;; -------------------------------------------------------------------------

    (define (*-chunk? x) (chunk? x))
    (define (*-chunk-limit) (chunk-limit.callable))
    (define (*-chunk-empty) (chunk-empty.callable))
    (define (*-chunk-length c) (chunk-length c))
    (define (*-chunk-ref c i) (chunk-ref c i))
    (define (*-chunk-slice c i k) (chunk-slice c i k))
    (define (*-chunk-append . cs) (chunk-append cs))

    (define *-chunk-width
      (if (number? elem-width)
          (λ (c) (* (chunk-length c) elem-width))
          (λ (c) (for/sum ([i (in-range (chunk-length c))]) (elem-width c i)))))

    ;; -------------------------------------------------------------------------
    ;; Element Operations
    ;; -------------------------------------------------------------------------

    (define (*-elem-width c i) (elem-width.callable c i))

    ;; -------------------------------------------------------------------------
    ;; Tree Construction
    ;; -------------------------------------------------------------------------

    (struct *-rope-leaf rope-leaf () #:transparent)
    (struct *-rope-node rope-node () #:transparent)

    (define (*-rope? x) (or (*-rope-leaf? x) (*-rope-node? x)))))

;; =============================================================================
;; Core rope type functionality that depends on hashing / equality internally.
;;
;; We do this at the end of the module expansion so that an Eq class can be
;; declared before these definitions are pinned down.
;;
;; CAVEATS:
;;
;; - Custom Eq classes must be defined in the same module as their type
;;   definition.
;; - Calling define-rope-Eq-instance from another module will generate the
;;   external bindings, but the type's definitions will silently continue to
;;   use the defaults internally.
;; - Although the bindings generated for the Eq class are visible immediately
;;   after define-rope-Eq-class is called, the non-primitive bindings
;;   generated for the underlying type are not visible to user-supplied code
;;   anywhere within the enclosing module.

(define-syntax-parse-rule (finish-rope-type type-id:id)
  #:do [(define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))
        (define desc-id (format-id (attribute type-id) "rope:~a:Eq" (syntax-e #'type-id)))
        (define desc (syntax-local-value desc-id (λ () #f)))
        (define (Eq-prim x) (and desc (cdr (assoc x (rope-class-descriptor-primitives desc)))))]

  ;; core operations
  #:with *-chunk?       (mk* "~a-chunk?")
  #:with *-chunk-limit  (mk* "~a-chunk-limit")
  #:with *-chunk-empty  (mk* "~a-chunk-empty")
  #:with *-chunk-length (mk* "~a-chunk-length")
  #:with *-chunk-width  (mk* "~a-chunk-width")
  #:with *-chunk-ref    (mk* "~a-chunk-ref")
  #:with *-chunk-slice  (mk* "~a-chunk-slice")
  #:with *-chunk-append (mk* "~a-chunk-append")
  #:with *-elem-width   (mk* "~a-elem-width")

  ;; tree construction
  #:with *-rope-leaf    (mk* "~a-rope-leaf")
  #:with *-rope-node    (mk* "~a-rope-node")
  #:with *-rope-leaf?   (mk* "~a-rope-leaf?")
  #:with *-rope-node?   (mk* "~a-rope-node?")
  #:with *-rope?        (mk* "~a-rope?")

  ;; smart constructors
  #:with make-*-rope-leaf  (mk* "make-~a-rope-leaf")
  #:with make-*-rope-node  (mk* "make-~a-rope-node")
  #:with make-empty-*-rope (mk* "make-empty-~a-rope")

  ;; Eq members
  #:with *-chunk=?   (or (Eq-prim 'x-chunk=?)   #'equal?)
  #:with *-elem=?    (or (Eq-prim 'x-elem=?)    #'equal?)
  #:with *-elem-hash (or (Eq-prim 'x-elem-hash) #'equal-hash-code)
  #:with ((~optional *-chunk-overlap=?)) (if desc (list (Eq-prim 'x-chunk-overlap=?)) null)

  ;; internal hashing / equality
  #:with *-chunk-hash     (mk* "~a-chunk-hash")
  #:with *-node-hash      (mk* "~a-node-hash")
  #:with *-rope-content=? (mk* "~a-rope-content=?")

  ;; conversions
  #:with *->rope (mk* "~a->rope")
  #:with rope->* (mk* "rope->~a")

  ;; basic operations
  #:with *-rope-concat       (mk* "~a-rope-concat")
  #:with *-rope-append2      (mk* "~a-rope-append2")
  #:with *-rope-append       (mk* "~a-rope-append")
  #:with *-rope-split        (mk* "~a-rope-split")
  #:with *-rope-ref          (mk* "~a-rope-ref")
  #:with *-rope-offset-index (mk* "~a-rope-offset-index")
  #:with *-rope-cut          (mk* "~a-rope-cut")
  #:with *-rope-slice        (mk* "~a-rope-slice")
  #:with *-rope-splice       (mk* "~a-rope-splice")

  (begin

    ;; -------------------------------------------------------------------------
    ;; Smart Constructors
    ;; -------------------------------------------------------------------------

    (define (make-*-rope-leaf c) (make-rope-leaf type-id c))
    (define (make-*-rope-node l r) (make-rope-node type-id l r))
    (define (make-empty-*-rope) (make-empty-rope type-id))

    ;; -------------------------------------------------------------------------
    ;; Hashing
    ;; -------------------------------------------------------------------------

    (define (*-chunk-hash c)
      ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
      (define n   (*-chunk-length c))
      (define n/4 (quotient n 4))
      (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
        (cond
          [(< j n/4)
           (define base (* j 4))
           (define e0 (bitwise-and (*-elem-hash (*-chunk-ref c base))        M))
           (define e1 (bitwise-and (*-elem-hash (*-chunk-ref c (+ base 1)))  M))
           (define e2 (bitwise-and (*-elem-hash (*-chunk-ref c (+ base 2)))  M))
           (define e3 (bitwise-and (*-elem-hash (*-chunk-ref c (+ base 3)))  M))
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
                 (let ([e (bitwise-and (*-elem-hash (*-chunk-ref c i)) M)])
                   (tail (+ i 1)
                         (fxmodulo-M (+ h (* e p)))
                         (fxmodulo-M (* p X))))))])))

    (define (*-node-hash l r)
      (define hl (rope-hash-h l))
      (define pl (rope-hash-p l))
      (define hr (rope-hash-h r))
      (define pr (rope-hash-p r))
      (values (fxmodulo-M (+ hl (* pl hr)))
              (fxmodulo-M (* pl pr))))

    ;; -------------------------------------------------------------------------
    ;; Content-Based Equality
    ;; -------------------------------------------------------------------------

    (define (*-rope-content=? a b)
      (define overlap=?
        (~? *-chunk-overlap=?
            (λ (c d ic id k)
              (for/and ([i (in-range k)])
                (*-elem=? (*-chunk-ref c (+ ic i))
                          (*-chunk-ref d (+ id i)))))))

      (define (chunk-done? c i)
        (or (not c) (>= i (*-chunk-length c))))

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
          (cond
            [(and c (< i (*-chunk-length c)))
             (values c i stack)]
            [(null? stack)
             (values #f 0 null)]
            [(rope-leaf? (car stack))
             (loop (rope-leaf-chunk (car stack)) 0 (cdr stack))]
            [else
             (loop c i (list* (rope-node-left (car stack))
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
                 (cond
                   [(and (not ca**) (not cb**)) #t]
                   [(or  (not ca**) (not cb**)) #f]
                   [else
                    (define len-a (*-chunk-length ca**))
                    (define len-b (*-chunk-length cb**))
                    (if (and (= len-a len-b) (= ia** ib**))
                        (*-chunk=? ca** cb**)
                        (let ([k (min (- len-a ia**) (- len-b ib**))])
                          (and (overlap=? ca** cb** ia** ib** k)
                               (walk ca** (+ ia** k) stack-a**
                                     cb** (+ ib** k) stack-b**))))])))))

    ;; -------------------------------------------------------------------------
    ;; Conversions
    ;; -------------------------------------------------------------------------

    (define (*->rope c) (chunk->rope type-id c))
    (define (rope->* a) (rope->chunk type-id a))

    ;; -------------------------------------------------------------------------
    ;; Basic Operations
    ;; -------------------------------------------------------------------------

    (define (*-rope-concat a b) (rope-concat type-id a b))
    (define (*-rope-append2 a b) (rope-append2 type-id a b))
    ;; (define (*-rope-append       as)      (rope-append       type-id as))
    ;; (define (*-rope-split        a i)     (rope-split        type-id a i))
    ;; (define (*-rope-ref          a i)     (rope-ref          type-id a i))
    ;; (define (*-rope-offset-index a p)     (rope-offset-index type-id a p))
    ;; (define (*-rope-cut          a i k)   (rope-cut          type-id a i k))
    ;; (define (*-rope-slice        a i k)   (rope-slice        type-id a i k))
    ;; (define (*-rope-splice       a i k b) (rope-splice       type-id a i k b))

    ))
