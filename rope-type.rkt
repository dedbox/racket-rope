#lang racket/base

;; rope/rope-type.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor
                     syntax/parse)
         racket/fixnum
         rope2/generic-ops
         rope2/private/hash
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class (fun arity)
    #:description "a zero-argument function"
    #:opaque
    #:literals (lambda λ)
    (pattern ((~or lambda λ) (args:id ...) . _)
             #:when (= arity (length (attribute args)))))

  (define-syntax-class vector
    #:description "a vector literal"
    #:opaque
    (pattern x #:when (vector? (syntax-e #'x))))

  (define-syntax-class hash
    #:description "a hash literal"
    #:opaque
    (pattern x #:when (hash? (syntax-e #'x))))

  (define-syntax-class box
    #:description "a box literal"
    #:opaque
    (pattern x #:when (box? (syntax-e #'x))))

  (define-syntax-class prefab
    #:description "a prefab struct literal"
    #:opaque
    (pattern x #:when (struct? (syntax-e #'x))))

  (define-syntax-class self-quoting-lit
    #:description "a self-quoting literal"
    #:opaque
    ;; built-in syntax classes
    (pattern (~or :number :boolean :string :bytes :char :regexp :byte-regexp))
    ;; custom syntax classes
    (pattern (~or :vector :hash :box :prefab)))

  (define-syntax-class quotable-lit
    #:description "a quotable literal"
    #:opaque
    (pattern (~or :self-quoting-lit :keyword :id)))

  (define-syntax-class lit
    #:description "a literal value"
    #:opaque
    #:literals (quote)
    ;; self-quoting atomic literals
    (pattern :self-quoting-lit)
    ;; quoted literals
    (pattern (quote (~or :quotable-lit (:quotable-lit ...)))))

  (define-syntax-class id+fun1
    #:description "an identifier or a one-argument function"
    #:opaque
    (pattern (~or :id (~var _ (fun 1)))))

  (define-syntax-class id+fun2
    #:description "an identifier or a two-argument function"
    #:opaque
    (pattern (~or :id (~var _ (fun 2)))))

  (define-syntax-class id+fun3
    #:description "an identifier or a three-argument function"
    #:opaque
    (pattern (~or :id (~var _ (fun 3)))))

  (define-syntax-class id+fun5
    #:description "an identifier or a five-argument function"
    #:opaque
    (pattern (~or :id (~var _ (fun 5)))))

  (define-syntax-class nat+id+fun0
    #:description "a natural number, an identifier, or a zero-argument function"
    #:opaque
    #:attributes (callable)
    (pattern n:nat
             #:attr callable #'(λ () n))
    (pattern (~and callable (~or :id (~var _ (fun 0))))))

  (define-syntax-class nat+id+fun2
    #:description "a natural number, an identifier, or a two-argument function"
    #:opaque
    #:attributes (callable)
    (pattern n:nat
             #:attr callable #'(λ (_x _y) n))
    (pattern (~and callable (~or :id (~var _ (fun 2))))))

  (define-syntax-class lit+id+fun0
    #:description "a literal value, an identifier, or a zero-argument function"
    #:opaque
    #:attributes (callable)
    (pattern l:lit
             #:attr callable #'(λ () l))
    (pattern (~and callable (~or :id (~var _ (fun 0)))))))

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
  #:with (~var rope:*)           (mk* "rope:~a")

  ;; per-chunk primitives
  #:with *-rope-chunk?           (mk* "~a-rope-chunk?")
  #:with *-rope-chunk-limit      (mk* "~a-rope-chunk-limit")
  #:with *-rope-chunk-empty      (mk* "~a-rope-chunk-empty")
  #:with *-rope-chunk-length     (mk* "~a-rope-chunk-length")
  #:with *-rope-chunk-width      (mk* "~a-rope-chunk-width")
  #:with *-rope-chunk-ref        (mk* "~a-rope-chunk-ref")
  #:with *-rope-chunk-slice      (mk* "~a-rope-chunk-slice")
  #:with *-rope-chunk-append     (mk* "~a-rope-chunk-append")
  #:with *-rope-chunk-compare    (mk* "~a-rope-chunk-compare")
  #:with *-rope-chunk-overlap=?  (mk* "~a-rope-chunk-overlap=?")

  ;; per-element primitives
  #:with *-rope-elem-width       (mk* "~a-rope-elem-width")
  #:with *-rope-elem-hash        (mk* "~a-rope-elem-hash")

  ;; rope structs
  #:with *-rope-leaf             (mk* "~a-rope-leaf")
  #:with *-rope-node             (mk* "~a-rope-node")

  ;; rope predicates
  #:with *-rope-leaf?            (mk* "~a-rope-leaf?")
  #:with *-rope-node?            (mk* "~a-rope-node?")
  #:with *-rope?                 (mk* "~a-rope?")

  ;; smart constructors
  #:with make-*-rope-leaf        (mk* "make-~a-rope-leaf")
  #:with make-*-rope-node        (mk* "make-~a-rope-node")

  ;; conversions
  #:with *-chunk->rope           (mk* "~a-chunk->rope")
  #:with *-rope->chunk           (mk* "~a-rope->chunk")

  ;; basic operations
  #:with *-rope-concat           (mk* "~a-rope-concat")

  ;; hashing
  #:with *-rope-hash-cache       (mk* "~a-rope-hash-cache")
  #:with *-rope-chunk-hash       (mk* "~a-rope-chunk-hash")
  #:with *-rope-hash             (mk* "~a-rope-hash")

  (begin

    ;; rope type descriptor
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
                            #'*-rope-node))

    ;; per-chunk primitives
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
          (λ (c) (for/sum ([i (in-range (chunk-length c))])
                   (elem-width c (chunk-ref c i))))))

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

    ;; per-element primitives
    (define (*-rope-elem-width c i) (elem-width c i))
    (define (*-rope-elem-hash  c)   (~? (elem-hash c) (equal-hash-code c)))

    ;; rope structs & predicates
    (struct *-rope-leaf rope-leaf () #:transparent)
    (struct *-rope-node rope-node () #:transparent)

    (define (*-rope? x) (or (*-rope-leaf? x) (*-rope-node? x)))

    ;; smart constructors
    (define (make-*-rope-leaf c)   (make-rope-leaf type-id c))
    (define (make-*-rope-node l r) (make-rope-node type-id l r))

    ;; conversions
    (define (*-chunk->rope c) (chunk->rope type-id c))
    (define (*-rope->chunk a) (rope->chunk type-id a))

    ;; basic operations
    (define (*-rope-concat l r) (rope-concat type-id l r))

    ;; hashing
    (define (*-rope-chunk-hash c X X⁴)
      (define n (chunk-length c))
      (define limit (fx- n (fxand n 3)))
      (let loop ([i 0] [h 0])
        (cond
          [(fx= i limit)
           (let tail-loop ([j i] [th h])
             (cond
               [(fx= j n) th]
               [else
                (define next-th
                  (fxmodulo-M (fx+ (fxmodulo-M (fx* th X)) (elem-hash (chunk-ref c j)))))
                (tail-loop (fx+ j 1) next-th)]))]
          [else
           (define c0 (elem-hash (chunk-ref c i)))
           (define c1 (elem-hash (chunk-ref c (fx+ i 1))))
           (define c2 (elem-hash (chunk-ref c (fx+ i 2))))
           (define c3 (elem-hash (chunk-ref c (fx+ i 3))))
           ;; h_new = h * X^4 + c0*X^3 + c1*X^2 + c2*X + c3
           (define poly (fxmodulo-M (fx+ c3 (fx* X (fx+ c2 (fx* X (fx+ c1 (fx* X c0))))))))
           (loop (fx+ i 4) (fxmodulo-M (fx+ (fxmodulo-M (fx* h X⁴)) poly)))])))

    (define (*-rope-hash a)
      (cond
        [(rope-leaf? a)
         (values (*-rope-chunk-hash (rope-leaf-chunk a) X₁ X₁⁴)
                 (*-rope-chunk-hash (rope-leaf-chunk a) X₂ X₂⁴))]
        [else
         (define-values (hl1 hl2) (*-rope-hash (rope-node-left a)))
         (define-values (hr1 hr2) (*-rope-hash (rope-node-right a)))
         (define-values (right-len) (rope-length (rope-node-right a)))
         (hash-combine hl1 hr1 hl2 hr2 right-len)]))

    ))
