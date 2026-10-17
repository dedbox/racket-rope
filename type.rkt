#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/syntax
                     rope/stxclasses
                     syntax/parse
                     "./private/scope.rkt"
                     "./private/signature.rkt"
                     "./private/type.rkt")
         rope/generic
         rope/rope
         rope/signature
         syntax/parse/define
         "./private/hash.rkt"
         "./private/type-scope.rkt")

(provide (all-defined-out))

(define-syntax (define-member stx)
  (raise-syntax-error #f "allowed only in the body of realize-rope-type" stx))

(define-syntax (define-member-struct stx)
  (raise-syntax-error #f "allowed only in the body of realize-rope-type" stx))

(define-syntax (define-member-via stx)
  (raise-syntax-error #f "allowed only in the body of realize-rope-type" stx))

;; ---------------------------------------------------------------------------------------
;; type-member
;; ---------------------------------------------------------------------------------------

;; Expands to the realized identifer of member `key` of type τ.
(define-syntax (type-member stx)
  (syntax-parse stx
    [(_ τ:id key:id)
     (define desc (lookup-type 'type-member stx #'τ))
     (define id (rope-type-ref desc (syntax-e #'key)))
     (unless id
       (raise-syntax-error 'type-member
         (format "unknown type member ~a of type ~a~a"
                 (syntax-e #'key) (rope-type-name desc)
                 (did-you-mean (syntax-e #'key) (hash-keys (rope-type-members desc))))
         stx #'key))
     (syntax-local-introduce id)]))

;; ---------------------------------------------------------------------------------------
;; realize-rope-type
;; ---------------------------------------------------------------------------------------

(begin-for-syntax
  ;; keys    : keys this form defines
  ;; key-ids : the identifiers the keys were taken from
  ;; emit    : ids self → definition-syntax
  (struct mdef (keys key-ids emit))

  (define (scope-bodies τ self ids bodies)
    (define keys (remove-duplicates (map car (collect-member-refs τ bodies))))
    (with-syntax ([τ τ]
                  [self self]
                  [(ref ...) (for/list ([k (in-list keys)]) (member-ref-id τ k))]
                  [(target ...) (for/list ([k (in-list keys)]) (hash-ref ids k))]
                  [(body ...) bodies])
      #'(with-type-scope (τ self) ([ref target] ...) body ...)))

  (define (parse-member τ form)
    (define (key-of id)
      (or (member-ref-key τ id)
          (raise-syntax-error 'realize-rope-type
            (format "expected a member name of the form ~a.<key>" (syntax-e τ))
            form id)))

    (syntax-parse form
      #:literals (define-member define-member-struct define-member-via)

      ;; (define-member (τ.key . formals) body ...+)
      [(define-member (m:id . formals) body ...+)
       (define k (key-of #'m))
       (mdef (list k) (list #'m)
             (λ (ids self)
               (with-syntax ([rid (hash-ref ids k)]
                             [scoped (scope-bodies τ self ids (attribute body))])
                 #'(define (rid . formals) scoped))))]

      ;; (define-member τ.key expr)
      [(define-member m:id expr)
       (define k (key-of #'m))
       (mdef (list k) (list #'m)
             (λ (ids self)
               (with-syntax ([rid (hash-ref ids k)]
                             [scoped (scope-bodies τ self ids (list #'expr))])
                 #'(define rid scoped))))]

      ;; (define-member-struct (τ.ctor τ.pred) super (field ...) option ...)
      [(define-member-struct (c:id p:id) super:id (field ...) opt ...)
       (define kc (key-of #'c))
       (define kp (key-of #'p))
       (mdef (list kc kp) (list #'c #'p)
             (λ (ids self)
               (define cid (hash-ref ids kc))
               (define pid (hash-ref ids kp))
               (unless (equal? (format "~a?" (syntax-e cid)) (symbol->string (syntax-e pid)))
                 (raise-syntax-error 'define-member-struct
                   (format "realized names ~a and ~a disagree: struct derives the predicate ~a?"
                           (syntax-e cid) (syntax-e pid) (syntax-e cid))
                   form #'p))
               (with-syntax ([cid cid]) #'(struct cid super (field ...) opt ...))))]

      ;; (define-member-via τ.key macro arg ...)  ↝  (macro realized self arg ...)
      [(define-member-via m:id macro:id arg ...)
       (define k (key-of #'m))
       (mdef (list k) (list #'m)
             (λ (ids self)
               (with-syntax ([rid (hash-ref ids k)] [self self]) #'(macro rid self arg ...))))]

      [_ (raise-syntax-error 'realize-rope-type
           "expected define-member, define-member-struct, or define-member-via"
           form)])))

(define-syntax-parse-rule (realize-rope-type (τ:id name:id) sig-id:id form ...)
  #:do [(define sig (lookup-signature 'realize-rope-type #'sig-id this-syntax))
        (define keys (signature-keys sig))
        (define forms (attribute form))
        (define defs (for/list ([form (in-list forms)]) (parse-member #'τ form)))

        ;; Every defined key is declared precisely once.
        (define refs
          (for*/list ([def (in-list defs)]
                      [ref (in-list (map cons (mdef-keys def) (mdef-key-ids def)))])
            ref))
        (check-member-refs 'realize-rope-type sig #'τ refs this-syntax)
        (define defined
          (for/fold ([seen (hasheq)]) ([ref (in-list refs)])
            (when (hash-has-key? seen (car ref))
              (raise-syntax-error 'realize-rope-type
                (format "member ~a is defined twice" (car ref)) this-syntax (cdr ref)))
            (hash-set seen (car ref) (cdr ref))))

        ;; Every type variable reference has a declared member.
        (check-member-refs 'realize-rope-type sig #'τ (collect-member-refs #'τ forms) this-syntax)

        ;; Every declared member is defined.
        (define missing (filter (λ (k) (not (hash-has-key? defined k))) keys))
        (unless (null? missing)
          (raise-syntax-error 'realize-rope-type
            (format "members declared in ~a but not defined: ~a"
                    (signature-name sig)
                    (string-join (map symbol->string missing) ", "))))

        ;; Realized names are exclusively defined here.
        (define ids (for/hasheq ([spec (in-list (signature-specs sig))])
                      (define k (member-spec-key spec))
                      (values k (format-id #'name (member-spec-pattern spec) #'name
                                           #:source (hash-ref defined k)))))
        (define self (format-id #'name "rope:~a" #'name))]
  #:with (key ...) (for/list ([k (in-list keys)]) (datum->syntax #f k))
  #:with (rid ...) (for/list ([k (in-list keys)]) (hash-ref ids k))
  #:with (def ...) (for/list ([d (in-list defs)]) ((mdef-emit d) ids self))
  #:with self self
  (begin
    (define-syntax self
      (rope-type 'name #'self #'sig-id (make-immutable-hasheq (list (cons 'key #'rid) ...))))
    def ...))

;; ---------------------------------------------------------------------------------------
;; define-rope-type
;; ---------------------------------------------------------------------------------------

(define-syntax-parse-rule (define-rope-type name:id
                            (~alt (~once (~seq #:chunk?       chunk?:id+fun1))
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
  (realize-rope-type (τ name) rope-signature

    ;; -----------------------------------------------------------------------------------
    ;; Primitives
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.chunk? x)          (chunk? x))
    (define-member (τ.chunk-limit)       (chunk-limit.callable))
    (define-member (τ.chunk-empty)       (chunk-empty.callable))
    (define-member (τ.chunk-length c)    (chunk-length c))
    (define-member (τ.chunk-ref c i)     (chunk-ref c i))
    (define-member (τ.chunk-slice c i k) (chunk-slice c i k))
    (define-member (τ.chunk-append . cs) (chunk-append cs))

    (define-member τ.chunk-width
      (if (number? elem-width)
          (λ (c) (* (τ.chunk-length c) elem-width))
          (λ (c) (for/sum ([i (in-range (τ.chunk-length c))])
                   (τ.elem-width c i)))))

    (define-member (τ.elem-width c i) (elem-width.callable c i))

    ;; -----------------------------------------------------------------------------------
    ;; Tree Construction
    ;; -----------------------------------------------------------------------------------

    (define-member-struct (τ.rope-leaf τ.rope-leaf?) rope-leaf () #:transparent)
    (define-member-struct (τ.rope-node τ.rope-node?) rope-node () #:transparent)

    (define-member (τ.rope? x) (or (τ.rope-leaf? x) (τ.rope-node? x)))

    ;; -----------------------------------------------------------------------------------
    ;; Smart Constructors
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.make-rope-leaf c)   (make-rope-leaf  τ c))
    (define-member (τ.make-rope-node l r) (make-rope-node  τ l r))
    (define-member (τ.make-empty-rope)    (make-empty-rope τ))

    ;; -----------------------------------------------------------------------------------
    ;; Hashing
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.elem-hash x) ((~? elem-hash-arg equal-hash-code) x))

    (define-member (τ.chunk-hash c)
      ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
      (define n   (τ.chunk-length c))
      (define n/4 (quotient n 4))
      (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
        (cond
          [(< j n/4)
           (define base (* j 4))
           (define e0 (bitwise-and (τ.elem-hash (τ.chunk-ref c base)) M))
           (define e1 (bitwise-and (τ.elem-hash (τ.chunk-ref c (+ base 1))) M))
           (define e2 (bitwise-and (τ.elem-hash (τ.chunk-ref c (+ base 2))) M))
           (define e3 (bitwise-and (τ.elem-hash (τ.chunk-ref c (+ base 3))) M))
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
                (define e (bitwise-and (τ.elem-hash (τ.chunk-ref c i)) M))
                (tail (+ i 1) (modulo-M (+ h (* e p))) (modulo-M (* p X)))]))])))

    (define-member (τ.node-hash l r)
      (define hl (rope-hash-h l))
      (define pl (rope-hash-p l))
      (define hr (rope-hash-h r))
      (define pr (rope-hash-p r))
      (values (modulo-M (+ hl (* pl hr))) (modulo-M (* pl pr))))

    (define-member (τ.rope-hash a) (rope-hash τ a))

    ;; -----------------------------------------------------------------------------------
    ;; Content-Based Equality
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.chunk=? c d) ((~? chunk=?-arg equal?) c d))
    (define-member (τ.elem=? x y)  ((~? elem=?-arg equal?) x y))

    (define-member (τ.chunk-overlap=? c d ic id k)
      (~? (chunk-overlap=?-arg c d ic id k)
          (for/and ([i (in-range k)])
            (τ.elem=? (τ.chunk-ref c (+ ic i))
                      (τ.chunk-ref d (+ id i))))))

    (define-member (τ.rope=? a b)
      (define (chunk-done? c i) (or (not c) (>= i (τ.chunk-length c))))

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
          (cond [(and c (< i (τ.chunk-length c))) (values c i stack)]
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
               (let walk ([ca #f] [ia 0] [stack-a (list a)] [cb #f]
                                  [ib 0] [stack-b (list b)])
                 (define-values (ca* ia* stack-a* cb* ib* stack-b*)
                   (skip-shared ca ia stack-a cb ib stack-b))
                 (define-values (ca** ia** stack-a**) (advance ca* ia* stack-a*))
                 (define-values (cb** ib** stack-b**) (advance cb* ib* stack-b*))
                 (cond [(and (not ca**) (not cb**)) #t]
                       [(or  (not ca**) (not cb**)) #f]
                       [else
                        (define len-a (τ.chunk-length ca**))
                        (define len-b (τ.chunk-length cb**))
                        (if (and (= len-a len-b) (= ia** ib**))
                            (τ.chunk=? ca** cb**)
                            (let ([k (min (- len-a ia**) (- len-b ib**))])
                              (and (τ.chunk-overlap=? ca** cb** ia** ib** k)
                                   (walk ca** (+ ia** k) stack-a**
                                         cb** (+ ib** k) stack-b**))))])))))

    ;; -----------------------------------------------------------------------------------
    ;; Conversions
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.source->rope c) (source->rope τ c))
    (define-member (τ.rope->source a) (rope->source τ a))

    ;; -----------------------------------------------------------------------------------
    ;; Basic Operations
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.rope-concat a b)       (rope-concat       τ a b))
    (define-member (τ.rope-append2 a b)      (rope-append2      τ a b))
    (define-member (τ.rope-append as)        (rope-append       τ as))
    (define-member (τ.rope-split a i)        (rope-split        τ a i))
    (define-member (τ.rope-ref a i)          (rope-ref          τ a i))
    (define-member (τ.rope-offset-index a p) (rope-offset-index τ a p))
    (define-member (τ.rope-cut a i k)        (rope-cut          τ a i k))
    (define-member (τ.rope-slice a i k)      (rope-slice        τ a i k))
    (define-member (τ.rope-splice a i k b)   (rope-splice       τ a i k b))

    ;; -----------------------------------------------------------------------------------
    ;; Cursors
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.cursor->rope cur)         (cursor->rope         τ cur))
    (define-member (τ.cursor-peek cur)          (cursor-peek          τ cur))
    (define-member (τ.cursor-split cur)         (cursor-split         τ cur))
    (define-member (τ.mutable-cursor->rope cur) (mutable-cursor->rope τ cur))
    (define-member (τ.mutable-cursor-peek cur)  (mutable-cursor-peek  τ cur))

    ;; -----------------------------------------------------------------------------------
    ;; Folds
    ;; -----------------------------------------------------------------------------------

    (define-member (τ.rope-foldl proc init a) (rope-foldl τ proc init a))
    (define-member (τ.rope-foldr proc init a) (rope-foldr τ proc init a))

    ;; -----------------------------------------------------------------------------------
    ;; Sequences
    ;; -----------------------------------------------------------------------------------

    ;; These must be bound directly to an identifier or they will not expand to the faster
    ;; implementation.
    (define-member-via τ.in-rope   define-rope-sequence)
    (define-member-via τ.in-cursor define-cursor-sequence)))
