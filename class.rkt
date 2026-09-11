#lang racket/base

;; rope/class.rkt

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "./private/class/descriptor.rkt"
                     "./private/type/descriptor.rkt")
         rope2/rope
         syntax/parse/define
         "./private/hash.rkt")

(provide (all-defined-out))

;; Needed for custom equal+hash:
;;
;;   Derived:
;;     *-chunk-hash
;;     *-node-hash
;;     *-rope-hash
;;     *-rope-content=?
;;
;;   Primitive:
;;     *-elem-hash        (has default)
;;     *-chunk=?          (has default)
;;     *-chunk-overlap=?  (has default)
;;
;; Every type has a default Eq instance. It can be overriden for better
;; performancee, but can't be removed.
;;
;; Maybe we add Eq instantiation to type definition output for Ord to require,
;; then manually defined types (infinite streams?) might use standard class
;; definer for their Eq instances.

(begin-for-syntax
  (define (mk-id stx fmt) (format-id stx fmt (syntax-e stx))))

;; Defines the public Eq API. Types without an Eq instance will use the
;; default hashing and content-based equality operations internally, but the
;; user-facing derived operations (*-chunk-hash, *-node-hash,
;; *-rope-content=?) will not be defined. Calling this macro with no keyword
;; arguments will bind these operations to the defaults.
(define-syntax-parse-rule (define-rope-Eq-instance type-id:id
                            (~optional (~seq #:elem-hash elem-hash:expr))
                            (~optional (~seq #:chunk=? chunk=?:expr))
                            (~optional (~seq #:chunk-overlap=? chunk-overlap=?)))
  #:do [(define type-desc-id (format-id #'type-id "rope:~a" (syntax-e #'type-id)))
        (define type-desc (syntax-local-value type-desc-id (λ () #f)))
        (unless type-desc
          (raise-syntax-error 'define-rope-Eq-instance "expected a rope type" #'type-id))]

  ;; type primitives
  #:with chunk?                (rope-type-descriptor-chunk?                type-desc)
  #:with chunk-limit           (rope-type-descriptor-chunk-limit           type-desc)
  #:with chunk-empty           (rope-type-descriptor-chunk-empty           type-desc)
  #:with chunk-length          (rope-type-descriptor-chunk-length          type-desc)
  #:with chunk-width           (rope-type-descriptor-chunk-width           type-desc)
  #:with chunk-ref             (rope-type-descriptor-chunk-ref             type-desc)
  #:with chunk-slice           (rope-type-descriptor-chunk-slice           type-desc)
  #:with chunk-append          (rope-type-descriptor-chunk-append          type-desc)
  #:with chunk-compare         (rope-type-descriptor-chunk-compare         type-desc)
  #:with chunk-compare-overlap (rope-type-descriptor-chunk-compare-overlap type-desc)
  #:with elem-width            (rope-type-descriptor-elem-width            type-desc)
  #:with leaf-constructor      (rope-type-descriptor-leaf-constructor      type-desc)
  #:with node-constructor      (rope-type-descriptor-node-constructor      type-desc)

  ;; class descriptor
  #:with (~var rope:*:%)   (format-id #'type-id "rope:~a:~a" (syntax-e #'type-id) 'Eq)

  ;; class primitives
  #:with *-elem-hash       (mk-id #'type-id "~a-elem-hash")
  #:with *-chunk=?         (mk-id #'type-id "~a-chunk=?")
  #:with *-chunk-overlap=? (mk-id #'type-id "~a-chunk-overlap=?")

  ;; derived operations
  #:with *-chunk-hash      (mk-id #'type-id "~a-chunk-hash")
  #:with *-node-hash       (mk-id #'type-id "~a-node-hash")
  #:with *-rope-hash       (mk-id #'type-id "~a-rope-hash")
  #:with *-rope-content=?  (mk-id #'type-id "~a-rope-content=?")
  (begin
    (define-syntax rope:*:%
      (rope-class-descriptor
       ;; primitives
       #'*-elem-hash
       #'*-chunk=?
       #'*-chunk-overlap=?
       ;; derived operations
       #'*-chunk-hash
       #'*-node-hash
       #'*-rope-hash
       #'*-rope-content=?))

    ;; -------------------------------------------------------------------------
    ;; Class Primitives
    ;; -------------------------------------------------------------------------

    (define (*-elem-hash x) ((~? elem-hash equal-hash-code) x))
    (define (*-chunk=? c d) ((~? chunk=? equal?) c d))

    (define (*-chunk-overlap=? c d ic id k)
      (~? (chunk-overlap=? c d ic id k)
          (for/and ([i (in-range k)])
            (equal? (chunk-ref c (+ ic i))
                    (chunk-ref d (+ id i))))))

    ;; -------------------------------------------------------------------------
    ;; Derived Operations
    ;; -------------------------------------------------------------------------

    (define (*-chunk-hash c)
      ;; h = h₀ + X·h₁ + X²·h₂ + X³·h₃    hₖ = Σⱼ e₄ⱼ₊ₖ·(X⁴)ʲ
      (define n   (chunk-length c))
      (define n/4 (quotient n 4))
      (let loop ([j 0] [h0 0] [h1 0] [h2 0] [h3 0] [q 1])
        (cond
          [(< j n/4)
           (define base (* j 4))
           (define e0 (bitwise-and (*-elem-hash (chunk-ref c base))        M))
           (define e1 (bitwise-and (*-elem-hash (chunk-ref c (+ base 1)))  M))
           (define e2 (bitwise-and (*-elem-hash (chunk-ref c (+ base 2)))  M))
           (define e3 (bitwise-and (*-elem-hash (chunk-ref c (+ base 3)))  M))
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
                 (let ([e (bitwise-and (*-elem-hash (chunk-ref c i)) M)])
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

    (define (*-rope-hash a)
      (if (rope-leaf? a)
          (*-chunk-hash (rope-leaf-chunk a))
          (*-node-hash (rope-node-left a) (rope-node-right a))))

    (define (*-rope-content=? a b)
      (define (chunk-done? c i)
        (or (not c) (>= i (chunk-length c))))
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
            [(and c (< i (chunk-length c)))
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
                    (define len-a (chunk-length ca**))
                    (define len-b (chunk-length cb**))
                    (if (and (= len-a len-b) (= ia** ib**))
                        (*-chunk=? ca** cb**)
                        (let ([k (min (- len-a ia**) (- len-b ib**))])
                          (and (*-chunk-overlap=? ca** cb** ia** ib** k)
                               (walk ca** (+ ia** k) stack-a**
                                     cb** (+ ib** k) stack-b**))))])))))

    ))


;; (define-syntax-parse-rule (define-rope-class class-id:id
;;                             (~optional (~seq #:requires (req-id:id ...)))
;;                             ([f-id:id f-stxcls:id (~optional (~or* (~seq #:default f-default:expr)
;;                                                                    (~seq #:optional)))]
;;                              ...)
;;                             body ...)
;;   (begin
;;     (define-syntax rope:* (rope-class-descriptor))

;;     (define-syntax-parse-rule (define-rope-*-instance ρ:id (~seq f-kw (~var f-impl f-stxcls)) (... ...))
;;       (void))))

;; ;; Type
;; (define-rope-type string
;;   #:chunk?                string?
;;   #:chunk-limit           512
;;   #:chunk-empty           ""
;;   #:chunk-length          string-length
;;   #:chunk-ref             string-ref
;;   #:chunk-slice           (λ (c i k) (substring c i (+ i k)))
;;   #:chunk-append          (λ (cs) (apply string-append cs))
;;   #:elem-width            1
;;   #:elem-hash             char->integer)

;; (define empty-string-rope (make-empty-string-rope))

;; (define (string->rope c) (string-chunk->rope c))
;; (define (rope->string a) (string-rope->chunk a))

;; ;; Class
;; (define-rope-class Ord
;;   #:requires (Eq)
;;   ([chunk-compare         id+fun2 #:optional]
;;    [chunk-compare-overlap id+fun5 #:optional]
;;    [elem<?                id+fun2]
;;    [elem>?                id+fun2]
;;    [elem=?                id+fun2 #:default (λ (x y) (not (or (elem<? x y) (elem>? x y))))]))

;; (define-Ord-op (rope-chunk-compare _ ca cb)
;;   (~? (chunk-compare ca cb)
;;       (let ([k (chunk-limit)])
;;         (let loop ([i 0])
;;           (define x (chunk-ref ca (+ ia i)))
;;           (define y (chunk-ref cb (+ ib i)))
;;           (cond [(elem=? x y) (loop (add1 i))]
;;                 [(elem<? x y) '<]
;;                 [(elem>? x y) '>]
;;                 [(= i k)      '=]
;;                 [else         'incomparable])))))

;; (define (*-chunk-compare ca cb)
;;   ;; how to handle these type-specific, non-macro defs with specializable
;;   ;; names?
;;   ...)

;; (define-Ord-op (*-chunk-compare-overlap _ ca cb ia ib k)
;;   (~? (chunk-compare-overlap ca cb ia ib k)
;;       (~@ (define (loop [i 0])
;;             (define x (chunk-ref ca (+ ia i)))
;;             (define y (chunk-ref cb (+ ib i)))
;;             (cond [(elem=? x y) (compare-loop (add1 i))]
;;                   [(elem<? x y) '<]
;;                   [(elem>? x y) '>]
;;                   [(= i k)      '=]
;;                   [else         'incomparable]))
;;           (if (and (= ia 0) (= k (chunk-length ca))
;;                    (= ib 0) (= k (chunk-length cb)))
;;               (~? (chunk-compare ca cb) (loop))
;;               (loop)))))

;; (define-rope-op (rope-compare-with f0 a b)
;;   (let ([f f0])
;;     (let loop ([c-a (rope->mutable-cursor a)]
;;                [c-b (rope->mutable-cursor b)])
;;       (cond
;;         [(not (or c-a c-b)) '=]
;;         [(not c-a) '<]
;;         [(not c-b) '>]
;;         [else
;;          (define leaf-a (mutable-cursor-leaf c-a))
;;          (define leaf-b (mutable-cursor-leaf c-b))
;;          (define ia (mutable-cursor-rel-idx c-a))
;;          (define ib (mutable-cursor-rel-idx c-b))
;;          (define k  (min (- (rope-length leaf-a) ia) (- (rope-length leaf-b) ib)))
;;          (define result (f (rope-leaf-chunk leaf-a) (rope-leaf-chunk leaf-b) ia ib k))
;;          (if (not (eq? result '=)) result (loop (cursor-advance! c-a k)
;;                                                 (cursor-advance! c-b k)))]))))

;; (define-rope-op (rope-compare a b) (rope-compare-with chunk-compare-overlap a b))

;; (define (*-rope-compare-with f a b)
;;   ;; how to auto-instantiate the arg at `??` with the appropriate type
;;   ;; descriptor id?
;;   (rope-compare-with ?? f a b))

;; (define (*-rope-compare a b)
;;   ;; same as above
;;   (rope-compare ?? a b))

;; (define-rope-ord-instance string #:elem<? char<? #:elem>? char>? #:elem=? char=?)
