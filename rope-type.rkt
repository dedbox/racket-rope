#lang racket/base

;; rope/rope-type.rkt

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor)
         rope2/generic
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

;; A polynomial rolling hash (Rabin–Karp style) that is associative under
;; concatenation.
;;
;; H(A ++ B) depends only on H(A), H(B), and width(A). With a memo table,
;; this becomes O(log n) amortized after small edits.
;;
;; For a chunk r₀ r₁ ... r_{k−1}, define
;;
;;   H(run) ≡ Σᵢ hash(rᵢ) · Pⁱ   (mod M)
;;
;; where M is a Mersenne prime and P is a fixed base coprime to M. The
;; identity
;;
;;   H(A ++ B) ≡ H(A) + P|A| · H(B)   (mod M)
;;
;; is exact and independent of how A ++ B is further subdivided, so
;; caching (H(subtree), P(count(subtree)) mod M) per node makes any parent
;; combination O(1). Hashing a freshly built rope of n elements is O(n).
;; Hashing it again, or hashing any rope sharing structure with an already
;; hashed rope, approaches O(1).

;; If we set M = 2³¹ - 1, the maximum intermediate multiplication is (2³¹
;; - 1)², which requires 62 bits. This fits perfectly into Racket CS's
;; 62-bit fixnum boundary, avoiding any bignum heap allocations.
(define HASH-MOD  (sub1 (expt 2 31))) ; Mersenne prime 2³¹ - 1
(define HASH-BASE (sub1 (expt 2 29))) ; a coprime base < HASH-MOD

(begin-for-syntax
  ;; To add a new type-specific rope operation:
  ;;
  ;; 1. Name the operation by creating an entry in rope-type-ids.
  ;; 2. Bind the name to an identifier at the top of define-rope-type.
  ;; 3. Define the operation at the bottom of define-rope-type.

  (define (rope-type-ids src-stx [type-id #f])
    (define (mk-id fmt) (format-id src-stx fmt (syntax-e type-id)))
    (define (mk-id- fmt)
      (define rope-id
        (if type-id (string-append (symbol->string (syntax-e type-id)) "-") ""))
      (format-id src-stx fmt rope-id))
    (make-hasheq
     `([*-rope-chunk?          . ,(mk-id- "~arope-chunk?")]
       [*-rope-elem-size       . ,(mk-id- "~arope-elem-size")]
       [*-rope-chunk-limit     . ,(mk-id- "~arope-chunk-limit")]
       [*-rope-chunk-empty     . ,(mk-id- "~arope-chunk-empty")]
       [*-rope-chunk-count     . ,(mk-id- "~arope-chunk-count")]
       [*-rope-chunk-size      . ,(mk-id- "~arope-chunk-size")]
       [*-rope-chunk-slice     . ,(mk-id- "~arope-chunk-slice")]
       [*-rope-chunk-append    . ,(mk-id- "~arope-chunk-append")]
       [*-rope-chunk-ref       . ,(mk-id- "~arope-chunk-ref")]
       [*-rope-chunk-compare   . ,(mk-id- "~arope-chunk-compare")]
       [*-rope-chunk-overlap=? . ,(mk-id- "~arope-chunk-overlap=?")]
       [*-rope-hash-cache      . ,(mk-id- "~arope-hash-cache")]
       [*-leaf-poly-hash       . ,(mk-id- "~aleaf-poly-hash")]
       [*-rope-poly-hash       . ,(mk-id- "~arope-poly-hash")]
       [*-rope-content=?       . ,(mk-id- "~arope-content=?")]
       [*-rope-leaf            . ,(mk-id- "~arope-leaf")]
       [*-rope-leaf?           . ,(mk-id- "~arope-leaf?")]
       [*-rope-node            . ,(mk-id- "~arope-node")]
       [*-rope-node?           . ,(mk-id- "~arope-node?")]
       [*-rope?                . ,(mk-id- "~arope?")]
       [make-*-rope-leaf       . ,(mk-id- "make-~arope-leaf")]
       [make-*-rope-node       . ,(mk-id- "make-~arope-node")]
       [make-empty-*-rope      . ,(mk-id- "make-empty-~arope")]
       [*-rope-concat          . ,(mk-id- "~arope-concat")]
       [*-rope-append          . ,(mk-id- "~arope-append")]
       [*-rope-append2         . ,(mk-id- "~arope-append2")]
       [*-rope-split           . ,(mk-id- "~arope-split")]
       [*-rope-ref             . ,(mk-id- "~arope-ref")]
       [*-rope-offset-index    . ,(mk-id- "~arope-offset-index")]
       [*-rope-cut             . ,(mk-id- "~arope-cut")]
       [*-rope-slice           . ,(mk-id- "~arope-slice")]
       [*-rope-splice          . ,(mk-id- "~arope-splice")]
       [*->rope                . ,(mk-id  "~a->rope")]
       [rope->*                . ,(mk-id  "rope->~a")]
       [*-rope-ensure-balance  . ,(mk-id- "~arope-ensure-balance")]
       [*-rope-rebalance       . ,(mk-id- "~arope-rebalance")]
       [*-rope-defrag          . ,(mk-id- "~arope-defrag")]

       [*-rope-forest-add     . ,(mk-id- "~arope-forest-add")]
       [*-rope-forest->rope   . ,(mk-id- "~arope-forest->rope")]
       )))

  (define (raise-def-error msg src1 [src2 #f])
    (raise-syntax-error 'define-rope-type msg src1 src2))

  (define (parse-rope-type-defs src-stx stxs)
    (define def-map (make-hasheq))
    (let loop ([stxs stxs])
      (unless (and (list? stxs) (list? (cdr stxs)) (list? (cddr stxs)))
        (raise-def-error "unexpected end of rope specification" src-stx))
      (define kw-stx  (car  stxs))
      (define def-stx (cadr stxs))
      (define kw-sym
        (syntax-parse kw-stx
          [(~or #:chunk? #:elem-size #:chunk-limit #:chunk-empty #:chunk-count
                #:chunk-size #:chunk-slice #:chunk-append #:chunk-ref
                #:chunk-compare #:chunk-overlap=?)
           (string->symbol (keyword->string (syntax-e kw-stx)))]
          [_ (raise-def-error
              (format "unrecognized rope-specification directive: #:~a" kw-sym)
              src-stx kw-stx)]))
      (when (hash-has-key? def-map kw-sym)
        (raise-def-error "rope-specification keyword already defined" src-stx kw-stx))
      (case kw-sym
        [(elem-size)
         (unless (syntax-parse def-stx
                   #:literals (lambda λ)
                   [(~or :number :id (lambda (:id :id) . _) (λ (:id :id) . _)) #t]
                   [_ #f])
           (raise-def-error "expected a number, an identifier, or a two-argument lambda"
                            src-stx def-stx))]
        [else
         (unless (syntax-parse def-stx
                   #:literals (lambda λ)
                   [(~or :id (lambda . _) (λ . _)) #t]
                   [_ #f])
           (raise-def-error "expected an identifier or a lambda" src-stx def-stx))])
      (hash-set! def-map kw-sym def-stx)
      (unless (null? (cddr stxs)) (loop (cddr stxs))))
    (define (raise-missing-kw-error kw-sym)
      (raise-def-error (format "missing required rope-specification keyword: #:~a" kw-sym)
                       src-stx))
    (define defs
      (for/list ([kw-sym (in-list '(chunk? elem-size chunk-limit chunk-empty chunk-count
                                           chunk-size chunk-slice chunk-append chunk-ref
                                           chunk-compare chunk-overlap=?))])
        (hash-ref def-map kw-sym (λ () (if (or (eq? kw-sym 'chunk-compare)
                                               (eq? kw-sym 'chunk-overlap=?))
                                           #f
                                           (raise-missing-kw-error kw-sym))))))
    (remove* (list #f) defs)))

(define-syntax-parse-rule (define-rope-type type-id:id defs ...)
  #:with (chunk? elem-size chunk-limit chunk-empty chunk-count
                 chunk-size chunk-slice chunk-append chunk-ref
                 (~optional chunk-compare)
                 (~optional chunk-overlap=?))
  (parse-rope-type-defs this-syntax (attribute defs))
  #:do [(define ids (rope-type-ids this-syntax (attribute type-id)))
        (define (id* key) (hash-ref ids key))]
  #:with (~var rope:type-id) (format-id (attribute type-id) "rope:~a" (syntax-e #'type-id))
  #:with *-rope-chunk?          (id* '*-rope-chunk?)
  #:with *-rope-elem-size       (id* '*-rope-elem-size)
  #:with *-rope-chunk-limit     (id* '*-rope-chunk-limit)
  #:with *-rope-chunk-empty     (id* '*-rope-chunk-empty)
  #:with *-rope-chunk-count     (id* '*-rope-chunk-count)
  #:with *-rope-chunk-size      (id* '*-rope-chunk-size)
  #:with *-rope-chunk-slice     (id* '*-rope-chunk-slice)
  #:with *-rope-chunk-append    (id* '*-rope-chunk-append)
  #:with *-rope-chunk-ref       (id* '*-rope-chunk-ref)
  #:with ((~optional *-rope-chunk-compare))
  (if (attribute chunk-compare) (list (id* '*-rope-chunk-compare)) null)
  #:with *-rope-chunk-overlap=? (id* '*-rope-chunk-overlap=?)
  #:with *-rope-hash-cache      (id* '*-rope-hash-cache)
  #:with *-leaf-poly-hash       (id* '*-leaf-poly-hash)
  #:with *-rope-poly-hash       (id* '*-rope-poly-hash)
  #:with *-rope-content=?       (id* '*-rope-content=?)
  #:with *-rope-leaf            (id* '*-rope-leaf)
  #:with *-rope-leaf?           (id* '*-rope-leaf?)
  #:with *-rope-node            (id* '*-rope-node)
  #:with *-rope-node?           (id* '*-rope-node?)
  #:with *-rope?                (id* '*-rope?)
  #:with make-*-rope-leaf       (id* 'make-*-rope-leaf)
  #:with make-*-rope-node       (id* 'make-*-rope-node)
  #:with make-empty-*-rope      (id* 'make-empty-*-rope)
  #:with *-rope-concat          (id* '*-rope-concat)
  #:with *-rope-append          (id* '*-rope-append)
  #:with *-rope-append2         (id* '*-rope-append2)
  #:with *-rope-split           (id* '*-rope-split)
  #:with *-rope-ref             (id* '*-rope-ref)
  #:with *-rope-offset-index    (id* '*-rope-offset-index)
  #:with *-rope-splice          (id* '*-rope-splice)
  #:with *-rope-slice           (id* '*-rope-slice)
  #:with *->rope                (id* '*->rope)
  #:with rope->*                (id* 'rope->*)
  #:with *-rope-ensure-balance  (id* '*-rope-ensure-balance)
  #:with *-rope-rebalance       (id* '*-rope-rebalance)
  #:with *-rope-defrag          (id* '*-rope-defrag)

  #:with *-rope-forest-add      (id* '*-rope-forest-add)
  #:with *-rope-forest->rope    (id* '*-rope-forest->rope)
  (begin
    (define-syntax rope:type-id
      (rope-type-descriptor #'*-rope-chunk?
                            #'*-rope-elem-size
                            #'*-rope-chunk-limit
                            #'*-rope-chunk-empty
                            #'*-rope-chunk-count
                            #'*-rope-chunk-size
                            #'*-rope-chunk-slice
                            #'*-rope-chunk-append
                            #'*-rope-chunk-ref
                            (~? #'*-rope-chunk-compare #f)
                            ;; The default overlap equality check loops over
                            ;; the elements in a chunk. This is generally
                            ;; faster for strings but slower for bytes.
                            #'*-rope-chunk-overlap=?
                            #'*-rope-leaf #'*-rope-node))
    ;; fundamental operations
    (define (*-rope-chunk?        a)     (chunk?        a))
    (define (*-rope-elem-size     a i)   (elem-size     a i))
    (define (*-rope-chunk-limit)         (chunk-limit))
    (define (*-rope-chunk-empty)         (chunk-empty))
    (define (*-rope-chunk-count   a)     (chunk-count   a))
    (define (*-rope-chunk-size    a)     (chunk-size    a))
    (define (*-rope-chunk-slice   a s e) (chunk-slice   a s e))
    (define (*-rope-chunk-append  as)    (chunk-append  as))
    (define (*-rope-chunk-ref     a k)   (chunk-ref     a k))
    (~? (define (*-rope-chunk-compare a b) (chunk-compare a b)))
    (~? (define (*-rope-chunk-overlap=? ac bc ap bp k)
          (chunk-overlap=? type-id ac bc ap bp k))
        (define (*-rope-chunk-overlap=? ac bc ap bp k)
          (let loop ([i 0])
            (or (= i k)
                (and (equal? (chunk-ref ac (+ ap i))
                             (chunk-ref bc (+ bp i)))
                     (loop (add1 i)))))))

    ;; the composable polynomial hash algorithm --------------------------------

    ;; one cache instance per type
    (define *-rope-hash-cache (make-weak-hasheq))

    (define (*-leaf-poly-hash chunk)
      (for/fold ([h 0] [p 1] #:result (values h p))
                ([i (in-range (*-rope-chunk-count chunk))])
        ;; Bitmasking the native hash ensures e is < 2³¹, preventing bignums
        ;; when computing the polynomial term `(* e p)`
        (define e (bitwise-and (equal-hash-code (*-rope-chunk-ref chunk i)) HASH-MOD))
        (values (modulo (+ h (* e p)) HASH-MOD)
                (modulo (* p HASH-BASE) HASH-MOD))))

    (define (*-rope-poly-hash rope)
      ;; Set hash-ref's failure-result to a value (#f) to prevent allocation
      ;; of a closure every time the cache is checked.
      (or (hash-ref *-rope-hash-cache rope #f)
          (let-values
              ([(h p) (if (*-rope-leaf? rope)
                          (*-leaf-poly-hash (rope-leaf-chunk rope))
                          (let ([hl+pl (*-rope-poly-hash (rope-node-left  rope))]
                                [hr+pr (*-rope-poly-hash (rope-node-right rope))])
                            (let ([hl (car hl+pl)]
                                  [pl (cdr hl+pl)]
                                  [hr (car hr+pr)]
                                  [pr (cdr hr+pr)])
                              (values (modulo (+ hl (* pl hr)) HASH-MOD)
                                      (modulo (* pl pr) HASH-MOD)))))])
            (define result (cons h p))
            (hash-set! *-rope-hash-cache rope result)
            result)))

    ;; -------------------------------------------------------------------------

    ;;  efficient content-base equality check
    (define (*-rope-content=? a b)
      ;; Strict left-to-right depth-first traversal. Returns the next chunk
      ;; and the remaining traversal stack. Amortized O(1) time complexity per
      ;; leaf.
      (define (next-chunk stack)
        (let loop ([st stack])
          (cond
            [(null? st)
             (values #f 0 null)]
            ;; For a leaf, return its chunk and the remaining stack.
            [(*-rope-leaf? (car st))
             (values (rope-leaf-chunk (car st)) 0 (cdr st))]
            ;; For a node, push right branch and descend into left branch
            [(*-rope-node? (car st))
             (loop (list* (rope-node-left  (car st))
                          (rope-node-right (car st))
                          (cdr st)))])))
      (or (eq? a b)
          (and (= (rope-count a) (rope-count b))
               ;; cache hit: O(1) comparison of associative hashes
               (equal? (*-rope-poly-hash a) (*-rope-poly-hash b))
               ;; cache miss: perform a (mostly-)allocation-free traversal
               (let walk ([a-chunk #f] [a-pos 0] [a-stack (list a)]
                                       [b-chunk #f] [b-pos 0] [b-stack (list b)])
                 ;; Advance to the next chunk if the current one is finished.
                 ;; Automatically skips empty leaves in O(1) time.
                 (let-values ([(a-chunk a-pos a-stack)
                               (if (or (not a-chunk) (>= a-pos (*-rope-chunk-count a-chunk)))
                                   (next-chunk a-stack)
                                   (values a-chunk a-pos a-stack))]
                              [(b-chunk b-pos b-stack)
                               (if (or (not b-chunk) (>= b-pos (*-rope-chunk-count b-chunk)))
                                   (next-chunk b-stack)
                                   (values b-chunk b-pos b-stack))])
                   (cond
                     ;; match: both trees finish simultaneously
                     [(and (not a-chunk) (not b-chunk)) #t]
                     ;; msimatch: one tree finishes early
                     [(or (not a-chunk) (not b-chunk)) #f]
                     [else
                      (define na (- (*-rope-chunk-count a-chunk) a-pos))
                      (define nb (- (*-rope-chunk-count b-chunk) b-pos))
                      (define k  (min na nb))
                      (and (*-rope-chunk-overlap=? a-chunk b-chunk a-pos b-pos k)
                           (walk a-chunk (+ a-pos k) a-stack
                                 b-chunk (+ b-pos k) b-stack))]))))))

    ;; specialized tree structs
    (struct *-rope-leaf rope-leaf ()
      #:transparent
      #:methods gen:equal+hash
      [(define (equal-proc a b _) (*-rope-content=? a b))
       (define (hash-proc  a _)   (car (*-rope-poly-hash a)))
       (define (hash2-proc a _)   (cdr (*-rope-poly-hash a)))])

    (struct *-rope-node rope-node ()
      #:transparent
      #:methods gen:equal+hash
      [(define (equal-proc a b _) (*-rope-content=? a b))
       (define (hash-proc  a _)   (car (*-rope-poly-hash a)))
       (define (hash2-proc a _)   (cdr (*-rope-poly-hash a)))])

    (define (*-rope? a) (or (*-rope-leaf? a) (*-rope-node? a)))

    ;; smart constructors
    (define (make-*-rope-leaf chunk) (make-rope-leaf  type-id chunk))
    (define (make-*-rope-node l r)   (make-rope-node  type-id l r))
    (define (make-empty-*-rope)      (make-empty-rope type-id))

    (define (*-rope-concat         l r)      (rope-concat         type-id l r))
    (define (*-rope-append       . as)       (rope-append         type-id as))
    (define (*-rope-append2        l r)      (rope-append2        type-id l r))
    (define (*-rope-split          a i)      (rope-split          type-id a i))
    (define (*-rope-ref            a i)      (rope-ref            type-id a i))
    (define (*-rope-offset-index   a p)      (rope-offset-index   type-id a p))
    (define (*-rope-cut            a i k)    (rope-cut            type-id a i k))
    (define (*-rope-slice          a i k)    (rope-slice          type-id a i k))
    (define (*-rope-splice         a i k es) (rope-splice         type-id a i k es))
    (define (*->rope               c)        (chunk->rope         type-id c))
    (define (rope->*               a)        (rope->chunk         type-id a))
    (define (*-rope-ensure-balance a)        (rope-ensure-balance type-id a))
    (define (*-rope-rebalance      a)        (rope-rebalance      type-id a))
    (define (*-rope-defrag         a)        (rope-defrag         type-id a))

    (define (*-rope-forest-add     f a)      (rope-forest-add     type-id f a))
    (define (*-rope-forest->rope   f)        (rope-forest->rope   type-id f))
    ))
