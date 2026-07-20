#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     syntax/parse)
         racket/contract
         racket/match
         racket/splicing
         rope/rope
         syntax/parse/define)

(provide define-rope-type rope-type-out rope-type-out/contract)

;; `define-rope-type`, `rope-type-out`, and `rope-type-out/contract` all call this with the same
;; `type` identifier, so the names they produce are `free-identifier=?` to one another.
(begin-for-syntax
  (define (rope-type-ids type-stx)
    (define (mk  fmt) (format-id type-stx fmt (syntax-e type-stx)))
    (define (mk2 fmt) (format-id type-stx fmt (syntax-e type-stx) (syntax-e type-stx)))
    (hasheq
     'rope-gen          (mk  "~a-rope-gen")
     'rope-leaf         (mk  "~a-rope-leaf")
     'rope-node         (mk  "~a-rope-node")
     'rope-leaf?        (mk  "~a-rope-leaf?")
     'rope-node?        (mk  "~a-rope-node?")
     'rope?             (mk  "~a-rope?")
     'raw?              (mk  "~a-raw?")
     'raw-limit         (mk  "~a-raw-limit")
     'raw-empty         (mk  "~a-raw-empty")
     'raw-count         (mk  "~a-raw-count")
     'raw-width         (mk  "~a-raw-width")
     'raw-slice         (mk  "~a-raw-slice")
     'raw-append        (mk  "~a-raw-append")
     'raw-ref           (mk  "~a-raw-ref")
     'rope-leaf-ctor    (mk  "~a-rope-leaf-ctor")
     'rope-node-ctor    (mk  "~a-rope-node-ctor")
     'make-rope-leaf    (mk  "make-~a-rope-leaf")
     'make-empty-rope   (mk  "make-empty-~a-rope")
     'rope-append1      (mk  "~a-rope-append1")
     'rope-append       (mk  "~a-rope-append")
     'rope-split        (mk  "~a-rope-split")
     'rope-offset-index (mk  "~a-rope-offset-index")
     'rope-splice       (mk  "~a-rope-splice")
     'rope-slice        (mk  "~a-rope-slice")
     'raw->rope         (mk2 "~a-raw->~a-rope")
     'rope->raw         (mk2 "~a-rope->~a-raw")
     'rope-compare      (mk  "~a-rope-compare")
     'rope-compare-with (mk  "~a-rope-compare-with")
     'rope<?            (mk  "~a-rope<?")
     'rope>?            (mk  "~a-rope>?")
     'rope<=?           (mk  "~a-rope<=?")
     'rope>=?           (mk  "~a-rope>=?")
     'cursor-at-end?    (mk  "~a-cursor-at-end?")
     'cursor-peek       (mk  "~a-cursor-peek")
     'cursor-advance    (mk  "~a-cursor-advance")
     'cursor-drop       (mk  "~a-cursor-drop")
     'cursor-take       (mk  "~a-cursor-take")
     'rope->cursor      (mk  "~a-rope->cursor")
     'cursor->rope      (mk  "cursor->~a-rope")
     'rope-foldl        (mk  "~a-rope-foldl")
     'rope-foldr        (mk  "~a-rope-foldr")
     'in-rope-runtime   (mk  "in-~a-rope-runtime")
     'in-rope           (mk  "in-~a-rope")))

  (define public-key-order
    '(rope-leaf
      rope-node rope-leaf? rope-node? rope?
      raw? raw-limit raw-empty raw-count raw-width raw-slice raw-append raw-ref
      make-rope-leaf make-empty-rope
      rope-append1 rope-append rope-split rope-offset-index rope-splice rope-slice
      raw->rope rope->raw
      rope-compare rope-compare-with rope<? rope>? rope<=? rope>=?
      cursor-at-end? cursor-peek cursor-advance cursor-drop cursor-take
      rope->cursor cursor->rope
      rope-foldl rope-foldr
      in-rope)))

(define-simple-macro (define-rope-type type:id
                       raw?-expr:expr
                       raw-limit-expr:expr
                       raw-empty-expr:expr
                       raw-count-expr:expr
                       raw-width-expr:expr
                       raw-slice-expr:expr
                       raw-append-expr:expr
                       raw-ref-expr:expr
                       (~optional (~seq #:compare raw-compare-expr:expr)))
  #:do [(define ids (rope-type-ids (attribute type)))
        (define (id* key) (hash-ref ids key))]
  #:with *-rope-gen          (id* 'rope-gen)
  #:with *-rope-leaf         (id* 'rope-leaf)
  #:with *-rope-node         (id* 'rope-node)
  #:with *-rope-leaf?        (id* 'rope-leaf?)
  #:with *-rope-node?        (id* 'rope-node?)
  #:with *-rope?             (id* 'rope?)
  #:with *-raw?              (id* 'raw?)
  #:with *-raw-limit         (id* 'raw-limit)
  #:with *-raw-empty         (id* 'raw-empty)
  #:with *-raw-count         (id* 'raw-count)
  #:with *-raw-width         (id* 'raw-width)
  #:with *-raw-slice         (id* 'raw-slice)
  #:with *-raw-append        (id* 'raw-append)
  #:with *-raw-ref           (id* 'raw-ref)
  #:with *-rope-leaf-ctor    (id* 'rope-leaf-ctor)
  #:with *-rope-node-ctor    (id* 'rope-node-ctor)
  #:with make-*-rope-leaf    (id* 'make-rope-leaf)
  #:with make-empty-*-rope   (id* 'make-empty-rope)
  #:with *-rope-append1      (id* 'rope-append1)
  #:with *-rope-append       (id* 'rope-append)
  #:with *-rope-split        (id* 'rope-split)
  #:with *-rope-offset-index (id* 'rope-offset-index)
  #:with *-rope-splice       (id* 'rope-splice)
  #:with *-rope-slice        (id* 'rope-slice)
  #:with *-raw->*-rope       (id* 'raw->rope)
  #:with *-rope->*-raw       (id* 'rope->raw)
  #:with *-rope-compare-with (id* 'rope-compare-with)
  #:with *-rope-compare      (id* 'rope-compare)
  #:with *-rope<?            (id* 'rope<?)
  #:with *-rope>?            (id* 'rope>?)
  #:with *-rope<=?           (id* 'rope<=?)
  #:with *-rope>=?           (id* 'rope>=?)
  #:with *-cursor-at-end?    (id* 'cursor-at-end?)
  #:with *-cursor-peek       (id* 'cursor-peek)
  #:with *-cursor-advance    (id* 'cursor-advance)
  #:with *-cursor-drop       (id* 'cursor-drop)
  #:with *-cursor-take       (id* 'cursor-take)
  #:with *-rope->cursor      (id* 'rope->cursor)
  #:with cursor->*-rope      (id* 'cursor->rope)
  #:with *-rope-foldl        (id* 'rope-foldl)
  #:with *-rope-foldr        (id* 'rope-foldr)
  #:with in-*-rope-runtime   (id* 'in-rope-runtime)
  #:with in-*-rope           (id* 'in-rope)
  (begin
    (struct *-rope-gen rope () #:transparent
      #:methods gen:ropeable
      [(define (raw?       _ obj)   (raw?-expr obj))
       (define (raw-limit  _)       (raw-limit-expr))
       (define (raw-empty  _)       (raw-empty-expr))
       (define (raw-count  _ r)     (raw-count-expr r))
       (define (raw-width  _ r)     (raw-width-expr r))
       (define (raw-slice  _ r p l) (raw-slice-expr r p l))
       (define (raw-append _ . rs)  (raw-append-expr rs))
       (define (raw-ref    _ r p)   (raw-ref-expr r p))
       (define (rope-leaf-ctor _)   *-rope-leaf)
       (define (rope-node-ctor _)   *-rope-node)
       (~? (define (raw-compare _ a b) (raw-compare-expr a b)))])

    ;; Using an independent ⟨base, mod⟩ pair for the secondary hash (rather
    ;; than reusing pow) reduces collisions in nested hash tables.
    (struct *-rope-leaf rope-leaf ()
      #:methods gen:rope-equatable
      [(define rope=?    (λ (a b) (*-rope-content=? a b)))
       (define rope-hash (λ (a)   (rope-poly-hash a)))])

    (struct *-rope-node rope-node ()
      #:methods gen:rope-equatable
      [(define rope=?    (λ (a b) (*-rope-content=? a b)))
       (define rope-hash (λ (a)   (rope-poly-hash a)))])

    (define (*-rope? obj) (or (*-rope-leaf? obj) (*-rope-node? obj)))

    (splicing-let ([gen (*-rope-gen)])
      (define (*-raw? obj)                       (raw?              gen obj))
      (define (*-raw-limit)                      (raw-limit         gen))
      (define (*-raw-empty)                      (raw-empty         gen))
      (define (*-raw-count         raw)          (raw-count         gen raw))
      (define (*-raw-width         raw)          (raw-width         gen raw))
      (define (*-raw-slice         raw pos end)  (raw-slice         gen raw pos end))
      (define (*-raw-ref           raw pos)      (raw-ref           gen raw pos))
      (define (*-raw-append    .   raws)         (apply raw-append  gen raws))
      (define (*-rope-leaf-ctor)                 (rope-leaf-ctor    gen))
      (define (*-rope-node-ctor)                 (rope-node-ctor    gen))
      (define (make-*-rope-leaf    raw)          (make-rope-leaf    gen raw))
      (define (make-empty-*-rope)                (make-empty-rope   gen))
      (define (*-rope-append1      left right)   (rope-append1      gen left right))
      (define (*-rope-append   .   ropes)        (apply rope-append gen ropes))
      (define (*-rope-split        rope i)       (rope-split        gen rope i))
      (define (*-rope-offset-index rope pos)     (rope-offset-index gen rope pos))
      (define (*-rope-splice       rope s ol nt) (rope-splice       gen rope s ol nt))
      (define (*-rope-slice        rope s l)     (rope-slice        gen rope s l))
      (define (*-raw->*-rope       raw)          (raw->rope         gen raw))
      (define (*-rope->*-raw       rope)         (rope->raw         gen rope))
      (define (*-cursor-at-end?    cur)          (cursor-at-end?    gen cur))
      (define (*-cursor-peek       cur)          (cursor-peek       gen cur))
      (define (*-cursor-advance    cur)          (cursor-advance    gen cur))
      (define (*-cursor-drop       cur k)        (cursor-drop       gen cur k))
      (define (*-cursor-take       cur k)        (cursor-take       gen cur k))
      (define (*-rope->cursor      rope)         (rope->cursor      gen rope))
      (define (cursor->*-rope      cur)          (cursor->rope      gen cur))
      (define (*-rope-foldl proc init rope0 . ropes) (apply rope-foldl gen proc init rope0 ropes))
      (define (*-rope-foldr proc init rope0 . ropes) (apply rope-foldr gen proc init rope0 ropes))
      (define (*-rope-compare-with cmp a b) (rope-compare-with gen cmp a b))
      (define (*-rope-compare a b) (rope-compare gen a b))
      (define (*-rope<?       a b) (rope<?       gen a b))
      (define (*-rope>?       a b) (rope>?       gen a b))
      (define (*-rope<=?      a b) (rope<=?      gen a b))
      (define (*-rope>=?      a b) (rope>=?      gen a b))

      ;; A Composable Polynomial Hash
      ;;
      ;; We define a polynomial rolling hash (Rabin–Karp Style) that is
      ;; algebraically associative under concatenation, so H(A ++ B) depends
      ;; only on H(A), H(B), and width(A), and never on how the tree groups A
      ;; and B. Combined with the an eq?-keyed memo table, this becomes O(log n)
      ;; amortized after small edits:
      ;;
      ;; For a raw run r₀ r₁ … r_{k−1}, define
      ;;
      ;;   H(run) ≡ Σᵢ hash(rᵢ) · Pⁱ   (mod M)
      ;;
      ;; with M a Mersenne prime (fixnum-friendly on 64-bit CS) and P a fixed
      ;; base coprime to M. The identity:
      ;;
      ;;   H(A ++ B) ≡ H(A) + P^{|A|} · H(B)   (mod M)
      ;;
      ;; is exact and independent of how A ++ B is further subdivided, so
      ;; caching ⟨H(subtree), P^{count(subtree)} mod M⟩ per node makes any
      ;; parent combination O(1).

      (define HASH-BASE (sub1 (expt 2 31))) ; odd, < M
      (define HASH-MOD  (sub1 (expt 2 61))) ; Mersenne prime 2^61 - 1

      (define hash-cache (make-weak-hasheq))

      (define (leaf-poly-hash raw)
        (for/fold ([h 0] [p 1] #:result (cons h p))
                  ([i (in-range (*-raw-count raw))])
          (define e (equal-hash-code (*-raw-ref raw i)))
          (values (modulo (+ h (* e p)) HASH-MOD)
                  (modulo (* p HASH-BASE) HASH-MOD))))

      (define (rope-poly-hash rope)
        (hash-ref!
         hash-cache rope
         (λ ()
           (match rope
             [(*-rope-leaf _ _ raw) (leaf-poly-hash raw)]
             [(*-rope-node _ _ l r)
              (match-define (cons hl pl) (rope-poly-hash l))
              (match-define (cons hr pr) (rope-poly-hash r))
              (cons (modulo (+ hl (* pl hr)) HASH-MOD)
                    (modulo (* pl pr)        HASH-MOD))]))))

      ;; O(1) reject on count mismatch, otherwise walk both ropes' raw runs in
      ;; lockstep, comparing only the overlapping prefix of whatever chunk
      ;; each cursor is currently in, so leaf boundaries never need to align.
      (define (*-rope-content=? a b)
        (or (eq? a b)
            (and (= (rope-count a) (rope-count b))
                 (let loop ([ca (*-rope->cursor a)] [cb (*-rope->cursor b)])
                   (cond
                     [(and (*-cursor-at-end? ca) (*-cursor-at-end? cb)) #t]
                     [else
                      (match-define (cursor ra pa afa) ca)
                      (match-define (cursor rb pb afb) cb)
                      (define na (- (*-raw-count ra) pa))
                      (define nb (- (*-raw-count rb) pb))
                      (define k  (min na nb))
                      (and (for/and ([i (in-range k)])
                             (equal? (*-raw-ref ra (+ pa i)) (*-raw-ref rb (+ pb i))))
                           (loop (if (= k na) (*-rope->cursor afa) (cursor ra (+ pa k) afa))
                                 (if (= k nb) (*-rope->cursor afb) (cursor rb (+ pb k) afb))))])))))

      ;; Evaluated when `in-*-rope` is used as a first-class value outside of a `for` loop (e.g.,
      ;; passed to standard higher-order functions like `sequence-map`).
      (define (in-*-rope-runtime rope)
        (make-do-sequence
         (λ ()
           (values *-cursor-peek
                   *-cursor-advance
                   (*-rope->cursor rope)
                   (λ (cur) (not (*-cursor-at-end? cur)))
                   #f
                   #f))))

      ;; Evaluated when `in-*-rope` is used directly in a `for` comprehension clause. Expands into a
      ;; specialized `:do-in` form that Racket optimizes heavily.
      (define-sequence-syntax in-*-rope
        (λ () #'in-*-rope-runtime)
        (λ (stx)
          (syntax-parse stx
            [[(id:id) (_ rope-expr:expr)]
             #'[(id)
                (:do-in
                 ([(rope) rope-expr])
                 (begin)
                 ([cur (*-rope->cursor rope)])
                 (not (*-cursor-at-end? cur))
                 ([(id) (*-cursor-peek cur)])
                 #t
                 #t
                 ((*-cursor-advance cur)))]]))))))

(define-syntax rope-type-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ type:id)
        #:do [(define ids (rope-type-ids (attribute type)))]
        #:with (pub ...) (map (λ (k) (hash-ref ids k)) public-key-order)
        #'(combine-out pub ...)]))))

;; The macro only knows the shape of a rope type generically: `~a-raw?` classifies raw payloads and
;; `~a-rope?` classifies ropes, so those predicates are the honest default contracts for anything
;; raw-shaped or rope-shaped. It cannot know, e.g., that a string-rope raw is specifically `string?`
;; rather than merely `string-raw?`, or that a raw-ref on it returns `char?` rather than an opaque
;; element — that concrete knowledge belongs to the instantiating module. `#:raw` and `#:element`
;; let the caller tighten those two contracts.
;;
;; Struct field names/types (count, width, raw / count, width, left, right) are taken from the
;; observed shape of `rope-leaf` / `rope-node` in `rope/rope` — if that base layout ever changes,
;; this clause must change with it.
(define-syntax rope-type-out/contract
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ type:id
           (~optional (~seq #:raw     raw-ctc:expr))
           (~optional (~seq #:element elem-ctc:expr) #:defaults ([elem-ctc #'any/c])))
        #:do [(define ids (rope-type-ids #'type))
              (define (id* key) (hash-ref ids key))]
        #:with *rope-leaf         (id* 'rope-leaf)
        #:with *rope-node         (id* 'rope-node)
        #:with *rope-leaf?        (id* 'rope-leaf?)
        #:with *rope-node?        (id* 'rope-node?)
        #:with *rope?             (id* 'rope?)
        #:with *raw?              (id* 'raw?)
        #:with *raw-limit         (id* 'raw-limit)
        #:with *raw-empty         (id* 'raw-empty)
        #:with *raw-count         (id* 'raw-count)
        #:with *raw-width         (id* 'raw-width)
        #:with *raw-slice         (id* 'raw-slice)
        #:with *raw-append        (id* 'raw-append)
        #:with *raw-ref           (id* 'raw-ref)
        #:with make-*rope-leaf    (id* 'make-rope-leaf)
        #:with make-empty-*rope   (id* 'make-empty-rope)
        #:with *rope-append1      (id* 'rope-append1)
        #:with *rope-append       (id* 'rope-append)
        #:with *rope-split        (id* 'rope-split)
        #:with *rope-offset-index (id* 'rope-offset-index)
        #:with *rope-splice       (id* 'rope-splice)
        #:with *rope-slice        (id* 'rope-slice)
        #:with *raw->*rope        (id* 'raw->rope)
        #:with *rope->*raw        (id* 'rope->raw)
        #:with *rope-compare      (id* 'rope-compare)
        #:with *rope-compare-with (id* 'rope-compare-with)
        #:with *rope<?            (id* 'rope<?)
        #:with *rope>?            (id* 'rope>?)
        #:with *rope<=?           (id* 'rope<=?)
        #:with *rope>=?           (id* 'rope>=?)
        #:with *cursor-at-end?    (id* 'cursor-at-end?)
        #:with *cursor-peek       (id* 'cursor-peek)
        #:with *cursor-advance    (id* 'cursor-advance)
        #:with *cursor-drop       (id* 'cursor-drop)
        #:with *cursor-take       (id* 'cursor-take)
        #:with *rope->cursor      (id* 'rope->cursor)
        #:with cursor->*rope      (id* 'cursor->rope)
        #:with *rope-foldl        (id* 'rope-foldl)
        #:with *rope-foldr        (id* 'rope-foldr)
        #:with in-*rope           (id* 'in-rope)
        #:with raw/c              (if (attribute raw-ctc) #'raw-ctc #'*raw?)
        (pre-expand-export
         #'(combine-out
            (contract-out
             (struct *rope-leaf ([count exact-nonnegative-integer?]
                                 [width exact-nonnegative-integer?]
                                 [raw   raw/c]))
             (struct *rope-node ([count exact-nonnegative-integer?]
                                 [width exact-nonnegative-integer?]
                                 [left  *rope?]
                                 [right *rope?]))
             [*rope?             (-> any/c boolean?)]
             [*raw?              (-> any/c boolean?)]
             [*raw-limit         (-> exact-nonnegative-integer?)]
             [*raw-empty         (-> raw/c)]
             [*raw-count         (raw/c . -> . exact-nonnegative-integer?)]
             [*raw-width         (raw/c . -> . exact-nonnegative-integer?)]
             [*raw-slice         (raw/c exact-nonnegative-integer?
                                        exact-nonnegative-integer? . -> . raw/c)]
             [*raw-ref           (raw/c exact-nonnegative-integer? . -> . elem-ctc)]
             [*raw-append        (raw/c (... ...) . -> . raw/c)]
             [make-*rope-leaf    (raw/c . -> . *rope-leaf?)]
             [make-empty-*rope   (-> *rope?)]
             [*rope-append1      (*rope? *rope? . -> . *rope?)]
             [*rope-append       (*rope? (... ...) . -> . *rope?)]
             [*rope-split        (*rope? exact-nonnegative-integer? . -> .
                                         (values *rope? *rope?))]
             [*rope-offset-index (*rope? exact-nonnegative-integer? . -> .
                                         exact-nonnegative-integer?)]
             [*rope-splice       (*rope? exact-nonnegative-integer?
                                         exact-nonnegative-integer? raw/c . -> . *rope?)]
             [*rope-slice        (*rope? exact-nonnegative-integer?
                                         exact-nonnegative-integer? . -> . *rope?)]
             [*raw->*rope        (raw/c . -> . *rope?)]
             [*rope->*raw        (*rope? . -> . raw/c)]
             [*rope-compare      (*rope? *rope? . -> . (or/c '< '= '>))]
             [*rope-compare-with (procedure? *rope? *rope? . -> . (or/c '< '= '>))]
             [*rope<?            (*rope? *rope? . -> . boolean?)]
             [*rope>?            (*rope? *rope? . -> . boolean?)]
             [*rope<=?           (*rope? *rope? . -> . boolean?)]
             [*rope>=?           (*rope? *rope? . -> . boolean?)]
             [*cursor-at-end?    (cursor? . -> . boolean?)]
             [*cursor-peek       (cursor? . -> . (or/c #f elem-ctc))]
             [*cursor-advance    (cursor? . -> . cursor?)]
             [*cursor-drop       (cursor? exact-nonnegative-integer? . -> . cursor?)]
             [*cursor-take       (cursor? exact-nonnegative-integer? . -> . *rope?)]
             [*rope->cursor      (*rope? . -> . cursor?)]
             [cursor->*rope      (cursor? . -> . *rope?)]
             [*rope-foldl        (procedure? any/c *rope? *rope? (... ...) . -> . any/c)]
             [*rope-foldr        (procedure? any/c *rope? *rope? (... ...) . -> . any/c)])
            in-*rope)
         modes)]))))
