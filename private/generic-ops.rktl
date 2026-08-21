;; rope/private/generic-ops.rktl

(define-syntax-parse-rule (define-rope-operation (gen-id . args) body ...)
  #:with ρ                (format-id this-syntax "ρ")
  #:with chunk?           (format-id this-syntax "chunk?")
  #:with elem-size        (format-id this-syntax "elem-size")
  #:with chunk-limit      (format-id this-syntax "chunk-limit")
  #:with chunk-empty      (format-id this-syntax "chunk-empty")
  #:with chunk-count      (format-id this-syntax "chunk-count")
  #:with chunk-size       (format-id this-syntax "chunk-size")
  #:with chunk-slice      (format-id this-syntax "chunk-slice")
  #:with chunk-append     (format-id this-syntax "chunk-append")
  #:with chunk-ref        (format-id this-syntax "chunk-ref")
  #:with chunk-compare    (format-id this-syntax "chunk-compare")
  #:with chunk-overlap=?  (format-id this-syntax "chunk-overlap=?")
  #:with leaf-constructor (format-id this-syntax "leaf-constructor")
  #:with node-constructor (format-id this-syntax "node-constructor")
  (define-syntax-parse-rule (gen-id ρ . args)
    #:do [(unless (identifier? (attribute gen-id))
            (raise-syntax-error 'define-rope-operation "expected an identifier"
                                this-syntax (attribute gen-id)))
          ;; error reporting helper
          (define (raise-gen-error msg stx)
            (raise-syntax-error 'gen-id msg this-syntax stx))
          ;; format the descriptor's identifier
          (define ρ-stx (format-id (attribute ρ) "rope:~a" (attribute ρ)))
          (unless (identifier? ρ-stx)
            (raise-gen-error "expected an identifier" ρ-stx))
          ;; get the descriptor's value
          (define desc
            (syntax-local-value
             ρ-stx (λ () (raise-gen-error "expected a rope type descriptor" ρ-stx))))
          (unless (rope-type-descriptor? desc)
            (raise-gen-error "expected a rope type descriptor" ρ-stx))]
    #:with chunk?        (rope-type-descriptor-chunk?       desc)
    #:with elem-size     (rope-type-descriptor-elem-size    desc)
    #:with chunk-limit   (rope-type-descriptor-chunk-limit  desc)
    #:with chunk-empty   (rope-type-descriptor-chunk-empty  desc)
    #:with chunk-count   (rope-type-descriptor-chunk-count  desc)
    #:with chunk-size    (rope-type-descriptor-chunk-size   desc)
    #:with chunk-slice   (rope-type-descriptor-chunk-slice  desc)
    #:with chunk-append  (rope-type-descriptor-chunk-append desc)
    #:with chunk-ref     (rope-type-descriptor-chunk-ref    desc)
    #:with chunk-compare
    (let ([stx (rope-type-descriptor-chunk-compare desc)])
      (or stx #'(error 'chunk-compare "operation not defined for ~a-rope" 'ρ)))
    #:with chunk-overlap=?
    (let ([stx (rope-type-descriptor-chunk-overlap=? desc)])
      (or stx
          #'(λ (ac bc ap bp k)
              (let loop ([i 0])
                (or (= i k)
                    (and (equal? (chunk-ref ac (^+ ap i))
                                 (chunk-ref bc (^+ bp i)))
                         (loop (^add1 i))))))))
    #:with leaf-constructor (rope-type-descriptor-leaf-constructor desc)
    #:with node-constructor (rope-type-descriptor-node-constructor desc)
    (begin body ...)))

(define-rope-operation (rope-chunk?           a)             (chunk?          a))
(define-rope-operation (rope-chunk-limit)                    (chunk-limit))
(define-rope-operation (rope-chunk-empty)                    (chunk-empty))
(define-rope-operation (rope-chunk-count      a)             (chunk-count     a))
(define-rope-operation (rope-chunk-size       a)             (chunk-size      a))
(define-rope-operation (rope-chunk-slice      a i k)         (chunk-slice     a i k))
(define-rope-operation (rope-chunk-append     as)            (chunk-append    as))
(define-rope-operation (rope-chunk-ref        a k)           (chunk-ref       a k))
(define-rope-operation (rope-chunk-compare    a b)           (chunk-compare   a b))
(define-rope-operation (rope-chunk-overlap=?  ac bc ap bp k) (chunk-overlap=? ac bc ap bp k))

(define-rope-operation (rope-elem-size a i)
  (if (number? elem-size) elem-size (elem-size a i)))

(define-rope-operation (make-rope-leaf chunk)
  (leaf-constructor (chunk-count chunk) (chunk-size chunk) chunk))

(define-rope-operation (make-rope-node l r)
  (node-constructor (^+ (rope-count l) (rope-count r))
                    (^+ (rope-size l) (rope-size r))
                    (^add1 (^max (rope-depth l) (rope-depth r)))
                    l r))

(define-rope-operation (make-empty-rope) (make-rope-leaf ρ (chunk-empty)))

;; Naive concatenation. O(1)
(define-rope-operation (rope-concat l r) (make-rope-node ρ l r))

;; Concatenation with rebalancing. O(log n) amortized.
(define-rope-operation (rope-append2 l r)
  (cond
    [(= 0 (rope-count l)) r]
    [(= 0 (rope-count r)) l]
    [else
     (define combined (rope-concat ρ l r))
     (if (rope-balanced? combined) combined (rope-rebalance ρ combined))]))

;; O(log n) amortized
(define-rope-operation (rope-append as)
  (let ([b (for/fold ([l (make-empty-rope ρ)]) ([r (in-list as)])
             (rope-concat ρ l r))])
    (if (rope-balanced? b) b (rope-rebalance ρ b))))

;; Splits at an element index, returning the two halves [0, i) and [i, n).
;; O(log n) amortized
(define-rope-operation (rope-split a0 i0)
  (let-values
      ([(l r)
        (let loop ([a a0] [i i0])
          (cond
            [(rope-leaf? a)
             (define chunk (rope-leaf-chunk a))
             (values (make-rope-leaf ρ (chunk-slice chunk 0 i))
                     (make-rope-leaf ρ (chunk-slice chunk i (^- (rope-leaf-count a) i))))]
            [else
             (define l (rope-node-left a))
             (define r (rope-node-right a))
             (define n (rope-count l))
             (cond
               [(^<= i n)
                (define-values (ll lr) (loop l i))
                (values ll (rope-concat ρ lr r))]
               [else
                (define-values (rl rr) (loop r (^- i n)))
                (values (rope-concat ρ l rl) rr)])]))])
    (values (if (rope-balanced? l) l (rope-rebalance ρ l))
            (if (rope-balanced? r) r (rope-rebalance ρ r)))))

;;; O(log n)
(define-rope-operation (rope-ref a0 i0)
  (let loop ([a a0] [i i0])
    (cond
      [(rope-leaf? a) (chunk-ref (rope-leaf-chunk a) i)]
      [(rope-node? a)
       (define n (rope-count (rope-node-left a)))
       (if (^< i n)
           (loop (rope-node-left a) i)
           (loop (rope-node-right a) (^- i n)))])))

;; Finds the left-most element index containing offset p0, clamped to the end
;; of the rope. O(1) if elem-size is a numeric literal, otherwise O(log n)
(define-rope-operation (rope-offset-index a0 p0)
  (if (number? elem-size)
      (^min (^quotient p0 elem-size) (^sub1 (rope-count a0)))
      (let loop ([a a0] [p p0])
        (if (rope-leaf? a)
            (let chunk-loop ([p p] [i 0])
              (let ([k (elem-size (rope-leaf-chunk a) i)])
                (if (^<= p k) i (chunk-loop (^- p k) (^add1 i)))))
            (let ([l (rope-node-left a)])
              (if (^< p (rope-size l))
                  (loop l p)
                  (^+ (rope-count l) (loop (rope-node-right a) (^- p (rope-size l))))))))))

;; Efficient dual-split variant that throws away the interval [i, i + k).
;; Delays the actual splits until it finds the sub-tree(s) containing the
;; endpoints of the interval, limiting the number of rebalances to three.
;; O(log n) amortized
(define-rope-operation (rope-cut a0 i0 k0)
  (let-values
      ([(l r)
        (let loop ([a a0] [i i0] [j (^+ i0 k0)])
          (cond
            [(rope-leaf? a)
             (define chunk (rope-leaf-chunk a))
             (values (make-rope-leaf ρ (chunk-slice chunk 0 i))
                     (make-rope-leaf ρ (chunk-slice chunk j (rope-leaf-count a))))]
            [else
             (define l (rope-node-left a))
             (define r (rope-node-right a))
             (define n (rope-count l))
             (cond
               ;; (end of) interval must be in the left sub-tree
               [(^<= j n)
                (define-values (ll lr) (loop l i j))
                (values ll (rope-concat ρ lr r))]
               ;; (start of) interval must be in the right sub-tree
               [(^>= i n)
                (define-values (rl rr) (loop r (^- i n) (^- j n)))
                (values (rope-concat ρ l rl) rr)]
               ;; interval touches both sub-trees
               [else
                (define-values (ll _lr) (rope-split ρ l i))
                (define-values (_rl rr) (rope-split ρ r (^- j n)))
                (values ll rr)])]))])
    (values (if (rope-balanced? l) l (rope-rebalance ρ l))
            (if (rope-balanced? r) r (rope-rebalance ρ r)))))

;; The complement of rope-cut. Keeps only the interval [i, i + k). O(log n)
;; amortized
(define-rope-operation (rope-slice a0 i0 k0)
  (let ([b (let loop ([a a0] [i i0] [j (^+ i0 k0)])
             (cond
               [(rope-leaf? a)
                (make-rope-leaf ρ (chunk-slice (rope-leaf-chunk a) i (^- j i)))]
               [else
                (define l (rope-node-left a))
                (define r (rope-node-right a))
                (define n (rope-count l))
                (cond
                  [(^<= j n) (loop l i j)]
                  [(^>= i n) (loop r (^- i n) (^- j n))]
                  [else
                   (define-values (_ll lr) (rope-split ρ l i))
                   (define-values (rl _rr) (rope-split ρ r (^- j n)))
                   (rope-concat ρ lr rl)])]))])
    (if (rope-balanced? b) b (rope-rebalance ρ b))))

;; Replaces the interval [i, i + k) with chunk. O(log n) amortized
(define-rope-operation (rope-splice a i k chunk)
  (let ([b (let-values ([(l r) (rope-cut ρ a i k)])
             (rope-concat ρ (rope-concat ρ l (chunk->rope ρ chunk)) r))])
    (if (rope-balanced? b) b (rope-rebalance ρ b))))

;; Produces an optimally balanced rope.
(define-rope-operation (chunk->rope chunk0)
  (let loop ([chunk chunk0])
    (define n (chunk-count chunk))
    (if (^<= n (chunk-limit))
        (make-rope-leaf ρ chunk)
        (let ([mid (^quotient n 2)])
          (define l (loop (chunk-slice chunk 0 mid)))
          (define r (loop (chunk-slice chunk mid (^- n mid))))
          (rope-concat ρ l r)))))

(define-rope-operation (rope->chunk a) (chunk-append (rope-chunks a)))

(define-rope-operation (rope-defrag a)
  (chunk->rope ρ (rope->chunk ρ a)))

(define-rope-operation (rope-rebalance a0)
  (let ()
    (define (insert slots a)
      (let loop ([i 0] [carry a] [current-slots slots])
        (define slot-i (hash-ref current-slots i #f))
        ;; If the current slot is occupied, it represents elements strictly to
        ;; the left of `carry`. Merge them to maintain chunk order before
        ;; evaluating Fibonacci bounds.
        (define next-carry (if slot-i (rope-concat ρ slot-i carry) carry))
        (define next-slots (if slot-i (hash-remove current-slots i) current-slots))

        ;; We only place the merged chunk if the slot was initially empty AND
        ;; the chunk's length is small enough for this slot's Fibonacci bound.
        (if (and (not slot-i) (< (rope-count next-carry) (fib-bound (+ i 3))))
            (hash-set next-slots i next-carry)
            (loop (add1 i) next-carry next-slots))))

    (define (traverse a slots)
      (if (rope-balanced? a)
          (insert slots a)
          (traverse (rope-node-right a) (traverse (rope-node-left a) slots))))

    (define (collapse slots)
      ;; Collapse from smallest index to largest to maintain depth bounds.
      (for/fold ([result #f]) ([i (in-range max-fib-index)])
        (define slot-i (hash-ref slots i #f))
        (cond
          [(not slot-i) result]
          [(not result) slot-i]
          [else (rope-concat ρ slot-i result)])))

    (collapse (traverse a0 (hasheqv)))))
