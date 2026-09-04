#lang racket/base

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           racket/pretty
           racket/string
           racket/vector
           rackunit
           rackunit/text-ui
           rope2/cursor
           rope2/generic-ops
           rope2/rope
           rope2/rope-type
           rope2/string-rope
           syntax/parse/define)

  (define (run-suite! suite)
    (define failed (run-tests suite 'verbose))
    (unless (zero? failed)
      (error 'raco-test "~a check(s) failed" failed)))

  (define-syntax (test-property stx)
    (syntax-parse stx
      [(_ name:expr #:trials trials:expr ([id:id gen-expr:expr] ...) body:expr ...+)
       #`(test-case name
           (for ([iteration (in-range trials)])
             (let* ([id gen-expr] ...)
               (with-check-info
                 (['iteration iteration]
                  ['expression (let* ([str (pretty-format '(let () body ...) #:mode 'display)]
                                      [lines (string-split str "\n")]
                                      [indented (cons (car lines)
                                                      (for/list ([line (in-list (cdr lines))])
                                                        (string-append "  " line)))])
                                 (string-info (string-join indented "\n")))]
                  ['id id] ...)
                 #,(syntax/loc stx (check-true (let () body ...)))))))]))

  (define WEIGHTED-CHUNK-LIMIT 8)

  (define (weighted-chunk-compare a b)
    (let loop ([i 0])
      (cond [(and (= i (vector-length a)) (= i (vector-length b))) '=]
            [(= i (vector-length a))                               '<]
            [(= i (vector-length b))                               '>]
            [(< (vector-ref a i) (vector-ref b i))                 '<]
            [(> (vector-ref a i) (vector-ref b i))                 '>]
            [else (loop (add1 i))])))

  (define-rope-type weighted
    #:chunk?        vector?
    #:chunk-limit   (λ () WEIGHTED-CHUNK-LIMIT)
    #:chunk-empty   #()
    #:chunk-length  vector-length
    #:chunk-ref     vector-ref
    #:chunk-slice   (λ (chunk i k) (vector-copy chunk i (+ i k)))
    #:chunk-append  (λ (chunks) (apply vector-append chunks))
    #:chunk-compare weighted-chunk-compare
    #:elem-width    vector-ref)

  (define (random-weight)           (add1 (random 4)))
  (define (random-weighted-chunk n) (build-vector n (λ (_) (random-weight))))
  (define (weighted->vec a)         (weighted-rope->chunk a))

  (define generic-ops-suite
    (test-suite "generic"
      (test-case "chunk generic operations agree with their direct definitions"
        (check-equal? (rope-chunk-limit weighted) WEIGHTED-CHUNK-LIMIT)
        (check-true   (rope-chunk? weighted (vector 1 2 3)))
        (check-false  (rope-chunk? weighted "not a vector"))
        (check-equal? (rope-chunk-empty weighted) (vector))
        (check-equal? (rope-chunk-length weighted (vector 1 2 3)) 3)
        (check-equal? (rope-chunk-width weighted (vector 1 2 3)) 6)
        (check-equal? (rope-chunk-slice weighted (vector 1 2 3 4) 1 2) (vector 2 3))
        (check-equal? (rope-chunk-append weighted (list (vector 1 2) (vector 3 4)))
                      (vector 1 2 3 4))
        (check-equal? (rope-chunk-ref weighted (vector 10 20 30) 1) 20))

      (test-case "leaf-constructor / node-constructor return usable constructors"
        (let-values ([(h p) (rope-chunk-hash weighted (vector 1))])
          (check-equal? (make-rope-leaf weighted (vector 1))
                        (weighted-rope-leaf 1 1 h p weighted-rope-content=? (vector 1))))
        (let*-values ([(l)   (make-rope-leaf weighted (vector 1 2))]
                      [(r)   (make-rope-leaf weighted (vector 3 4))]
                      [(h p) (rope-node-hash weighted l r)])
          (check-equal? (make-rope-node weighted l r)
                        (weighted-rope-node 4 10 h p weighted-rope-content=? 1 l r))))))

  (define core-ops-suite
    (test-suite "empty rope, leaves, concat"
      (test-case "empty rope: zero length/width, empty content, balanced"
        (define e (make-empty-rope weighted))
        (check-equal? (rope-length e) 0)
        (check-equal? (rope-width  e) 0)
        (check-true   (rope-empty? e))
        (check-equal? (rope->chunk weighted e) (vector)))

      (test-case "leaf length ≠ leaf width for weighted elements"
        (define l (make-rope-leaf weighted (vector 1 2 3 4)))
        (check-true   (rope-leaf? l))
        (check-equal? (rope-length l) 4)
        (check-equal? (rope-width  l) 10))

      (test-case "rope-concat derives length/width correctly"
        (define n (rope-concat weighted
                               (make-rope-leaf weighted (vector 1 2))
                               (make-rope-leaf weighted (vector 3 4 5))))
        (check-true   (rope-node?  n))
        (check-equal? (rope-length n) 5)
        (check-equal? (rope-width  n) 15)
        (check-equal? (rope->chunk weighted n) (vector 1 2 3 4 5)))))

  (define append-suite
    (test-suite "append"
      (test-property "rope-append agrees with vector-append and stays mostly balanced"
          #:trials 300
          ([a (random-weighted-chunk (random 40))]
           [b (random-weighted-chunk (random 40))])
        (define r (rope-append2 weighted
                               (make-rope-leaf weighted a)
                               (make-rope-leaf weighted b)))
        (and (= (rope-length r) (+ (vector-length a) (vector-length b)))
             (= (rope-width  r) (+ (rope-chunk-width weighted a)
                                   (rope-chunk-width weighted b)))
             (equal? (rope->chunk weighted r) (vector-append a b))
             (rope-mostly-balanced? r)))

      (test-case "empty rope is a left unit and a right unit of rope-concat/lazy"
        (define e (make-empty-rope weighted))
        (define r (make-rope-leaf weighted (vector 1 2 3)))
        (check-equal? (rope->chunk weighted (rope-append2 weighted e r)) (rope->chunk weighted r))
        (check-equal? (rope->chunk weighted (rope-append2 weighted r e)) (rope->chunk weighted r)))

      (test-case "rope-append with no ropes returns the empty rope"
        (check-true (rope-empty? (rope-append weighted null))))

      (test-property "rope-append agrees with vector-append"
          #:trials 50
          ([chunks (for/list ([_ (in-range (add1 (random 9)))])
                     (random-weighted-chunk (random 7)))])
        (define r (rope-append weighted (map (λ (c) (make-rope-leaf weighted c)) chunks)))
        (equal? (rope->chunk weighted r) (apply vector-append chunks)))

      (test-property "rope-rebalance handles non-leaf runs, not just leaves"
          #:trials 100
          ([depth (add1 (random 5))]
           [leaf-chunk (random-weighted-chunk (add1 (random 6)))])
        (define deep
          (for/fold ([r (make-rope-leaf weighted (random-weighted-chunk 1))])
                    ([_ (in-range depth)])
            (rope-append2 weighted r (make-rope-leaf weighted (random-weighted-chunk 1)))))
        (define combined (rope-concat weighted deep (make-rope-leaf weighted leaf-chunk)))
        (rope-mostly-balanced? (rope-rebalance weighted combined)))

      (test-case "sequential appends stay Fibonacci-balanced across many steps"
        (for/fold ([r (make-empty-rope weighted)]) ([i (in-range 800)])
          (define chunk  (random-weighted-chunk (add1 (random 6))))
          (define leaf   (make-rope-leaf weighted chunk))
          (define r+leaf (rope-append2 weighted r leaf))
          (with-check-info (['iteration i] ['chunk chunk] ['r+leaf r+leaf])
            (check-true (rope-mostly-balanced? r+leaf)))
          r+leaf))))

  (define split-suite
    (test-suite "split"
      (test-property "all partitions are a total cover"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 200)))])
        (define n (vector-length chunk))
        (define a (chunk->rope weighted chunk))
        (for/and ([i (in-range (add1 n))])
          (define-values (l r) (rope-split weighted a i))
          (and (= (rope-length l) i)
               (= (rope-length r) (- n i))
               (equal? (vector-append (weighted->vec l) (weighted->vec r)) chunk))))

      (test-case "splitting at one end returns an empty rope"
        (define chunk (random-weighted-chunk 30))
        (define a (chunk->rope weighted chunk))
        (define-values (l0 r0) (rope-split weighted a 0))
        (define-values (ln rn) (rope-split weighted a (vector-length chunk)))
        (check-true   (rope-empty? l0))
        (check-equal? (weighted->vec r0) chunk)
        (check-equal? (weighted->vec ln) chunk)
        (check-true   (rope-empty? rn)))))

  (define (vector-splice v i k chunk)
    (vector-append (vector-copy v 0 i) chunk
                   (vector-copy v (+ i k) (vector-length v))))

  (define splice/slice-suite
    (test-suite "splice/slice"
      (test-property "rope-splice matches a vector oracle"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))])
        (define n (vector-length chunk))
        (define i (random (add1 n)))
        (define k (random (add1 (- n i))))
        (define new-chunk (random-weighted-chunk (random 20)))
        (define a (chunk->rope weighted new-chunk))
        (equal? (weighted->vec (rope-splice weighted (chunk->rope weighted chunk) i k a))
                (vector-splice chunk i k new-chunk)))

      (test-property "rope-slice matches a vector oracle"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))])
        (define n (vector-length chunk))
        (define i (random (add1 n)))
        (define k (random (add1 (- n i))))
        (define a (chunk->rope weighted chunk))
        (equal? (weighted->vec (rope-slice weighted a i k))
                (vector-copy chunk i (+ i k))))

      (test-case "rope-slice/rope-splice don't fail at either end"
        (define chunk (random-weighted-chunk 20))
        (define a (chunk->rope weighted chunk))
        (define b (make-empty-rope weighted))
        (check-not-exn (λ () (rope-slice weighted a 0 0)))
        (check-not-exn (λ () (rope-slice weighted a (vector-length chunk) 0)))
        (check-not-exn (λ () (rope-splice weighted a 0 0 b)))
        (check-not-exn (λ () (rope-splice weighted a (vector-length chunk) 0 b))))))

  (define (owning-index chunk p)
    (let loop ([i 0] [acc 0])
      (define w (vector-ref chunk i))
      (if (< p (+ acc w)) i (loop (add1 i) (+ acc w)))))

  (define offset-index-suite
    (test-suite "rope-offset-index"
      (test-property "matches a linear-scan oracle"
          #:trials 200
          ([chunk (random-weighted-chunk (add1 (random 30)))]
           [a (chunk->rope weighted chunk)]
           [p (random (rope-width a))]
           [i (rope-offset-index weighted a p)]
           [j (owning-index chunk p)])
        (= i j))

      (test-case "at both ends, and one past the end"
        (define chunk (vector 3 1 4 1 5)) ; width 14
        (define a (make-rope-leaf weighted chunk))
        (check-equal? (rope-offset-index weighted a 0) 0)
        (check-equal? (rope-offset-index weighted a 13) 4)
        (check-not-exn (λ () (rope-offset-index weighted a 14)))
        (check-equal? (rope-offset-index weighted a 14) 4))))

  (define balance-suite
    (test-suite "conversions and balance"
      (test-property "chunk-rope maintains balance"
          #:trials 100
          ([chunk (random-weighted-chunk (random 500))])
        (define a (chunk->rope weighted chunk))
        (and (equal? (weighted->vec a) chunk) (rope-mostly-balanced? a)))

      (test-case "chunk larger than limit produces at least one node"
        (define chunk (random-weighted-chunk (* 3 WEIGHTED-CHUNK-LIMIT)))
        (check-true (> (rope-depth (chunk->rope weighted chunk)) 0)))

      (test-case "a deeply concatenated rope is unbalanced"
        (define a (for/fold ([a (make-rope-leaf weighted (vector 1))])
                            ([_ (in-range 99)])
                    (rope-concat weighted a (make-rope-leaf weighted (vector 1)))))
        (check-equal? (rope-depth a) 99)
        (check-not-exn (λ () (rope-strictly-balanced? a))) ; fib-bound clamp
        (check-not-exn (λ () (rope-mostly-balanced? a)))   ; doesn't crash
        (check-false (rope-strictly-balanced? a))
        (check-false (rope-mostly-balanced? a)))))

  (define (cursor-position index path)
    (for/fold ([pos index]) ([fr (in-list path)])
      (if (eq? (crumb-side fr) 'right)
          (+ pos (rope-length (crumb-left fr)))
          pos)))

  (define cursor-suite
    (test-suite "cursor"
      (test-property "cursor-advance and cursor-retreat are inverses"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))]
           [a (chunk->rope weighted chunk)]
           [n (rope-length a)]
           [i (random n)]
           [k (- (random n) i)])            ;; keeps i+k in [0, n)
        (define c0 (rope->cursor a i))
        (define c1 (cursor-advance c0 k))
        (define c2 (cursor-advance c1 (- k)))
        (and c1 c2
             (= (cursor-position (cursor-index c0) (cursor-path c0))
                (cursor-position (cursor-index c2) (cursor-path c2)))
             (equal? (weighted-cursor-peek c0) (weighted-cursor-peek c2))))

      (test-property "cursor-advance by k agrees with k single steps"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))]
           [a (chunk->rope weighted chunk)]
           [n (rope-length a)]
           [i (random n)]
           [k (- (random n) i)])
        (define jump (cursor-advance (rope->cursor a i) k))
        (define step
          (for/fold ([c (rope->cursor a i)]) ([_ (in-range (abs k))])
            (and c (cursor-advance c (if (positive? k) 1 -1)))))
        (and jump step
             (= (cursor-position (cursor-index jump) (cursor-path jump))
                (cursor-position (cursor-index step) (cursor-path step)))))

      (test-property "mutable-cursor-advance! round-trips"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))]
           [a (chunk->rope weighted chunk)]
           [n (rope-length a)]
           [i (random n)]
           [k (- (random n) i)])
        (define cur (rope->mutable-cursor a i))
        (define pos0 (cursor-position (mutable-cursor-index cur) (mutable-cursor-path cur)))
        (define fwd (cursor-advance! cur k))
        (define back (and fwd (cursor-advance! cur (- k))))
        (and fwd back
             (= pos0 (cursor-position (mutable-cursor-index cur) (mutable-cursor-path cur)))))

      (test-property "mutable-cursor-advance! agrees with cursor-advance"
          #:trials 300
          ([chunk (random-weighted-chunk (add1 (random 100)))]
           [a (chunk->rope weighted chunk)]
           [n (rope-length a)]
           [i (random n)]
           [k (- (random n) i)])
        (define c  (cursor-advance (rope->cursor a i) k))
        (define mc (cursor-advance! (rope->mutable-cursor a i) k))
        (and c mc
             (= (cursor-position (cursor-index c) (cursor-path c))
                (cursor-position (mutable-cursor-index mc) (mutable-cursor-path mc)))))))

  (define regression-suite
    (test-suite "regressions"
      (test-case "rope-rebalance does not skip intervening occupied slots"
        (define chunks (list #(3) #() #(2 1)))
        (define leaves (map (λ (c) (make-rope-leaf weighted c)) chunks))
        (define r (rope-append weighted leaves))
        (define a (rope->chunk weighted r))
        (define b (apply vector-append chunks))
        (check-equal? a b))

      (test-case "a single-leaf rope is rope-content=? to a many-node variant"
        (define chars (for/list ([i (in-range 3)]) (integer->char i)))
        (define leaf (string-chunk->rope (apply string chars)))
        (define nodes (for/fold ([a (make-empty-string-rope)])
                                ([char (in-list chars)])
                        (string-rope-concat a (string-chunk->rope (string char)))))
        (check-true (string-rope-content=? leaf nodes))
        (check-true (string-rope-content=? nodes leaf))
        (check-equal? leaf nodes)
        (check-equal? nodes leaf))

      (test-case "distinct rope types with equal length and hash are not equal?"
        (check-false (equal? (make-empty-string-rope) (make-empty-weighted-rope))))

      ))

  (run-suite! (test-suite "generic-tests.rkt"
                generic-ops-suite
                core-ops-suite
                append-suite
                split-suite
                splice/slice-suite
                offset-index-suite
                balance-suite
                cursor-suite
                regression-suite)))
