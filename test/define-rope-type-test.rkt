#lang racket/base

(module+ test
  (require rackunit
           racket/sequence
           racket/vector
           rope/rope
           rope/define-rope-type)

  ;; A minimal instance: raw = vector of symbols. limit/empty are thunks now.
  (define-rope-type sym
    (λ (v) (and (vector? v) (for/and ([e (in-vector v)]) (symbol? e))))
    (λ () 4)
    (λ () (vector))
    vector-length
    vector-length
    (λ (v s e) (vector-copy v s e))
    (λ (raws) (apply vector-append raws))
    vector-ref)

  ;; Macro-safety / hygiene probe: name the type `gen`, textually identical to the identifier the
  ;; macro's own internal `splicing-let` introduces.
  (define-rope-type gen
    vector?
    (λ () 4)
    (λ () (vector))
    vector-length
    vector-length
    (λ (v s e) (vector-copy v s e))
    (λ (raws) (apply vector-append raws))
    vector-ref)

  ;;; -------------------------------------------------------------------------------------------
  ;;; Raw generics — including confirming the thunk-based limit/empty are re-invoked, not cached
  ;;; -------------------------------------------------------------------------------------------
  (test-case "raw generics are bound and correct"
    (check-true  (sym-raw? (vector 'a 'b)))
    (check-false (sym-raw? (vector 1 2)))
    (check-equal? (sym-raw-limit) 4)
    (check-equal? (sym-raw-empty) (vector))
    (check-equal? (sym-raw-count (vector 'a 'b 'c)) 3)
    (check-equal? (sym-raw-width (vector 'a 'b 'c)) 3)
    (check-equal? (sym-raw-slice (vector 'a 'b 'c 'd) 1 3) (vector 'b 'c))
    (check-equal? (sym-raw-append (vector 'a) (vector 'b)) (vector 'a 'b))
    (check-equal? (sym-raw-ref (vector 'a 'b 'c) 2) 'c))

  (test-case "raw-empty is invoked fresh each call (equal?, need not be eq?)"
    (check-equal? (sym-raw-empty) (sym-raw-empty))
    (check-equal? (sym-raw-limit) (sym-raw-limit)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Conversions, construction
  ;;; -------------------------------------------------------------------------------------------
  (test-case "conversions round-trip"
    (define v (vector 'a 'b 'c 'd 'e 'f 'g))
    (define r (sym-raw->sym-rope v))
    (check-equal? (sym-rope->sym-raw r) v)
    (check-equal? (rope-count r) (vector-length v)))

  (test-case "make-empty-sym-rope produces a genuinely empty, correctly-tagged rope"
    (define e (make-empty-sym-rope))
    (check-true (rope-empty? e))
    (check-equal? (rope-count e) 0)
    (check-true (sym-rope? e)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; append1 / append / split / splice / slice / offset-index via the generated API
  ;;; -------------------------------------------------------------------------------------------
  (test-case "append1 / append / offset-index"
    (define v1 (vector 'a 'b 'c))
    (define v2 (vector 'd 'e))
    (define r1 (sym-rope-append1 (sym-raw->sym-rope v1) (sym-raw->sym-rope v2)))
    (check-equal? (sym-rope->sym-raw r1) (vector-append v1 v2))
    (define r2 (sym-rope-append (sym-raw->sym-rope v1) (sym-raw->sym-rope v2) (make-empty-sym-rope)))
    (check-equal? (sym-rope->sym-raw r2) (vector-append v1 v2))
    (check-equal? (sym-rope-offset-index r1 0) 0)
    (check-equal? (sym-rope-offset-index r1 4) 4))

  (test-case "split / splice / slice"
    (define r (sym-raw->sym-rope (vector 'a 'b 'c 'd 'e)))
    (define-values (l rr) (sym-rope-split r 2))
    (check-equal? (sym-rope->sym-raw l) (vector 'a 'b))
    (check-equal? (sym-rope->sym-raw rr) (vector 'c 'd 'e))
    (check-equal? (sym-rope->sym-raw (sym-rope-splice r 1 2 (vector 'x))) (vector 'a 'x 'd 'e))
    (check-equal? (sym-rope->sym-raw (sym-rope-slice r 1 3)) (vector 'b 'c 'd)))

  (test-case "cursor identifiers"
    (define r (sym-raw->sym-rope (vector 'a 'b 'c)))
    (define c (sym-rope->cursor r))
    (check-false (sym-cursor-at-end? c))
    (check-equal? (sym-cursor-peek c) 'a)
    (check-equal? (sym-cursor-peek (sym-cursor-advance c)) 'b)
    (check-equal? (sym-rope->sym-raw (cursor->sym-rope (sym-cursor-drop c 2))) (vector 'c))
    (check-equal? (sym-rope->sym-raw (sym-cursor-take c 2)) (vector 'a 'b))
    (check-true (sym-rope? (sym-cursor-take c 2)))
    (check-false (sym-cursor-peek (sym-cursor-drop c 3))))

  (test-case "fold identifiers"
    (define r (sym-raw->sym-rope (vector 1 2 3)))
    (check-equal? (sym-rope-foldl (λ (acc x) (cons x acc)) '() r) '(3 2 1)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Sequencing: compile-time optimized clause and runtime fallback agree
  ;;; -------------------------------------------------------------------------------------------
  (test-case "in-sym-rope: for-loop and first-class sequence use agree"
    (define r (sym-raw->sym-rope (vector 'a 'b 'c 'd 'e)))
    (define via-for (for/list ([x (in-sym-rope r)]) x))
    (define via-seq (sequence->list (in-sym-rope-runtime r)))
    (check-equal? via-for (vector->list (sym-rope->sym-raw r)))
    (check-equal? via-seq via-for))

  (test-case "in-sym-rope-runtime on a non-rope fails via rope.rkt's own contract"
    (check-exn exn:fail:contract? (λ () (sequence->list (in-sym-rope-runtime 5)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Type-specific predicates — now correctly tag both leaves and nodes
  ;;; -------------------------------------------------------------------------------------------
  (test-case "instance predicates recognize their own leaves and nodes, not each other's"
    (define sym-r (sym-raw->sym-rope (vector 'a 'b)))
    (define gen-r (gen-raw->gen-rope (vector 1 2)))
    (check-true  (sym-rope? sym-r))
    (check-false (gen-rope? sym-r))
    (check-true  (gen-rope? gen-r))
    (check-false (sym-rope? gen-r)))

  (test-case "a bare, untagged rope-leaf is not an instance rope"
    (check-false (sym-rope? (rope-leaf 1 1 (vector 'a)))))

  (test-case "tagging survives multi-leaf construction, not just fresh leaves"
    ;; Force enough elements to exceed the raw limit (4) and produce internal nodes, then force a
    ;; further rebalance via repeated append1, confirming both rope-node? *and* sym-rope-node?
    ;; hold — this is exactly the path that used to crash before rope-concat's witness fix.
    (define big (sym-raw->sym-rope (build-vector 50 (λ (i) (string->symbol (format "s~a" i))))))
    (check-true (rope-node? big))
    (check-true (sym-rope? big))
    (define built-via-append
      (for/fold ([r (make-empty-sym-rope)]) ([i (in-range 50)])
        (sym-rope-append1 r (sym-raw->sym-rope (vector (string->symbol (format "s~a" i)))))))
    (check-true (sym-rope? built-via-append))
    (check-equal? (sym-rope->sym-raw built-via-append) (sym-rope->sym-raw big))))
