#lang racket/base

(module+ test
  (require rackunit
           racket/sequence
           racket/vector
           rope/define-rope-type
           rope/rope
           "private/testing.rkt")

  ;; A minimal instance: raw = vector of symbols. limit/empty are thunks.
  (define (sym-raw-compare a b)
    (define la (vector-length a))
    (define lb (vector-length b))
    (let loop ([i 0])
      (cond [(and (= i la) (= i lb)) '=]
            [(= i la) '<]
            [(= i lb) '>]
            [else
             (define sa (symbol->string (vector-ref a i)))
             (define sb (symbol->string (vector-ref b i)))
             (cond [(string<? sa sb) '<]
                   [(string>? sa sb) '>]
                   [else (loop (add1 i))])])))

  (define-rope-type sym
    (λ (v) (and (vector? v) (for/and ([e (in-vector v)]) (symbol? e))))
    (λ () 4)
    (λ () (vector))
    vector-length
    vector-length
    (λ (v s e) (vector-copy v s e))
    (λ (raws) (apply vector-append raws))
    vector-ref
    #:compare sym-raw-compare)

  ;; Macro-safety / hygiene probe: rope.rkt used to use the identifier `gen`
  ;; internally. This confirms nothing leaks.
  (define-rope-type gen
    vector?
    (λ () 4)
    (λ () (vector))
    vector-length
    vector-length
    (λ (v s e) (vector-copy v s e))
    (λ (raws) (apply vector-append raws))
    vector-ref)

  (define instantiation-suite
    (test-suite "define-rope-type instantiation"
      (test-case "raw generics are bound and correct; thunk-based limit/empty are re-invoked"
        (check-true  (sym-raw? (vector 'a 'b)))
        (check-false (sym-raw? (vector 1 2)))
        (check-equal? (sym-raw-limit) 4)
        (check-equal? (sym-raw-empty) (vector))
        (check-equal? (sym-raw-count (vector 'a 'b 'c)) 3)
        (check-equal? (sym-raw-width (vector 'a 'b 'c)) 3)
        (check-equal? (sym-raw-slice (vector 'a 'b 'c 'd) 1 3) (vector 'b 'c))
        (check-equal? (sym-raw-append (vector 'a) (vector 'b)) (vector 'a 'b))
        (check-equal? (sym-raw-ref (vector 'a 'b 'c) 2) 'c)
        (check-equal? (sym-raw-empty) (sym-raw-empty)))          ; equal?, need not be eq?

      (test-case "conversions round-trip; make-empty is genuinely empty and correctly tagged"
        (define v (vector 'a 'b 'c 'd 'e 'f 'g))
        (define r (sym->rope v))
        (check-equal? (rope->sym r) v)
        (check-equal? (rope-count r) (vector-length v))
        (define e (make-empty-sym-rope))
        (check-true (rope-empty? e))
        (check-true (sym-rope? e)))

      (test-case "append1 / append / offset-index / split / splice / slice"
        (define v1 (vector 'a 'b 'c))
        (define v2 (vector 'd 'e))
        (define r1 (sym-rope-append1 (sym->rope v1) (sym->rope v2)))
        (check-equal? (rope->sym r1) (vector-append v1 v2))
        (check-equal? (sym-rope-offset-index r1 0) 0)
        (check-equal? (sym-rope-offset-index r1 4) 4)
        (define r (sym->rope (vector 'a 'b 'c 'd 'e)))
        (define-values (l rr) (sym-rope-split r 2))
        (check-equal? (rope->sym l) (vector 'a 'b))
        (check-equal? (rope->sym rr) (vector 'c 'd 'e))
        (check-equal? (rope->sym (sym-rope-splice r 1 2 (vector 'x))) (vector 'a 'x 'd 'e))
        (check-equal? (rope->sym (sym-rope-slice r 1 3)) (vector 'b 'c 'd)))

      (test-case "cursor identifiers"
        (define r (sym->rope (vector 'a 'b 'c)))
        (define c (sym-rope->cursor r))
        (check-false (sym-cursor-at-end? c))
        (check-equal? (sym-cursor-peek c) 'a)
        (check-equal? (sym-cursor-peek (sym-cursor-advance c)) 'b)
        (check-equal? (rope->sym (cursor->sym-rope (sym-cursor-drop c 2))) (vector 'c))
        (check-equal? (rope->sym (sym-cursor-take c 2)) (vector 'a 'b))
        (check-true (sym-rope? (sym-cursor-take c 2)))
        (check-false (sym-cursor-peek (sym-cursor-drop c 3))))

      (test-case "fold identifiers"
        (check-equal? (sym-rope-foldl (λ (acc x) (cons x acc)) '() (sym->rope (vector 1 2 3)))
                      '(3 2 1)))

      (test-case "in-sym-rope: for-loop and first-class sequence use agree"
        (define r (sym->rope (vector 'a 'b 'c 'd 'e)))
        (define via-for (for/list ([x (in-sym-rope r)]) x))
        (check-equal? via-for (vector->list (rope->sym r)))
        (check-equal? (sequence->list (in-sym-rope-runtime r)) via-for)
        (check-exn exn:fail:contract? (λ () (sequence->list (in-sym-rope-runtime 5)))))

      (test-case "instance predicates tag their own leaves and nodes, not each other's"
        (define sym-r (sym->rope (vector 'a 'b)))
        (define gen-r (gen->rope (vector 1 2)))
        (check-true  (sym-rope? sym-r))
        (check-false (gen-rope? sym-r))
        (check-true  (gen-rope? gen-r))
        (check-false (sym-rope? gen-r))
        (check-false (sym-rope? (rope-leaf 1 1 (vector 'a)))))   ; a bare rope-leaf is not tagged

      (test-case "tagging survives multi-leaf construction and rebalancing, not just fresh leaves"
        ;; Exceeds the raw limit (4), forcing internal nodes, then forces a
        ;; further rebalance via repeated append1 — exactly the path that used
        ;; to lose the type tag.
        (define syms (build-vector 50 (λ (i) (string->symbol (format "s~a" i)))))
        (define big (sym->rope syms))
        (check-true (rope-node? big))
        (check-true (sym-rope? big))
        (define built-via-append
          (for/fold ([r (make-empty-sym-rope)]) ([s (in-vector syms)])
            (sym-rope-append1 r (sym->rope (vector s)))))
        (check-true (sym-rope? built-via-append))
        (check-equal? (rope->sym built-via-append) syms))

      (test-case "comparison identifiers: compare / </<=/>/>=/rope=? agree with a raw oracle"
        (define a (sym->rope (vector 'a 'b 'c)))
        (define b (sym->rope (vector 'a 'b 'd)))
        (define c (sym->rope (vector 'a 'b 'c))) ; equal content, distinct object
        (check-eq? (sym-rope-compare a a) '=)
        (check-eq? (sym-rope-compare a b) '<)
        (check-eq? (sym-rope-compare b a) '>)
        (check-true  (sym-rope<?  a b))
        (check-false (sym-rope<?  b a))
        (check-true  (sym-rope<=? a c))
        (check-true  (sym-rope>?  b a))
        (check-true  (sym-rope>=? a c))
        (check-true  (sym-rope=?  a c))
        (check-false (sym-rope=?  a b))
        (check-true  (equal? a c)))))   ; content-hash path still agrees

  ;; rope-type-out/contract needs its own contracted vs. uncontracted views of
  ;; the same instantiation to test contract enforcement in isolation.
  (module sym-fixture racket/base
    (require racket/vector
             rope/define-rope-type)

    (provide (rope-type-out sym))

    (define-rope-type sym
      (λ (v) (and (vector? v) (for/and ([e (in-vector v)]) (symbol? e))))
      (λ () 4)
      (λ () (vector))
      vector-length
      vector-length
      (λ (v s e) (vector-copy v s e))
      (λ (raws) (apply vector-append raws))
      vector-ref
      #:compare (λ (a b) (cond [(string<? (format "~a" a) (format "~a" b)) '<]
                               [(equal? a b) '=]
                               [else '>]))))

  (module sym-fixture-contracted racket/base
    (require rope/define-rope-type
             (submod ".." sym-fixture))

    (provide (rope-type-out/contract sym #:raw sym-raw? #:element symbol?)))

  (require 'sym-fixture
           (prefix-in c: 'sym-fixture-contracted))

  (define contract-suite
    (test-suite "rope-type-out/contract"
      (test-case "plain rope-type-out attaches no contracts"
        (check-not-exn (λ () (sym-rope-leaf 1 1 "not even a vector"))))

      (test-case "rope-type-out/contract enforces #:raw on raw-* functions and the struct constructor"
        (check-exn exn:fail:contract? (λ () (c:make-sym-rope-leaf (vector 1 2))))
        (check-not-exn (λ () (sym-rope-leaf 2 2 (vector 'a 'b))))
        (check-exn exn:fail:contract? (λ () (c:sym-rope-leaf 2 2 (vector 1 2)))))

      (test-case "cursor-peek's contract accepts #f at the end"
        (define r (c:sym->rope (vector 'a)))
        (define c (c:sym-rope->cursor r))
        (check-not-exn (λ () (c:sym-cursor-peek (c:sym-cursor-drop c 1)))))

      (test-case "cursor-take wires under #:raw / #:element"
        (define r (c:sym->rope (vector 'a 'b 'c)))
        (define c (c:sym-rope->cursor r))
        (define taken (c:sym-cursor-take c 2))
        (check-true (c:sym-rope? taken))
        (check-equal? (rope->sym taken) (vector 'a 'b))
        (check-exn exn:fail:contract? (λ () (c:sym-cursor-take c -1))))

      (test-case "rope-type-out/contract enforces argument types on *rope=?"
        (define r (c:sym->rope (vector 'a 'b)))
        (check-not-exn (λ () (c:sym-rope=? r r)))
        (check-exn exn:fail:contract? (λ () (c:sym-rope=? r "not a rope"))))))

  (run-suite! (test-suite "define-rope-type.rkt" instantiation-suite contract-suite)))
