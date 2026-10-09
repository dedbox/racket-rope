#lang racket/base


;; The *algorithm* (append/split/slice/cursor/fold correctness) is tested
;; thoroughly, once, generically, in generic.rkt's own suite. This suite
;; is about define-rope-type's own responsibilities specifically: that
;; the type-specific wrapper names agree with the primitives and the
;; generic wrappers they're built from, and that every optional
;; equality/hashing keyword behaves correctly both supplied and omitted.

(module+ test
  (require racket/vector
           rope/generic
           rope/rope 
           rope/type
           "../cursor.rkt"
           "../private/testing.rkt")

  ;; A bare nat for #:chunk-limit and a bare literal for #:chunk-empty
  ;; specifically exercise nat+id+fun0/lit+id+fun0's synthesized
  ;; `callable` closures end to end (promised back in stxclasses.rkt's
  ;; own suite, which can't call them directly at the right phase).
  (define-rope-type ty1
    #:chunk?       vector?
    #:chunk-limit  4
    #:chunk-empty  vector
    #:chunk-length vector-length
    #:chunk-ref    vector-ref
    #:chunk-slice  (λ (c i k) (vector-copy c i (+ i k)))
    #:chunk-append (λ (cs) (apply vector-append cs))
    #:elem-width   1)

  (define-rope-type ty2 ; same shape, but every equality/hash keyword custom
    #:chunk?       vector?
    #:chunk-limit  4
    #:chunk-empty  vector
    #:chunk-length vector-length
    #:chunk-ref    vector-ref
    #:chunk-slice  (λ (c i k) (vector-copy c i (+ i k)))
    #:chunk-append (λ (cs) (apply vector-append cs))
    #:elem-width   1
    #:chunk=?      (λ (c d) #t)          ; deliberately permissive, to detect use
    #:elem=?       (λ (x y) (= (modulo x 10) (modulo y 10)))
    #:elem-hash    (λ (x) (modulo x 10))
    #:chunk-overlap=? (λ (c d ic id k) #t)) ; also deliberately permissive

  (define primitives-suite
    (test-suite "primitives"
      (test-case "chunk primitives agree with the functions they wrap"
        (check-true  (ty1-chunk? (vector 1 2)))
        (check-false (ty1-chunk? "nope"))
        (check-equal? (ty1-chunk-length (vector 1 2 3)) 3)
        (check-equal? (ty1-chunk-ref (vector 'a 'b) 1) 'b)
        (check-equal? (ty1-chunk-slice (vector 1 2 3 4) 1 2) (vector 2 3))
        (check-equal? (ty1-chunk-append (vector 1) (vector 2 3)) (vector 1 2 3)))

      (test-case "a bare-nat #:chunk-limit and bare-literal #:chunk-empty work end to end"
        (check-equal? (ty1-chunk-empty) (vector))
        ;; forcing more than 4 elements through source->rope must actually
        ;; split, confirming the nat #:chunk-limit's synthesized callable
        ;; returns the right number, not just *some* number.
        (define big (source->ty1-rope (vector 1 2 3 4 5 6 7 8 9)))
        (check-true (ty1-rope-node? big)))

      (test-case "#:elem-width as a bare number drives chunk-width directly"
        (check-equal? (ty1-chunk-width (vector 1 2 3)) 3))))

  (define construction-suite
    (test-suite "tree construction and smart constructors"
      (test-case "make-rope-leaf/make-rope-node/make-empty-rope agree with the predicates"
        (define leaf (make-ty1-rope-leaf (vector 1 2)))
        (check-true (ty1-rope-leaf? leaf))
        (check-true (ty1-rope? leaf))
        (define empty (make-empty-ty1-rope))
        (check-true (ty1-rope-leaf? empty))
        (check-equal? (rope-length empty) 0))))

  (define equality-suite
    (test-suite "content-based equality and hashing"
      (test-case "default #:chunk=?/#:elem=? (equal?) and #:elem-hash (equal-hash-code)"
        (define a (source->ty1-rope (vector 1 2 3)))
        (define b (source->ty1-rope (vector 1 2 3)))
        (define c (source->ty1-rope (vector 1 2 4)))
        (check-true  (ty1-rope=? a b))
        (check-false (ty1-rope=? a c))
        (check-true  (equal? a b)))

      (test-case "custom #:elem=?/#:elem-hash are actually used, not ignored"
        ;; ty2's elem=? treats values equal mod 10 as equal -- 3 and 13 are
        ;; not equal? but *are* elem=?, so the ropes must compare equal.
        (define a (source->ty2-rope (vector 3 12 23)))
        (define b (source->ty2-rope (vector 13 2 3)))
        (check-true (ty2-rope=? a b)))

      (test-case "custom #:chunk=? is consulted for whole-chunk comparisons"
        ;; ty2's chunk=? always says yes; two otherwise-unequal single-chunk
        ;; ropes of the same length must therefore compare equal.
        (define a (source->ty2-rope (vector 1 2)))
        (define b (source->ty2-rope (vector 99 98)))
        (check-true (ty2-rope=? a b)))))

  (define conversion-suite
    (test-suite "conversions"
      (test-case "source->rope / rope->source round-trip"
        (define src (vector 1 2 3 4 5))
        (check-equal? (ty1-rope->source (source->ty1-rope src)) src))))

  (define wrapper-agreement-suite
    (test-suite "type-specific wrappers agree with the generic API"
      (test-case "ty1-rope-append agrees with generic rope-append"
        (define a (source->ty1-rope (vector 1 2)))
        (define b (source->ty1-rope (vector 3 4)))
        (check-equal? (ty1-rope->source (ty1-rope-append2 a b))
                      (ty1-rope->source (rope-append2 ty1 a b))))

      (test-case "ty1-rope-foldl (single-rope form) agrees with a plain vector fold"
        (define a (source->ty1-rope (vector 1 2 3 4 5)))
        (check-equal? (ty1-rope-foldl (λ (acc x) (+ acc x)) 0 a) 15))

      (test-case "ty1-cursor-peek / mutable-ty1-cursor-peek agree, same position"
        (define a (source->ty1-rope (vector 'x 'y 'z)))
        (check-equal? (ty1-cursor-peek (rope->cursor a 1))
                      (mutable-ty1-cursor-peek (rope->mutable-cursor a 1)))
        (check-eq? (ty1-cursor-peek (rope->cursor a 1)) 'y))))

  (run-suite! (test-suite "type.rkt"
                          primitives-suite construction-suite equality-suite
                          conversion-suite wrapper-agreement-suite)))
