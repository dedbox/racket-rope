#lang racket/base

(module+ test
  (require rackunit
           "./private/sym-rope-fixture.rkt")

  (test-case "plain rope-type-out attaches no contracts"
    ;; Bypasses sym-raw?'s own check entirely — the struct constructor takes
    ;; whatever it's given.
    (check-not-exn (λ () (sym-rope-leaf 1 1 "not even a vector"))))

  (test-case "rope-type-out/contract enforces #:raw on the raw-* functions"
    (check-exn exn:fail:contract? (λ () (contracted-make-sym-rope-leaf (vector 1 2)))))

  (test-case "rope-type-out/contract now also enforces #:raw on the struct constructor"
    (check-not-exn (λ () (sym-rope-leaf 2 2 (vector 'a 'b))))
    (check-exn exn:fail:contract? (λ () (contracted-sym-rope-leaf 2 2 (vector 1 2)))))

    (test-case "rope-type-out/contract's cursor-peek contract now accepts #f at the end"
    (define r (contracted-sym-raw->sym-rope (vector 'a)))
    (define c (contracted-sym-rope->cursor r))
    (check-not-exn
     (λ () (contracted-sym-cursor-peek (contracted-sym-cursor-drop c 1)))))

  (test-case "rope-type-out/contract wires cursor-take under #:raw / #:element"
    (define r (contracted-sym-raw->sym-rope (vector 'a 'b 'c)))
    (define c (contracted-sym-rope->cursor r))
    (define taken (contracted-sym-cursor-take c 2))
    (check-true (contracted-sym-rope? taken))
    (check-equal? (sym-rope->sym-raw taken) (vector 'a 'b))
    (check-exn exn:fail:contract? (λ () (contracted-sym-cursor-take c -1)))))
