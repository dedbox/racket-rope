#lang racket/base

;; These are pure compile-time syntax classes -- there's no runtime
;; behavior to call, only "does this input match". `stx-matches?` (from
;; private/testing.rkt) turns that compile-time outcome into an ordinary
;; runtime boolean so rackunit can check it normally.

(module+ test
  (require (for-syntax racket/base
                       rope/stxclasses
                       syntax/parse)
           "../private/testing.rkt")

  (define idfun-suite
    (test-suite "id+funN family"
      (test-case "id+fun1: identifier or exactly 1 arg"
        (check-true  (stx-matches? id+fun1 some-name))
        (check-true  (stx-matches? id+fun1 (lambda (x) x)))
        (check-true  (stx-matches? id+fun1 (λ (x) x)))
        (check-false (stx-matches? id+fun1 (lambda () 1)))
        (check-false (stx-matches? id+fun1 (lambda (x y) x)))
        (check-false (stx-matches? id+fun1 "not callable at all")))

      (test-case "id+fun2: identifier or exactly 2 args"
        (check-true  (stx-matches? id+fun2 some-name))
        (check-true  (stx-matches? id+fun2 (lambda (x y) x)))
        (check-false (stx-matches? id+fun2 (lambda (x) x)))
        (check-false (stx-matches? id+fun2 (lambda (x y z) x))))

      (test-case "id+fun3: identifier or exactly 3 args"
        (check-true  (stx-matches? id+fun3 (lambda (x y z) x)))
        (check-false (stx-matches? id+fun3 (lambda (x y) x)))
        (check-false (stx-matches? id+fun3 (lambda (w x y z) x))))

      (test-case "id+fun5: identifier or exactly 5 args"
        (check-true  (stx-matches? id+fun5 (lambda (a b c d e) a)))
        (check-false (stx-matches? id+fun5 (lambda (a b c d) a)))
        (check-false (stx-matches? id+fun5 (lambda (a b c d e f) a))))))

  (define nat-callable-suite
    (test-suite "nat+id+funN and lit+id+fun0: callable synthesis"
      (test-case "nat+id+fun0: a bare nat, an identifier, or a 0-arg function"
        (check-true  (stx-matches? nat+id+fun0 7))
        (check-true  (stx-matches? nat+id+fun0 some-name))
        (check-true  (stx-matches? nat+id+fun0 (lambda () 7)))
        (check-false (stx-matches? nat+id+fun0 -1))
        (check-false (stx-matches? nat+id+fun0 (lambda (x) x))))

      ;; The #:attr callable branches synthesize a closure around a bare
      ;; nat/literal -- that synthesis only matters once something calls
      ;; the result, which `callable` itself can't test in isolation
      ;; (its attribute lives at the phase where the class is *used*
      ;; inside a macro pattern, not at ordinary runtime). It's exercised
      ;; properly, end to end, by type.rkt's own suite: #:elem-width and
      ;; #:chunk-empty are supplied as bare literals there specifically
      ;; to confirm the synthesized callables behave correctly once a
      ;; real rope actually calls them.

      (test-case "nat+id+fun2: rejects a nat written as a negative number"
        (check-false (stx-matches? nat+id+fun2 -5)))))

  (define lit-suite
    (test-suite "lit and its building blocks"
      (test-case "self-quoting atoms"
        (check-true (stx-matches? lit 5))
        (check-true (stx-matches? lit "a string"))
        (check-true (stx-matches? lit #t))
        (check-true (stx-matches? lit #\c)))

      (test-case "quoted single values, including non-self-quoting ones"
        (check-true (stx-matches? lit (quote sym)))
        (check-true (stx-matches? lit (quote #:a-keyword))))

      (test-case "quoted list of quotable literals"
        (check-true (stx-matches? lit (quote (a b c))))
        (check-true (stx-matches? lit (quote (1 "two" three)))))

      (test-case "vector/box/prefab/hash literals, bare and quoted"
        (check-true (stx-matches? lit #(1 2 3)))
        (check-true (stx-matches? lit #&5))
        (check-true (stx-matches? lit #hash((a . 1))))
        (check-true (stx-matches? lit (quote (#(1 2) "mixed")))))

      (test-case "an ordinary identifier is not, by itself, a lit"
        (check-false (stx-matches? lit some-name))
        ;; ... but it is a quotable-lit, and so is quotable once quoted.
        (check-true  (stx-matches? lit (quote some-name))))))

  (run-suite! (test-suite "stxclasses.rkt" idfun-suite nat-callable-suite lit-suite)))
