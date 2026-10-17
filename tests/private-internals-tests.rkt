#lang racket/base

;; Direct, isolated tests of private/ helpers that are shared across
;; multiple public files and are awkward to exercise precisely through
;; any single one of them. This file does not conceptually belong to any
;; one module, per the testing plan's own stated exception for exactly
;; this case.

(module+ test
  (require racket/set
           "../private/edit-distance.rkt"
           "../private/testing.rkt")

  ;; -------------------------------------------------------------------------------------
  ;; private/edit-distance.rkt
  ;; -------------------------------------------------------------------------------------

  (define edit-distance-suite
    (test-suite "bounded-levenshtein / nearest-name"
      (test-case "bounded-levenshtein: identical strings are distance 0"
        (check-equal? (bounded-levenshtein "same" "same" 3) 0))

      (test-case "bounded-levenshtein: one substitution is distance 1"
        (check-equal? (bounded-levenshtein "cat" "bat" 3) 1))

      (test-case "bounded-levenshtein: symmetric regardless of argument order"
        (check-equal? (bounded-levenshtein "cat" "cats" 3)
                      (bounded-levenshtein "cats" "cat" 3)))

      (test-case "bounded-levenshtein: #f when the length gap alone exceeds the bound"
        (check-false (bounded-levenshtein "cat" "elephant" 3)))

      (test-case "nearest-name: exact match in the valid set returns the name unchanged"
        (check-eq? (nearest-name 'p (seteq 'p 'q) (list "p" "q")) 'p))

      (test-case "nearest-name: a near miss resolves to the closest valid name"
        (check-eq? (nearest-name 'pp (seteq 'p 'q) (list "p" "q")) 'p))

      (test-case "nearest-name: no candidate within range should report no suggestion"
        (check-false (nearest-name 'zzzzzzzzzzzzzzzzzzzz (seteq 'p 'q) (list "p" "q"))))))

  (run-suite! (test-suite "private internals"
                edit-distance-suite)))
