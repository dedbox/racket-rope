#lang racket/base

(module+ test
  (require rackunit rope)

  (define (mixed-build str)
    ;; Build the same logical content two structurally different ways so
    ;; equal? is exercised across leaf boundaries that don't align.
    (string-rope-append (string->rope (substring str 0 3))
                        (string->rope (substring str 3))))

  (test-case "equal? is content-based across differing tree shapes"
    (define a (string->rope "hello world"))
    (define b (mixed-build "hello world"))
    (check-true (equal? a b))
    (check-false (equal? a (string->rope "hello worlD"))))

  (test-case "equal-hash-code agrees whenever equal? does"
    (define a (string->rope "supercalifragilisticexpialidocious"))
    (define b (mixed-build  "supercalifragilisticexpialidocious"))
    (check-equal? (equal-hash-code a) (equal-hash-code b))
    (check-equal? (equal-secondary-hash-code a) (equal-secondary-hash-code b)))

  (test-case "empty ropes are equal regardless of provenance"
    (check-true (equal? empty-string-rope (string->rope ""))))

  (test-case "ropes usable as hash-table keys"
    (define h (hash))
    (define h2 (hash-set h (string->rope "key") 'value))
    (check-equal? (hash-ref h2 (mixed-build "key")) 'value))

  (test-case "eq? fast path short-circuits identical ropes"
    (define a (string->rope "abc"))
    (check-true (equal? a a)))

  (test-case "bytes ropes: same coverage"
    (define a (bytes->rope #"hello world"))
    (define b (bytes-rope-append (bytes->rope #"hello ") (bytes->rope #"world")))
    (check-true (equal? a b))
    (check-equal? (equal-hash-code a) (equal-hash-code b))))
