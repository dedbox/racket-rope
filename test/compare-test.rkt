#lang racket/base

(module+ test
  (require rackunit
           rope/string
           rope/bytes)

  (test-case "string-rope ordering matches string<?/string>? on flat strings"
    (define a (string->rope "apple"))
    (define b (string->rope "banana"))
    (check-true  (string-rope<? a b))
    (check-false (string-rope>? a b))
    (check-true  (string-rope<=? a a))
    (check-true  (string-rope>=? b a)))

  (test-case "prefix relationships order by length"
    (check-true (string-rope<? (string->rope "ab") (string->rope "abc"))))

  (test-case "ordering is unaffected by tree shape"
    (define a (string->rope "hello world"))
    (define b (string-rope-append (string->rope "hello ") (string->rope "world")))
    (check-eq? (string-rope-compare a b) '=))

  (test-case "case-insensitive comparisons"
    (define a (string->rope "Apple"))
    (define b (string->rope "apple"))
    (check-eq? (string-rope-compare a b) '<)      ; case-sensitive: 'A' < 'a'
    (check-eq? (string-rope-ci-compare a b) '=)
    (check-true (string-rope-ci<=? a b))
    (check-true (string-rope-ci>=? a b)))

  (test-case "bytes-rope ordering"
    (check-true (bytes-rope<? (bytes->rope #"aaa") (bytes->rope #"aab")))))
