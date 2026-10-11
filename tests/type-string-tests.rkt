#lang racket/base

;; Deliberately thin: the generic algorithm (append/split/cursor/fold) is
;; already covered thoroughly, once, in generic.rkt's own suite, against a
;; synthetic type. This suite only confirms string's own primitive wiring
;; and its total-order instance.

(module+ test
  (require rope/type/string
           "../private/testing.rkt")

  (define test-suite-string
    (test-suite "type/string.rkt"
      (test-case "primitive wiring agrees with the underlying string functions"
        (check-equal? (string-chunk-ref "hello" 1) (string-ref "hello" 1))
        (check-equal? (string-chunk-length "hello") (string-length "hello")))

      (test-case "source->string-rope / string-rope->source round trip"
        (check-equal? (rope->string (string->rope "hello, rope")) "hello, rope")
        (check-equal? (rope->string (string->rope "")) ""))

      (test-case "string-rope-ref indexes characters"
        (define r (string->rope "hello"))
        (check-equal? (string-rope-ref r 0) #\h)
        (check-equal? (string-rope-ref r 4) #\o))

      (test-case "string-rope=? agrees with string=?"
        (check-true  (string-rope=? (string->rope "same") (string->rope "same")))
        (check-false (string-rope=? (string->rope "same") (string->rope "diff"))))

      (test-case "total-order instance agrees with char<?/char>?-based string order"
        (check-true  (string-rope<? (string->rope "abc") (string->rope "abd")))
        (check-true  (string-rope>? (string->rope "abd") (string->rope "abc")))
        ;; equal length, different content
        (check-true  (string-rope<? (string->rope "aaa") (string->rope "aab")))
        ;; different length, shared prefix
        (check-true  (string-rope<? (string->rope "ab") (string->rope "abc")))
        (check-true  (string-rope>? (string->rope "abc") (string->rope "ab"))))))

  (run-suite! test-suite-string))
