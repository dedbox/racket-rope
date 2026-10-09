#lang racket/base

(module+ test
  (require rope/type/bytes
           "../private/testing.rkt")

  (define test-suite-bytes
    (test-suite "type/bytes.rkt"
      (test-case "primitive wiring agrees with the underlying bytes functions"
        (check-equal? (bytes-chunk-ref #"hello" 1) (bytes-ref #"hello" 1))
        (check-equal? (bytes-chunk-length #"hello") (bytes-length #"hello")))

      (test-case "source->bytes-rope / bytes-rope->source round trip"
        (check-equal? (rope->bytes (bytes->rope #"hello, rope")) #"hello, rope")
        (check-equal? (rope->bytes (bytes->rope #"")) #""))

      (test-case "bytes-rope=? agrees with bytes=?"
        (check-true  (bytes-rope=? (bytes->rope #"same") (bytes->rope #"same")))
        (check-false (bytes-rope=? (bytes->rope #"same") (bytes->rope #"diff"))))

      (test-case "total-order instance agrees with numeric byte order"
        (check-true  (bytes-rope<? (bytes->rope #"abc") (bytes->rope #"abd")))
        (check-true  (bytes-rope>? (bytes->rope #"abd") (bytes->rope #"abc")))
        (check-true  (bytes-rope<? (bytes->rope #"ab") (bytes->rope #"abc"))))))

  (run-suite! test-suite-bytes))
