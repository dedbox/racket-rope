#lang racket/base

;; string3's only reason to exist is a chunk-limit small enough to force
;; real tree structure (several leaves, at least one internal node) even
;; for short input -- so this suite deliberately uses an input long
;; enough to actually exercise that, rather than one short enough to fit
;; in a single leaf and defeat the type's own purpose.

(module+ test
  (require rope/rope
           rope/type/string3
           "../private/testing.rkt")

  (define sample "a string long enough to force several chunk splits")

  (define test-suite-string3
    (test-suite "type/string3.rkt"
      (test-case "a chunk-limit of 3 actually forces a multi-leaf tree"
        (define r (source->string3-rope sample))
        (check-true (string3-rope-node? r))
        (check-true (> (length (rope-chunks r)) 1))
        (for ([c (in-list (rope-chunks r))])
          (check-true (<= (string-length c) 3))))

      (test-case "source->rope / rope->source round trip across many chunks"
        (check-equal? (string3-rope->source (source->string3-rope sample)) sample))))

  (run-suite! test-suite-string3))
