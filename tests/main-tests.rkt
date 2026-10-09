#lang racket/base

;; main.rkt currently defines and re-exports nothing, so there's nothing
;; meaningful to check beyond "this module compiles". Once it actually
;; re-exports the public API (define-rope-type, define-rope-class,
;; rope?, ...), this suite should grow into a check that every expected
;; top-level binding is reachable through a plain (require rope) --
;; flagging that here so it isn't forgotten once main.rkt is filled in.

(module+ test
  (require rackunit
           rackunit/text-ui
           rope)

  (run-tests
   (test-suite "main.rkt"
     (test-case "the module requires and compiles without error"
       (check-true #t)))))
