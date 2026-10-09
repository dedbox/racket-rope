#lang racket/base

;; Direct, isolated tests of private/ helpers that are shared across
;; multiple public files and are awkward to exercise precisely through
;; any single one of them. This file does not conceptually belong to any
;; one module, per the testing plan's own stated exception for exactly
;; this case.

(require racket/set
         "../private/instance.rkt"
         "../private/error.rkt"
         "../private/edit-distance.rkt"
         "../private/star.rkt"
         "../private/testing.rkt")

;; -----------------------------------------------------------------------------
;; private/instance.rkt: alist helpers
;; -----------------------------------------------------------------------------

(define alist-helpers-suite
  (test-suite "alist-remq-or-fail / alist-rename-or-fail"
    (test-case "alist-remq-or-fail removes the matching entry, keeps order"
      (check-equal? (alist-remq-or-fail 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
                    (list (cons 'a 1) (cons 'c 3))))

    (test-case "alist-remq-or-fail returns #f when the key isn't present"
      (check-false (alist-remq-or-fail 'z (list (cons 'a 1))))
      (check-false (alist-remq-or-fail 'z null)))

    (test-case "alist-remq-or-fail rejects non-list / malformed input"
      (check-exn exn:fail:contract? (λ () (alist-remq-or-fail 'a "not a list")))
      (check-exn exn:fail:contract? (λ () (alist-remq-or-fail 'a (list 1 2 3)))))

    (test-case "alist-rename-or-fail renames the key, keeps the value and order"
      (check-equal? (alist-rename-or-fail 'b 'bb (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))
                    (list (cons 'a 1) (cons 'bb 2) (cons 'c 3))))

    (test-case "alist-rename-or-fail returns #f when the key isn't present"
      (check-false (alist-rename-or-fail 'z 'zz (list (cons 'a 1))))
      (check-false (alist-rename-or-fail 'z 'zz null)))

    (test-case "alist-rename-or-fail rejects non-list / malformed input"
      (check-exn exn:fail:contract? (λ () (alist-rename-or-fail 'a 'aa "not a list")))
      (check-exn exn:fail:contract? (λ () (alist-rename-or-fail 'a 'aa (list 1 2 3)))))))

;; -----------------------------------------------------------------------------
;; private/instance.rkt: validate-mask!, called directly with hand-built
;; arguments that violate the "include and exclude are mutually
;; exclusive" invariant the public #:require grammar otherwise enforces
;; by construction (a single depmask can never populate both). Confirms
;; the "cannot exclude included parameter" guard behaves sanely if it's
;; ever actually reached, even though no valid syntax can reach it today.
;; -----------------------------------------------------------------------------

(define validate-mask-suite
  (test-suite "validate-mask! defensive branch"
    (test-case "cannot exclude included parameter, when forced directly"
      (check-exn
       exn:fail:syntax?
       (λ ()
         (validate-mask! #t
                        (list (datum->syntax #f 'p))   ; include-ids
                        (list (datum->syntax #f 'p))   ; exclude-ids (same name!)
                        null))))))

;; -----------------------------------------------------------------------------
;; private/error.rkt: with-caller / with-caller-sub
;; -----------------------------------------------------------------------------

(define caller-suite
  (test-suite "with-caller / with-caller-sub"
    ;; Each syntax literal is bound once: two separate #'x occurrences are
    ;; distinct objects, so comparing them with eq? would always fail.
    (test-case "with-caller sets all three fields"
      (define ex #'an-expr)
      (with-caller ('my-name ex #f)
        (define-values (n e s) (get-caller-info))
        (check-eq? n 'my-name)
        (check-eq? e ex)
        (check-false s)))

    (test-case "with-caller-sub overrides only sub-expr, preserving name/expr"
      (define outer-ex #'outer-expr)
      (define inner-ex #'inner-sub)
      (with-caller ('outer-name outer-ex #f)
        (with-caller-sub inner-ex
          (define-values (n e s) (get-caller-info))
          (check-eq? n 'outer-name)
          (check-eq? e outer-ex)
          (check-eq? s inner-ex))))

    (test-case "parameterize correctly restores the outer caller after a nested call returns"
      (with-caller ('outer #f #f)
        (with-caller ('inner #f #f) (void))
        (define-values (n e s) (get-caller-info))
        (check-eq? n 'outer)))

    (test-case "parameterize correctly restores the outer caller even if the inner body raises"
      ;; Directly confirms the error-reporting overhaul's move to real
      ;; parameterize: a raised exception inside a nested with-caller must
      ;; not leave the outer context corrupted for whatever runs next.
      (with-caller ('outer #f #f)
        (with-handlers ([exn:fail? (λ (e) (void))])
          (with-caller ('inner #f #f) (error "boom")))
        (define-values (n e s) (get-caller-info))
        (check-eq? n 'outer)))))

;; -----------------------------------------------------------------------------
;; private/edit-distance.rkt
;; -----------------------------------------------------------------------------

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
      ;; KNOWN ISSUE, found via this test: when nothing in other-name-list
      ;; is within MAX-K of `name`, the loop's base case still runs
      ;; (string->symbol best-name-str) with best-name-str still #f,
      ;; raising a contract violation instead of returning #f. A user
      ;; whose typo doesn't resemble any valid name would see
      ;; "string->symbol: contract violation" instead of the error
      ;; rope-error was actually trying to report. This check states the
      ;; behavior nearest-name's own contract should have (matching how
      ;; rope-error, its only caller, already handles a #f result); it is
      ;; expected to fail against the current implementation.
      (check-false (nearest-name 'zzzzzzzzzzzzzzzzzzzz (seteq 'p 'q) (list "p" "q"))))))

;; -----------------------------------------------------------------------------
;; private/star.rkt: update-identifiers over syntax shapes no current
;; class/type body happens to use
;; -----------------------------------------------------------------------------

(define star-traversal-suite
  (test-suite "star-expand / star-substitute over exotic syntax shapes"
    (test-case "vector literal"
      (define result (star-expand #'ctx 'string #'#(*-chunk? regular-name)))
      (define v (syntax-e result))
      (check-true (vector? v))
      (check-eq? (syntax-e (vector-ref v 0)) 'string-chunk?)
      (check-eq? (syntax-e (vector-ref v 1)) 'regular-name))

    (test-case "box literal"
      (define result (star-expand #'ctx 'string #'#&*-chunk?))
      (check-eq? (syntax-e (unbox (syntax-e result))) 'string-chunk?))

    (test-case "prefab struct literal"
      (define result (star-expand #'ctx 'string #'#s(mystruct *-chunk? regular-name)))
      (define v (syntax-e result))
      (define as-vec (struct->vector v))
      (check-eq? (syntax-e (vector-ref as-vec 1)) 'string-chunk?)
      (check-eq? (syntax-e (vector-ref as-vec 2)) 'regular-name))

    (test-case "immutable hash table literal (values renamed, not keys)"
      ;; Unlike the other three shapes, there is no current production
      ;; code that puts a *-prefixed identifier inside a hash literal --
      ;; this specific traversal branch is otherwise completely
      ;; unexercised. If this fails with something other than a clear
      ;; check-eq? mismatch (e.g. a contract violation from syntax-parse
      ;; on a non-syntax value), that's itself useful information about
      ;; whether hash values actually arrive here syntax-wrapped.
      (define result (star-expand #'ctx 'string #'#hash((a . *-chunk?) (b . regular-name))))
      (define h (syntax-e result))
      (check-true (hash? h))
      (check-eq? (syntax-e (hash-ref h 'a)) 'string-chunk?)
      (check-eq? (syntax-e (hash-ref h 'b)) 'regular-name))

    (test-case "star-substitute replaces by table lookup, independent of the *-prefix convention"
      (define table (list (cons 'elem<? #'char<?)))
      (define result (star-substitute table #'(list elem<? other-name)))
      (define lst (syntax-e result))
      (check-eq? (syntax-e (car lst)) 'list)
      (check-eq? (syntax-e (cadr lst)) 'char<?)
      (check-eq? (syntax-e (caddr lst)) 'other-name))))

(run-suite! (test-suite "private internals"
                        alist-helpers-suite
                        validate-mask-suite
                        caller-suite
                        edit-distance-suite
                        star-traversal-suite))
