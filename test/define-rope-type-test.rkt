#lang racket/base

(module+ test
  (require rackunit
           racket/sequence
           racket/vector
           rope/rope
           rope/define-rope-type)

  ;; A minimal instance: raw = vector of symbols, width ≡ length (like the shipped instances).
  (define SYM-LIMIT 4)

  (define-rope-type sym
    (rope-ops SYM-LIMIT (λ () (vector)) vector-length vector-length
              (λ (v s e) (vector-copy v s e)) (λ (vs) (apply vector-append vs)) vector-ref))

  ;; Macro-safety / hygiene probe: name the type `ops`, which is textually identical to the
  ;; identifier the macro's own `splicing-let` introduces internally. If hygiene ever regressed
  ;; (e.g. via a stray `datum->syntax` with the wrong context), this would be the first thing to
  ;; break.
  (define-rope-type ops
    (rope-ops SYM-LIMIT (λ () (vector)) vector-length vector-length
              (λ (v s e) (vector-copy v s e)) (λ (vs) (apply vector-append vs)) vector-ref))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Identifier surface: every promised binding exists and behaves.
  ;;; -------------------------------------------------------------------------------------------
  (test-case "core identifiers are all bound and minimally functional"
    (check-true  (sym-rope? empty-sym-rope))
    (check-equal? (rope-count empty-sym-rope) 0)
    (check-equal? (sym->rope (vector 'a 'b 'c)) (sym->rope (vector 'a 'b 'c))))

  (test-case "generated conversions round-trip"
    (define v (vector 'a 'b 'c 'd 'e 'f 'g))
    (define r (sym->rope v))
    (check-equal? (rope->sym r) v)
    (check-equal? (rope-count r) (vector-length v)))

  (test-case "append / split / splice / slice via the generated API"
    (define v1 (vector 'a 'b 'c))
    (define v2 (vector 'd 'e))
    (define r (sym-rope-append1 (sym->rope v1) (sym->rope v2)))
    (check-equal? (rope->sym r) (vector-append v1 v2))
    (define-values (l rr) (sym-rope-split r 3))
    (check-equal? (rope->sym l) v1)
    (check-equal? (rope->sym rr) v2)
    (check-equal? (rope->sym (sym-rope-splice r 1 3 (vector 'x))) (vector 'a 'x 'e))
    (check-equal? (rope->sym (sym-rope-slice r 1 3)) (vector 'b 'c 'd)))

  (test-case "cursor identifiers"
    (define r (sym->rope (vector 'a 'b 'c)))
    (define c (sym-rope->cursor r))
    (check-false (sym-cursor-at-end? c))
    (check-equal? (sym-cursor-peek c) 'a)
    (check-equal? (sym-cursor-peek (sym-cursor-advance c)) 'b)
    (check-equal? (rope->sym (cursor->sym-rope (sym-cursor-drop c 2))) (vector 'c)))

  (test-case "fold identifiers"
    (define r (sym->rope (vector 1 2 3)))
    (check-equal? (sym-rope-foldl (λ (acc x) (cons x acc)) '() r) '(3 2 1)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Sequencing: both the compile-time optimized clause and the runtime fallback agree.
  ;;; -------------------------------------------------------------------------------------------
  (test-case "in-sym-rope: for-loop and first-class sequence use agree"
    (define r (sym->rope (vector 'a 'b 'c 'd 'e)))
    (define via-for  (for/list ([x (in-sym-rope r)]) x))
    (define via-seq  (sequence->list (in-sym-rope-runtime r)))
    (check-equal? via-for (vector->list (rope->sym r)))
    (check-equal? via-seq via-for))

  (test-case "in-sym-rope-runtime rejects non-ropes"
    (check-exn exn:fail:contract? (λ () (sequence->list (in-sym-rope-runtime 5)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Cross-type discrimination: subtyped leaves/nodes are not confused with each other or with
  ;;; the un-subtyped base structs.
  ;;; -------------------------------------------------------------------------------------------
  (test-case "instance predicates do not cross-recognize other instances"
    (define sym-r (sym->rope (vector 'a 'b)))
    (define ops-r (ops->rope (vector 'a 'b)))          ; the `ops`-named instance
    (check-true  (sym-rope? sym-r))
    (check-false (ops-rope? sym-r))
    (check-true  (ops-rope? ops-r))
    (check-false (sym-rope? ops-r)))

  ;; (test-case "a bare, un-subtyped rope-leaf is not an instance rope"
  ;;   (define plain (make-rope-leaf sym-rope-ops (vector 'a)))
  ;;   (check-false (sym-rope? plain)))

  )
