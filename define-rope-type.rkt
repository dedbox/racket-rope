#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/sequence
         racket/splicing
         racket/struct
         rope/cursor
         rope/rope
         syntax/parse/define)

(provide (all-defined-out))

(define-simple-macro (define-rope-type name:id ops-expr:expr)
  #:do [(define (format-*id fmt) (format-id #'name fmt (syntax-e #'name)))]
  #:with *-rope-leaf         (format-*id "~a-rope-leaf")
  #:with *-rope-node         (format-*id "~a-rope-node")
  #:with *-rope?             (format-*id "~a-rope?")
  #:with *-rope-leaf?        (format-*id "~a-rope-leaf?")
  #:with *-rope-node?        (format-*id "~a-rope-node?")
  #:with empty-*-rope        (format-*id "empty-~a-rope")
  #:with *-rope-append       (format-*id "~a-rope-append")
  #:with *-rope-append*      (format-*id "~a-rope-append*")
  #:with *-rope-split        (format-*id "~a-rope-split")
  #:with *-rope-offset-index (format-*id "~a-rope-offset-index")
  #:with *-rope-splice       (format-*id "~a-rope-splice")
  #:with *-rope-slice        (format-*id "~a-rope-slice")
  #:with *->rope             (format-*id "~a->rope")
  #:with rope->*             (format-*id "rope->~a")
  #:with *-rope->cursor      (format-*id "~a-rope->cursor")
  #:with cursor->*-rope      (format-*id "cursor->~a-rope")
  #:with *-cursor-at-end?    (format-*id "~a-cursor-at-end?")
  #:with *-cursor-peek       (format-*id "~a-cursor-peek")
  #:with *-cursor-advance    (format-*id "~a-cursor-advance")
  #:with *-cursor-drop       (format-*id "~a-cursor-drop")
  #:with in-*-rope-runtime   (format-*id "in-~a-rope-runtime")
  #:with in-*-rope           (format-*id "in-~a-rope")
  (begin
    (struct *-rope-leaf rope-leaf () #:transparent)
    (struct *-rope-node rope-node () #:transparent)

    (define (*-rope? x)
      (or (*-rope-leaf? x) (*-rope-node? x)))

    (splicing-let ([ops (let ([fields (struct->list ops-expr)]
                              [ctors  (list *-rope-leaf *-rope-node)])
                          (apply rope-ops-impl (append fields ctors)))])
      (define empty-*-rope                  (make-empty-rope   ops))
      (define (*-rope-append       l r)     (rope-append       ops l r))
      (define (*-rope-append*      rs)      (rope-append*      ops rs))
      (define (*-rope-split        r i)     (rope-split        ops r i))
      (define (*-rope-offset-index r o)     (rope-offset-index ops r o))
      (define (*-rope-splice       r s o n) (rope-splice       ops r s o n))
      (define (*-rope-slice        r s l)   (rope-slice        ops r s l))
      (define (*->rope             r)       (raw->rope         ops r))
      (define (rope->*             r)       (rope->raw         ops r))
      (define (*-rope->cursor      r)       (rope->cursor      ops r))
      (define (cursor->*-rope      c)       (cursor->rope      ops c))
      (define (*-cursor-at-end?    c)       (cursor-at-end?    ops c))
      (define (*-cursor-peek       c)       (cursor-peek       ops c))
      (define (*-cursor-advance    c)       (cursor-advance    ops c))
      (define (*-cursor-drop       c k)     (cursor-drop       ops c k))

      ;; Evaluated when `in-*-rope` is used as a first-class value outside of a `for` loop (e.g.,
      ;; passed to standard higher-order functions like `sequence-map`).
      (define (in-*-rope-runtime r)
        (unless (*-rope? r)
          (raise-argument-error 'in-*-rope (format "~a-rope?" 'name) r))
        (make-do-sequence
         (λ ()
           (values *-cursor-peek
                   *-cursor-advance
                   (*-rope->cursor r)
                   (λ (c) (not (*-cursor-at-end? c)))
                   (λ (v) #t)
                   (λ (c v) #t)))))

      ;; Evaluated when `in-*-rope` is used directly in a `for` comprehension clause. Expands into a
      ;; specialized `:do-in` form that Racket optimizes heavily.
      (define-sequence-syntax in-*-rope
        (λ () #'in-*-rope-runtime)
        (λ (stx)
          (syntax-parse stx
            [[(id:id) (_ rope-expr:expr)]
             #'[(id)
                (:do-in
                 ;; Outer bindings (setup before the loop starts)
                 ([(r) rope-expr])

                 ;; Outer check (fails early if the provided value isn't a rope
                 (unless (*-rope? r)
                   (raise-argument-error 'in-*-rope (format "~a-rope?" 'name) r))

                 ;; Loop bindings (state initialization)
                 ([c (*-rope->cursor r)])

                 ;; Position guard (condition to continue iterating)
                 (not (*-cursor-at-end? c))

                 ;; Inner bindings (extracting the current element)
                 ([(id) (*-cursor-peek c)])

                 ;; Pre-guard (optional condition before body evaluation)
                 #t

                 ;; Post-guard (optional condition after body evaluation)
                 #t

                 ;; Loop arguments (state transition for the next iteration)
                 ((*-cursor-advance c)))]]))))))
