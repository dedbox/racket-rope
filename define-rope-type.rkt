#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/splicing
         rope/rope
         syntax/parse/define)

(provide define-rope-type)

(define-simple-macro (define-rope-type type:id
                       raw?-expr:expr
                       raw-limit-expr:expr
                       raw-empty-expr:expr
                       raw-count-expr:expr
                       raw-width-expr:expr
                       raw-slice-expr:expr
                       raw-append-expr:expr
                       raw-ref-expr:expr)
  #:do [(define (type-id  fmt) (format-id #'type fmt (syntax-e #'type)))
        (define (type-id2 fmt) (format-id #'type fmt (syntax-e #'type) (syntax-e #'type)))]
  #:with *-rope              (type-id  "~a-rope")
  #:with *-raw?              (type-id  "~a-raw?")
  #:with *-raw-limit         (type-id  "~a-raw-limit")
  #:with *-raw-empty         (type-id  "~a-raw-empty")
  #:with *-raw-count         (type-id  "~a-raw-count")
  #:with *-raw-width         (type-id  "~a-raw-width")
  #:with *-raw-slice         (type-id  "~a-raw-slice")
  #:with *-raw-append        (type-id  "~a-raw-append")
  #:with *-raw-ref           (type-id  "~a-raw-ref")
  #:with make-*-rope-leaf    (type-id  "make-~a-rope-leaf")
  #:with make-empty-*-rope   (type-id  "make-empty-~a-rope")
  #:with *-rope-append1      (type-id  "~a-rope-append1")
  #:with *-rope-append       (type-id  "~a-rope-append")
  #:with *-rope-offset-index (type-id  "~a-rope-offset-index")
  #:with *-raw->*-rope       (type-id2 "~a-raw->~a-rope")
  #:with *-rope->*-raw       (type-id2 "~a-rope->~a-raw")
  #:with *-cursor-at-end?    (type-id  "~a-cursor-at-end?")
  #:with *-cursor-peek       (type-id  "~a-cursor-peek")
  #:with *-cursor-advance    (type-id  "~a-cursor-advance")
  #:with *-cursor-drop       (type-id  "~a-cursor-drop")
  #:with *-rope->cursor      (type-id  "~a-rope->cursor")
  #:with cursor->*-rope      (type-id  "cursor->~a-rope")
  #:with *-rope-foldl        (type-id  "~a-rope-foldl")
  #:with *-rope-foldr        (type-id  "~a-rope-foldr")
  #:with in-*-rope-runtime   (type-id  "in-~a-rope-runtime")
  #:with in-*-rope           (type-id  "in-~a-rope")
  (begin
    (struct *-rope rope () #:transparent
      #:methods gen:ropeable
      [(define (raw?       _ obj)   (raw?-expr obj))
       (define (raw-limit  _)       raw-limit-expr)
       (define (raw-empty  _)       raw-empty-expr)
       (define (raw-count  _ r)     (raw-count-expr r))
       (define (raw-width  _ r)     (raw-width-expr r))
       (define (raw-slice  _ r p l) (raw-slice-expr r p l))
       (define (raw-append _ . rs)  (raw-append-expr rs))
       (define (raw-ref    _ r p)   (raw-ref-expr r p))])

    ;; A dummy instance provides the required internal raw methods
    (splicing-let ([gen (*-rope)])
      (define (*-raw? obj)                      (raw?              gen obj))
      (define (*-raw-limit)                     (raw-limit         gen))
      (define (*-raw-empty)                     (raw-empty         gen))
      (define (*-raw-count         raw)         (raw-count         gen raw))
      (define (*-raw-width         raw)         (raw-width         gen raw))
      (define (*-raw-slice         raw pos end) (raw-slice         gen raw pos end))
      (define (*-raw-ref           raw pos)     (raw-ref           gen raw pos))
      (define (*-raw-append    .   raws)        (apply raw-append  gen raws))
      (define (make-*-rope-leaf    raw)         (make-rope-leaf    gen raw))
      (define (make-empty-*-rope)               (make-empty-rope   gen))
      (define (*-rope-append1      left right)  (rope-append1      gen left right))
      (define (*-rope-append   .   ropes)       (apply rope-append gen ropes))
      (define (*-rope-offset-index rope pos)    (rope-offset-index gen rope pos))
      (define (*-raw->*-rope       raw)         (raw->rope         gen raw))
      (define (*-rope->*-raw       rope)        (rope->raw         gen rope))
      (define (*-cursor-at-end?    cur)         (cursor-at-end?    gen cur))
      (define (*-cursor-peek       cur)         (cursor-peek       gen cur))
      (define (*-cursor-advance    cur)         (cursor-advance    gen cur))
      (define (*-cursor-drop       cur k)       (cursor-drop       gen cur k))
      (define (*-rope->cursor      rope)        (rope->cursor      gen rope))
      (define (cursor->*-rope      cur)         (cursor->rope      gen cur))
      (define (*-rope-foldl proc init rope0 . ropes) (apply rope-foldl gen proc init rope0 ropes))
      (define (*-rope-foldr proc init rope0 . ropes) (apply rope-foldr gen proc init rope0 ropes))

      ;; Evaluated when `in-*-rope` is used as a first-class value outside of a `for` loop (e.g.,
      ;; passed to standard higher-order functions like `sequence-map`).
      (define (in-*-rope-runtime rope)
        ;; (unless (*-rope? r)
        ;;   (raise-argument-error 'in-*-rope (format "~a-rope?" 'name) r))
        (make-do-sequence
         (λ ()
           (values *-cursor-peek
                   *-cursor-advance
                   (*-rope->cursor rope)
                   (λ (cur) (not (*-cursor-at-end? cur)))
                   #f
                   #f))))

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
                 ([(rope) rope-expr])
                 ;; Outer check (fails early if the provided value isn't a rope
                 ;; (unless (*-rope? rope)
                 ;;   (raise-argument-error 'in-*-rope (format "~a-rope?" 'name) rope))
                 (begin)
                 ;; Loop bindings (state initialization)
                 ([cur (*-rope->cursor rope)])
                 ;; Position guard (condition to continue iterating)
                 (not (*-cursor-at-end? cur))
                 ;; Inner bindings (extracting the current element)
                 ([(id) (*-cursor-peek cur)])
                 ;; Pre-guard (optional condition before body evaluation)
                 #t
                 ;; Post-guard (optional condition after body evaluation)
                 #t
                 ;; Loop arguments (state transition for the next iteration)
                 ((*-cursor-advance cur)))]]))))))
