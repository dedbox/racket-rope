#lang racket/base
;; Shared test infrastructure. The synthetic "weighted" type is a hand-rolled gen:ropeable instance
;; (not built through define-rope-type), which keeps rope.rkt's own generics testable independently
;; of the macro's correctness. It now must supply rope-leaf-ctor/rope-node-ctor too, since both are
;; in gen:ropeable's #:requires list with no #:fallbacks default.

(require (for-syntax racket/base
                     syntax/parse)
         racket/generic
         racket/vector
         rackunit
         rope/rope)

(provide (all-defined-out))

;;; ---------------------------------------------------------------------------------------------
;;; Synthetic weighted raw type: a vector of naturals ≥ 1, where element i's "width" is its own
;;; value, so raw-count and raw-width diverge — exercising a path neither shipped instance can
;;; reach, since both set raw-width ≡ raw-count.
;;; ---------------------------------------------------------------------------------------------

(define WEIGHTED-LEAF-LIMIT 8)

(define (weighted-raw-width v)
  (for/sum ([w (in-vector v)]) w))

(struct weighted-gen ()
  #:methods gen:ropeable
  [(define (raw?       _ obj)     (vector? obj))
   (define (raw-limit  _)         WEIGHTED-LEAF-LIMIT)
   (define (raw-empty  _)         (vector))
   (define (raw-count  _ raw)     (vector-length raw))
   (define (raw-width  _ raw)     (weighted-raw-width raw))
   (define (raw-slice  _ raw s e) (vector-copy raw s e))
   (define (raw-append _ . raws)  (apply vector-append raws))
   (define (raw-ref    _ raw i)   (vector-ref raw i))
   ;; Untagged base constructors are fine here — this harness exists to test rope.rkt's own
   ;; generic algorithms, not per-type tagging (that's define-rope-type-test.rkt's job).
   (define (rope-leaf-ctor _)     rope-leaf)
   (define (rope-node-ctor _)     rope-node)])

(define gen (weighted-gen))                             ; the dispatch witness used throughout

(define (random-weight)         (add1 (random 4)))      ; widths in [1,4]
(define (random-weighted-raw n) (build-vector n (λ (_) (random-weight))))
(define (weighted->vec r)       (rope->raw gen r))

;;; ---------------------------------------------------------------------------------------------
;;; General-purpose random content, reused by the string/bytes suites.
;;; ---------------------------------------------------------------------------------------------

(define (random-string n)
  (list->string (for/list ([_ (in-range n)]) (integer->char (+ 32 (random 95))))))

;; Includes codepoints outside the BMP (astral plane), skipping the surrogate range.
(define (random-unicode-string n)
  (list->string
   (for/list ([_ (in-range n)])
     (let loop ()
       (define c (+ 32 (random #x1F900)))
       (if (<= #xD800 c #xDFFF) (loop) (integer->char c))))))

(define (build-bytes n proc)
  (apply bytes (build-list n proc)))

(define (random-bytes n)
  (build-bytes n (λ (_) (random 256))))

;;; ---------------------------------------------------------------------------------------------
;;; A tiny declarative macro: run `body` (must evaluate to a boolean) `trials` times against fresh
;;; random bindings, reporting the failing sample via rackunit's check-info machinery.
;;; ---------------------------------------------------------------------------------------------

(define-syntax (check-property stx)
  (syntax-parse stx
    [(_ #:trials trials:expr ([id:id gen-expr:expr] ...) body:expr ...+)
     #`(for ([iteration (in-range trials)])
         (let* ([id gen-expr] ...)
           (with-check-info (['iteration iteration] ['id id] ...)
             #,(syntax/loc stx
                 (check-true (let () body ...))))))]))
