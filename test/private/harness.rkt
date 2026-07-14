#lang racket/base

;; Shared test infrastructure: random generators, a synthetic "weighted" rope type used only to
;; exercise the count≠width path of the generic layer (neither shipped instance can reach it,
;; since both set raw-width ≡ raw-length), and a small declarative property macro.

(require (for-syntax racket/base)
         racket/vector
         rackunit
         syntax/parse/define
         rope/rope)

(provide (all-defined-out))

;;; ---------------------------------------------------------------------------------------------
;;; Synthetic weighted raw type: a vector of naturals ≥ 1, where element i's "width" is its own
;;; value. raw-length (element count) and raw-width (Σ values) therefore diverge.
;;; ---------------------------------------------------------------------------------------------

(define WEIGHTED-LEAF-LIMIT 8)

(define (weighted-raw-width v)
  (for/sum ([w (in-vector v)]) w))

(define weighted-base-ops
  (rope-ops WEIGHTED-LEAF-LIMIT
            (λ () (vector))
            vector-length
            weighted-raw-width
            vector-copy
            vector-append
            vector-ref))

;; The base structs double perfectly well as their own "instance" — no subtyping needed to probe
;; the generic algorithms directly.
(define weighted-ops
  (rope-ops-impl (rope-ops-limit         weighted-base-ops)
                 (rope-ops-raw-empty     weighted-base-ops)
                 (rope-ops-raw-length    weighted-base-ops)
                 (rope-ops-raw-width     weighted-base-ops)
                 (rope-ops-raw-slice     weighted-base-ops)
                 (rope-ops-raw-append    weighted-base-ops)
                 (rope-ops-raw-ref       weighted-base-ops)
                 rope-leaf
                 rope-node))

(define (random-weight)              (add1 (random 4)))       ; widths in [1,4]
(define (random-weighted-raw n)      (build-vector n (λ (_) (random-weight))))
(define (weighted->vec r)            (apply (rope-ops-raw-append weighted-ops) (rope-flatten r)))

;;; ---------------------------------------------------------------------------------------------
;;; General-purpose random content, reused by the string/bytes suites.
;;; ---------------------------------------------------------------------------------------------

(define (random-string n)
  (list->string (for/list ([_ (in-range n)]) (integer->char (+ 32 (random 95))))))

;; Includes codepoints outside the BMP (astral plane) to stress UTF-8 multi-byte encoding, while
;; skipping the surrogate range, which is not a valid Racket char.
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
;;; A tiny declarative macro: run `body` (which must evaluate to a boolean) `trials` times against
;;; fresh random bindings, reporting the failing sample via rackunit's check-info machinery.
;;; ---------------------------------------------------------------------------------------------

(define-syntax (check-property stx)
  (syntax-parse stx
    [(_ #:trials trials:expr ([id:id gen:expr] ...) body:expr ...+)
     #`(for ([iteration (in-range trials)])
         (let* ([id gen] ...)
           (with-check-info (['iteration iteration] ['id id] ...)
             #,(syntax/loc stx (check-true (let () body ...))))))]))
