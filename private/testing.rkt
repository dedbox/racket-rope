#lang racket/base
;; private/testing.rkt: Shared test-only infrastructure for this package's
;; `module+ test` submodules. Not part of the public API.

(require (for-syntax racket/base
                     syntax/parse)
         racket/vector
         rackunit
         rackunit/text-ui
         rope/rope)

(provide (all-defined-out))

;; `run-tests` only prints - it never signals failure to the process, so a
;; failing suite would otherwise report as a silent pass under `raco test`.
(define (run-suite! suite)
  (define failed (run-tests suite 'verbose))
  (unless (zero? failed)
    (error 'raco-test "~a check(s) failed" failed)))

;;; --------------------------------------------------------------------------
;;; Synthetic "weighted" raw type: a vector of naturals ≥ 1, where element i's
;;; width is its own value. raw-count and raw-width diverge here, exercising a
;;; path neither shipped instance can reach (for string/bytes, every element
;;; has width 1). Manually defined instead of building with define-rope-type,
;;; so rope.rkt's own generic algorithms can be tested independently of the
;;; macro's correctness.
;;; --------------------------------------------------------------------------

(define WEIGHTED-LEAF-LIMIT 8)

(define (weighted-raw-width v) (for/sum ([w (in-vector v)]) w))

(define (weighted-raw-compare a b)
  (let loop ([i 0])
    (cond [(and (= i (vector-length a)) (= i (vector-length b))) '=]
          [(= i (vector-length a))                               '<]
          [(= i (vector-length b))                               '>]
          [(< (vector-ref a i) (vector-ref b i))                 '<]
          [(> (vector-ref a i) (vector-ref b i))                 '>]
          [else (loop (add1 i))])))

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
   (define (raw-compare _ a b)    (weighted-raw-compare a b))
   ;; Untagged base constructors: this witness exists to test rope.rkt's own
   ;; generic algorithms.
   (define (rope-leaf-ctor _)     rope-leaf)
   (define (rope-node-ctor _)     rope-node)])

(define weighted (weighted-gen))        ; the dispatch witness

(define (random-weight)         (add1 (random 4))) ; widths in the interval [1,4]
(define (random-weighted-raw n) (build-vector n (λ (_) (random-weight))))
(define (weighted->vec r)       (rope->raw weighted r))

;;; --------------------------------------------------------------------------
;;; General-purpose random content, reused by the string/bytes suites.
;;; --------------------------------------------------------------------------

(define (random-string n)
  (list->string (for/list ([_ (in-range n)]) (integer->char (+ 32 (random 95))))))

;; Includes codepoints outside the BMP (astral plane), skipping the surrogate
;; range.
(define (random-unicode-string n)
  (list->string
   (for/list ([_ (in-range n)])
     (let loop ()
       (define c (+ 32 (random #x1F900)))
       (if (<= #xD800 c #xDFFF) (loop) (integer->char c))))))

(define (random-bytes n)
  (apply bytes (build-list n (λ (_) (random 256)))))

;;; --------------------------------------------------------------------------
;;; Property testing: run `body` (boolean-valued) `trials` times against fresh
;;; random bindings, wrapped as a single named test-case so it composes into
;;; test-suite hierarchies like any other check. Failing samples are reported
;;; via rackunit's check-info machinery.
;;; --------------------------------------------------------------------------

(define-syntax (test-property stx)
  (syntax-parse stx
    [(_ name:expr #:trials trials:expr ([id:id gen-expr:expr] ...) body:expr ...+)
     #`(test-case name
         (for ([iteration (in-range trials)])
           (let* ([id gen-expr] ...)
             (with-check-info (['iteration iteration] ['id id] ...)
               #,(syntax/loc stx
                   (check-true (let () body ...)))))))]))
