#lang racket/base

;; Star Identifier Expansion
;;
;; A type-generic identifier is an identifier whose name contains one or more
;; phrases of the form `*-<kind>`. Star expansion replaces the asterisks with
;; a type name while preserving lexical context, source location, and syntax
;; properties.

(require racket/syntax
         syntax/parse)

(provide (all-defined-out))

(define star-rx #px"\\*(?=-(rope|node|leaf|chunk|elem|cursor)\\b)")

(define (string->id ctx str loc-stx prop-stx)
  (datum->syntax ctx (string->symbol str) loc-stx prop-stx))

(define (resyntax stx x) (datum->syntax stx x stx stx))

(define (update-identifiers stx proc)
  (let loop ([stx stx])
    (syntax-parse stx
      [:id (proc stx)]
      [(head . tail) (resyntax stx (cons (loop #'head) (loop #'tail)))]
      [#(v ...) (resyntax stx (apply vector (map loop (attribute v))))]
      [#&v (resyntax stx (box (loop #'v)))]
      [#s(key v ...)
       (define vs (map loop (attribute v)))
       (resyntax stx (make-prefab-struct (syntax-e #'key) vs))]
      [_
       (define v (syntax-e stx))
       (if (and (hash? v) (immutable? v))
           (resyntax stx (for/hash ([(hk hv) (in-hash v)])
                           (values hk (loop hv))))
           stx)])))

(define (star-expand-string type-name str)
  (regexp-replace* star-rx str (symbol->string type-name)))

;; Traverses a syntax object and star-expands its identifiers.
(define (star-expand ctx type-name stx₀)
  (define (updater stx)
    (define str (symbol->string (syntax-e stx)))
    (if (regexp-match? star-rx str)
        (string->id ctx (star-expand-string type-name str) stx stx)
        stx))
  (update-identifiers stx₀ updater))

(define (star-substitute op-table stx₀)
  (define (updater stx)
    (define binding (assoc (syntax-e stx) op-table))
    (if binding (cdr binding) stx))
  (update-identifiers stx₀ updater))
