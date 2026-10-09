#lang racket/base

;; Star Identifier Expansion
;;
;; A type-generic identifier is an identifier whose name contains one or more
;; phrases of the form `*-<kind>`. Star expansion replaces the asterisks with
;; a type name while preserving lexical context, source location, and syntax
;; properties.

(require racket/struct
         racket/syntax
         syntax/parse)

(provide (all-defined-out))

(define star-rx #px"\\*(?=-(rope|node|leaf|chunk|elem|cursor)\\b)")

(define (string->id ctx str loc-stx prop-stx)
  (datum->syntax ctx (string->symbol str) loc-stx prop-stx))

(define (resyntax stx x) (datum->syntax stx x stx stx))

(define (update-identifiers stx proc)
  (let loop ([stx stx])
    (define data (syntax-e stx))
    (cond
      [(symbol? data) (proc stx)]
      [(pair? data)
       (syntax-parse stx
         [(_ . tail) (resyntax stx (cons (loop (car data)) (loop (attribute tail))))])]
      [(vector? data) (resyntax stx (apply vector (map loop (vector->list data))))]
      [(box? data) (resyntax stx (box (loop (unbox data))))]
      [(struct? data)
       (define vs (map loop (struct->list data)))
       (resyntax stx (apply make-prefab-struct (prefab-struct-key data) vs))]
      [else
       (if (and (hash? data) (immutable? data))
           (resyntax stx (for/hash ([(k v) (in-hash data)]) (values k (loop v))))
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
