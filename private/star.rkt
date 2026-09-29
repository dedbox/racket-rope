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

(define (string->id ctx str loc-stx prop-stx)
  (datum->syntax ctx (string->symbol str) loc-stx prop-stx))

(define (update-identifiers stx proc)
  (let loop ([stx stx])
    (syntax-parse stx
      [:id (proc stx)]
      [(head . tail)
       (datum->syntax stx (cons (loop #'head) (loop #'tail)) stx stx)]
      [#(v ...)
       (datum->syntax stx (apply vector (map loop (attribute v))) stx stx)]
      [#&v
       (datum->syntax stx (box (loop #'v)) stx stx)]
      [#s(key v ...)
       (define vs (map loop (attribute v)))
       (datum->syntax stx (make-prefab-struct (syntax-e #'key) vs) stx stx)]
      [_
       (define v (syntax-e stx))
       (if (and (hash? v) (immutable? v))
           (let ([new-hash
                  (for/hash ([(hk hv) (in-hash v)]) (values hk (loop hv)))])
             (datum->syntax stx new-hash stx stx))
           stx)])))

(define star-rx #px"\\*(?=-(rope|node|leaf|chunk|elem|cursor)\\b)")

(define (star-expand-string type-name str)
  (regexp-replace* star-rx str (symbol->string type-name)))

;; Traverses a syntax object and star-expands its identifiers.
(define (star-expand ctx type-name stx)
  (update-identifiers
   stx (λ (stx)
         (define str (symbol->string (syntax-e stx)))
         (if (regexp-match? star-rx str)
             (string->id ctx (star-expand-string type-name str) stx stx)
             stx))))

(define (star-substitute op-table stx)
  (update-identifiers
   stx (λ (stx)
         (define binding (assoc (syntax-e stx) op-table))
         (if binding (cdr binding) stx))))

;; (define (star-expand ctx type-name stx)
;;   (let loop ([stx stx])
;;     (syntax-parse stx
;;       [:id
;;        (define str (symbol->string (syntax-e stx)))
;;        (if (regexp-match? star-rx str)
;;            (string->id ctx (star-expand-string type-name str) stx stx)
;;            stx)]
;;       [(head . tail)
;;        (datum->syntax stx (cons (loop #'head) (loop #'tail)) stx stx)]
;;       [#(v ...)
;;        (datum->syntax stx (apply vector (map loop (attribute v))) stx stx)]
;;       [#&v
;;        (datum->syntax stx (box (loop #'v)) stx stx)]
;;       [#s(key v ...)
;;        (define vs (map loop (attribute v)))
;;        (datum->syntax stx (make-prefab-struct (syntax-e #'key) vs) stx stx)]
;;       [_
;;        (define v (syntax-e stx))
;;        (if (and (hash? v) (immutable? v))
;;            (let ([new-hash
;;                   (for/hash ([(hk hv) (in-hash v)]) (values hk (loop hv)))])
;;              (datum->syntax stx new-hash stx stx))
;;            stx)])))

;; (define (star-substitute op-table stx)
;;   (let loop ([stx stx])
;;     (syntax-parse stx
;;       [:id
;;        (define binding (assoc (syntax-e stx) op-table))
;;        (if binding (cdr binding) stx)]
;;       [(head . tail)
;;        (datum->syntax stx (cons (loop #'head) (loop #'tail)))]
;;       [#(v ...)
;;        (datum->syntax stx (apply vector (map loop (attribute v))) stx stx)]
;;       [#&v
;;        (datum->syntax stx (box (loop #'v)) stx stx)]
;;       [#s(key v ...)
;;        (define vs (map loop (attribute v)))
;;        (datum->syntax stx (make-prefab-struct (syntax-e #'key) vs) stx stx)]
;;       [_
;;        (define v (syntax-e stx))
;;        (if (and (hash? v) (immutable? v))
;;            (let ([new-hash
;;                   (for/hash ([(hk hv) (in-hash v)]) (values hk (loop hv)))])
;;              (datum->syntax stx new-hash stx stx))
;;            stx)])))
