#lang racket/base

;;; Helpers for type variable references (τ.key).

(require racket/set
         racket/string
         racket/syntax
         "./edit-distance.rkt"
         "./signature.rkt")

(provide (all-defined-out))

(define (syntax-identifiers stx)
  (let loop ([x stx] [acc null])
    (define data (if (syntax? x) (syntax-e x) x))
    (cond
      [(symbol? data) (cons x acc)]
      [(pair? data) (loop (cdr data) (loop (car data) acc))]
      [(vector? data) (for/fold ([acc acc]) ([elem (in-vector data)]) (loop elem acc))]
      [(box? data) (loop (unbox data) acc)]
      [(prefab-struct-key data) (for/fold ([acc acc]) ([elem (in-vector (struct->vector data) 1)])
                                  (loop elem acc))]
      [(and (hash? data) (immutable? data)) (for/fold ([acc acc]) ([v (in-hash-values data)])
                                              (loop v acc))]
      [else acc])))

;; The right-hand side of a τ.key identifier, or #f.
(define (member-ref-key τ id)
  (define prefix (string-append (symbol->string (syntax-e τ)) "."))
  (define str (symbol->string (syntax-e id)))
  (and (string-prefix? str prefix)
       (> (string-length str) (string-length prefix))
       (string->symbol (substring str (string-length prefix)))))

;; An identifier τ.key in τ's lexical context
(define (member-ref-id τ key)
  (format-id τ "~a.~a" τ key #:source τ))

;; A list of key-identifier pairs for every τ.key in forms.
(define (collect-member-refs τ forms)
  (for*/list ([form (in-list forms)]
              [id (in-list (syntax-identifiers form))]
              [key (in-value (member-ref-key τ id))]
              #:when key)
    (cons key id)))

(define (did-you-mean key keys)
  (define near (nearest-name key (list->seteq keys) (map symbol->string keys)))
  (if (and near (not (eq? near key)))
      (format " (did you mean ~a?)" near)
      ""))

(define (check-member-refs who sig τ refs stx)
  (define keys (signature-keys sig))
  (for ([r (in-list refs)])
    (unless (signature-ref sig (car r))
      (raise-syntax-error who
        (format "unknown type member ~a~a" (syntax-e (cdr r)) (did-you-mean (car r) keys))
        stx (cdr r)))))
