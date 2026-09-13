#lang racket/base

(require rope2/generic/define)

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; Chunk Operations
;; -----------------------------------------------------------------------------

(define-type-op (rope-chunk? _ x) (*-chunk? x))
(define-type-op (rope-chunk-limit _) (*-chunk-limit))
(define-type-op (rope-chunk-empty _) (*-chunk-empty))
(define-type-op (rope-chunk-length _ c) (*-chunk-length c))
(define-type-op (rope-chunk-width _ c) (*-chunk-width c))
(define-type-op (rope-chunk-ref _ c i) (*-chunk-ref c i))
(define-type-op (rope-chunk-slice _ c i k) (*-chunk-slice c i k))
(define-type-op (rope-chunk-append _ cs) (apply *-chunk-append cs))

;; -----------------------------------------------------------------------------
;; Element Operations
;; -----------------------------------------------------------------------------

(define-type-op (rope-elem-width _ c i) (*-elem-width c i))

;; -----------------------------------------------------------------------------
;; Tree Construction
;; -----------------------------------------------------------------------------

(define-type-op (make-rope-leaf _ c₀)
  (let ([c c₀])
    (define-values (h p) (*-chunk-hash c))
    (*-make-leaf (*-chunk-length c) (*-chunk-width c) h p *-rope-content=? c)))

(define-type-op (make-rope-node _ l₀ r₀)
  (let ([l l₀] [r r₀])
    (define-values (h p) (*-node-hash l r))
    (make-node (+ (rope-length l) (rope-length r))
               (+ (rope-width l) (rope-width r))
               h p *-rope-content=?
               (add1 (max (rope-depth l) (rope-depth r)))
               l r)))

(define-type-op (make-empty-rope τ) (make-rope-leaf τ (*-chunk-empty)))

(define-type-op (rope-hash _ a₀)
  (let ([a a₀])
    (if (rope-leaf? a)
        (*-chunk-hash (rope-leaf-chunk a))
        (*-node-hash (rope-node-left a) (rope-node-right a)))))
