#lang racket/base

(require rope2/generic/define
         rope2/rope)

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
    (*-make-node (+ (rope-length l) (rope-length r))
                 (+ (rope-width l) (rope-width r))
                 h p *-rope-content=?
                 (add1 (max (rope-depth l) (rope-depth r)))
                 l r)))

(define-type-op (make-empty-rope τ) (make-rope-leaf τ (*-chunk-empty)))

;; -----------------------------------------------------------------------------
;; Hashing
;; -----------------------------------------------------------------------------

(define-type-op (rope-hash _ a₀)
  (let ([a a₀])
    (if (rope-leaf? a)
        (*-chunk-hash (rope-leaf-chunk a))
        (*-node-hash (rope-node-left a) (rope-node-right a)))))

;; -----------------------------------------------------------------------------
;; Conversions
;; -----------------------------------------------------------------------------

(define-type-op (chunk->rope τ c₀)
  (let* ([c c₀] [total (*-chunk-length c)])
    (if (<= total (*-chunk-limit))
        (make-rope-leaf τ c)
        (let loop ([i 0] [k total])
          (if (<= k (*-chunk-limit))
              (make-rope-leaf τ (*-chunk-slice c i k))
              (let ([mid (quotient k 2)])
                (define l (loop i mid))
                (define r (loop (+ i mid) (- k mid)))
                (rope-concat τ l r)))))))

(define-type-op (rope->chunk _ a) (*-chunk-append (rope-chunks a)))

;; -----------------------------------------------------------------------------
;; Basic Operations
;; -----------------------------------------------------------------------------

;; O(1)
(define-type-op (rope-concat τ l₀ r₀)
  (let ([l l₀] [r r₀])
    (cond
      [(zero? (rope-length l)) r]
      [(zero? (rope-length r)) l]
      [else (make-rope-node τ l r)])))

;; O(log n) amortized
(define-type-op (rope-append2 τ a b) (rope-ensure-balance τ (rope-concat τ a b)))

;; O(log n) amortized
(define-type-op (rope-append ρ as)
  (rope-ensure-balance ρ
    (for/fold ([l (make-empty-rope ρ)])
              ([r (in-list as)])
      (rope-concat ρ l r))))

;; -----------------------------------------------------------------------------
;; Tree Balancing
;; -----------------------------------------------------------------------------

;; (define-rope-operation (rope-defrag τ a) (chunk->rope τ (rope->chunk τ a)))

(define-type-op (rope-ensure-balance τ a)
  (if (rope-mostly-balanced? a) a (rope-rebalance τ a)))

;; Efficient forest-based rope rebuild. O(log n) amortized
(define-type-op (rope-rebalance τ a₀)
  (let ([a a₀])
    (define slots (make-vector (add1 max-fib-index) #f))

    (define (target-slot len)
      ;; A rope with len elements is too large for the current slot if len
      ;; falls beyond the interval [Fₙ, Fₙ₊₁). Thus, we move on to the next
      ;; slot if len ≥ Fₙ₊₁.
      ;;
      ;; Since i = n - 2 ⇒ n = i + 2 (see the comment in rope.rkt), we have
      ;;
      ;;    len ≥ F₍ᵢ₊₂₎₊₁ = Fᵢ₊₃.
      ;;
      (let loop ([i 0])
        (if (>= len (fib-bound (+ i 3))) (loop (add1 i)) i)))

    (define (insert! a)
      (define n (target-slot (rope-length a)))
      ;; Consolidate any occupied slots [0, n) into one prefix, oldest-first.
      (define pfx
        (for/fold ([pfx #f]) ([i (in-range n)])
          (define cur (vector-ref slots i))
          (when cur (vector-set! slots i #f))
          (cond [(not cur) pfx]
                [(not pfx) cur]
                [else (rope-concat τ cur pfx)])))
      (define r (if pfx (rope-concat τ pfx a) a))
      ;; Cascade upward from slot n.
      (let cascade ([i n] [cur r])
        (define next (vector-ref slots i))
        (if next
            (begin (vector-set! slots i #f)
                   (cascade (add1 i) (rope-concat τ next cur)))
            (vector-set! slots i cur))))

    (define (traverse a)
      (if (or (rope-leaf? a) (rope-strictly-balanced? a)) ; must use strict balance here
          (insert! a)
          (begin (traverse (rope-node-left a))
                 (traverse (rope-node-right a)))))

    ;; Concatenate on the left, from smallest to largest slot.
    (define (collapse)
      (for/fold ([result #f]) ([i (in-range max-fib-index)])
        (define slot-i (vector-ref slots i))
        (cond
          [(not slot-i) result]
          [(not result) slot-i]
          [else (rope-concat τ slot-i result)])))

    (if (rope-mostly-balanced? a)
        a
        (begin (traverse a) (collapse)))))
