#lang racket/base

(require (for-syntax racket/base
                     rope2/stxclasses)
         rope2/class
         rope2/cursor
         rope2/rope
         rope2/stxclasses
         syntax/parse/define)

(provide (all-defined-out))

(define-rope-class total-order
  ([elem<? id+fun2]
   [elem>? id+fun2]
   [chunk-compare-overlap id+fun5 #:optional])

  (define (*-elem<? x y) (elem<? x y))
  (define (*-elem>? x y) (elem>? x y))

  (define *-chunk-compare-overlap
    (~? chunk-compare-overlap
        (λ (c d ic id k)
          (let loop ([i 0])
            (cond [(= i k) '=]
                  [(*-elem<? (*-chunk-ref c (+ ic i) (*-chunk-ref d (+id i)))) '<]
                  [(*-elem>? (*-chunk-ref c (+ ic i) (*-chunk-ref d (+id i)))) '>]
                  [else (loop (add1 i))])))))

  (define-class-op (rope-compare-with _ _ f:id+fun5 a b)
    (let loop ([cur-a (rope->mutable-cursor a)]
               [cur-b (rope->mutable-cursor b)])
      (cond
        [(and (not cur-a) (not cur-b)) '=]
        [(not cur-a) '<]
        [(not cur-b) '>]
        [else
         (define la (mutable-cursor-leaf cur-a))
         (define lb (mutable-cursor-leaf cur-b))
         (define pa (mutable-cursor-rel-idx cur-a))
         (define pb (mutable-cursor-rel-idx cur-b))
         (define k (min (- (rope-length la) pa) (- (rope-length lb) pb)))
         (define result (*-chunk-compare-overlap (rope-leaf-chunk la) (rope-leaf-chunk lb) pa pb k))
         (if (not (eq? result '=))
             result
             (loop (cursor-advance! cur-a k) (cursor-advance! cur-b k)))])))

  (define-class-op (rope-compare τ κ a b) (rope-compare-with τ κ chunk-compare-overlap a b))
  (define-class-op (rope<? τ κ a b) (eq? (rope-compare τ κ a b) '<))
  (define-class-op (rope>? τ κ a b) (eq? (rope-compare τ κ a b) '>))
  (define-class-op (rope<=? τ κ a b) (or (rope=? τ a b) (rope<? τ κ a b)))
  (define-class-op (rope>=? τ κ a b) (or (rope=? τ a b) (rope>? τ κ a b)))

  (define (*-rope-compare-with f a b) (rope-compare-with type-id class-id f a b))
  (define (*-rope-compare a b) (rope-compare type-id class-id a b))
  (define (*-rope<? a b) (rope<? type-id class-id a b))
  (define (*-rope>? a b) (rope>? type-id class-id a b))
  (define (*-rope<=? a b) (rope<=? type-id class-id a b))
  (define (*-rope>=? a b) (rope>=? type-id class-id a b)))
