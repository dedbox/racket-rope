#lang racket/base

(require racket/set)

(provide (all-defined-out))

;; TODO replace with weighted levenshtein distance, so "b" is closer to "a" than it is to "w"
(define (bounded-levenshtein s₁ s₂ Δₘₐₓ)
  (define len₁ (string-length s₁))
  (define len₂ (string-length s₂))
  (cond
    [(> (abs (- len₁ len₂)) Δₘₐₓ) #f]
    [(< len₂ len₁) (bounded-levenshtein s₂ s₁ Δₘₐₓ)]
    [else
     (define D (build-vector (add1 len₁) values))
     (let outer-loop ([j 0])
       (cond [(= j len₂)
              (define Δ (vector-ref D len₁))
              (and (<= Δ Δₘₐₓ) Δ)]
             [else
              (define c₂ (string-ref s₂ j))
              (define i-diag (vector-ref D 0))
              (vector-set! D 0 (add1 j))
              (let inner-loop ([i 0] [i-diag i-diag] [Δₘᵢₙ (add1 j)])
                (cond [(= i len₁) (and (<= Δₘᵢₙ Δₘₐₓ) (outer-loop (add1 j)))]
                      [else
                       (define c₁ (string-ref s₁ i))
                       (define i-curr (vector-ref D (add1 i)))
                       (define sub (+ i-diag (if (char=? c₁ c₂) 0 1)))
                       (define ins (add1 i-curr))
                       (define del (add1 (vector-ref D i)))
                       (define min-cost (min sub ins del))
                       (vector-set! D (add1 i) min-cost)
                       (inner-loop (add1 i) i-curr (min Δₘᵢₙ min-cost))]))]))]))

(define MAX-K 3)

(define (nearest-name name other-name-set other-name-list)
  (cond
    [(set-member? other-name-set name) name]
    [else
     (define name-str (symbol->string name))
     (let loop ([other-names other-name-list]
                [best-name-str #f]
                [best-distance +inf.0])
       (cond
         [(null? other-names) (string->symbol best-name-str)]
         [else
          (define other-name-str (car other-names))
          (define distance (bounded-levenshtein name-str other-name-str MAX-K))
          (if (and distance (< distance best-distance))
              (loop (cdr other-names) other-name-str distance)
              (loop (cdr other-names) best-name-str best-distance))]))]))
