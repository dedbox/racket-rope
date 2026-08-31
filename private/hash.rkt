#lang racket/base

;; rope/private/hash.rkt
;;
;; A High-Performance Associative Polynomial Rolling Hash
;;
;; Hash
;;
;; Polynomial Hash
;;
;; Rolling Hash
;;
;; Associativity
;;
;; Numeric Precision
;; - On CS, fixnums only give us 60 bits to work with
;;   - 1 bit for sign
;;   - what are the other two bits for?

(require (for-syntax racket/base
                     syntax/parse)
         racket/fixnum
         syntax/parse/define)

(provide (all-defined-out))

;; Constants for a 64-bit host
(define U  #x3fffffff)                  ; 2³⁰ - 1 (the largest 30-bit number)
(define M  #x3fffffdd)                  ; 2³⁰ - 35 (the largest 30-bit prime)
(define C  35)                          ; 2³⁰ (mod M)
(define X  31)
(define X⁴ #xe1781)

;; For pseudo-Mersenne prime M = 2³⁰ - 35, we can exploit the fact that
;;
;;    M = 2³⁰ - 35 ≡ 0  (mod M)
;;             2³⁰ ≡ 35 (mod M)
;;
;; to perform modular reduction without expensive division operations. By
;; splitting a 60-bit number X into its higher-order (H) and lower-order (L)
;; bits, and substituting 35 for 2³⁰, we get
;;
;;    X (mod M) ≡ H · 2³⁰ + L (mod M)
;;              ≡ H · 35  + L (mod M),
;;
;; but the result is not guaranteed to be less than M.
;;
;; However, the maximum number of reduction steps is strictly bounded. The
;; theoretical maximum number of reduction steps occurs when X = 2⁶⁰ - 1,
;; where H = L = 2³⁰ - 1 and we have
;;
;;    x₁ = (2³⁰ - 1) · 35 + (2³⁰ - 1) = 36 · (2³⁰ - 1) = 0x8ffffffdc,
;;
;; a 36-bit number. We can also decompose x₁ into
;;
;;    x₁ = H₁ · 2³⁰ + L₁
;;
;; where H₁ = 35 and L₁ = 2³⁰ - 72, and we have
;;
;;    x₂ = 35² + (2³⁰ - 72) = 2³⁰ + 1153 = 0x40000481,
;;
;; a 31-bit number. In the general case, if x₂ < M, then we have achieved full
;; modular reduction. Otherwise, x₂ is at most 2M and full modular reduction
;; is achieved by subtracting M.
;;
;; This means for any X < 2⁶⁰, full modular reduction is guaranteed in at most
;; two reduction steps and one conditional subtraction. Since modular
;; reduction is idempotent for any X < M, we can safely fix the number of
;; iterations at two to avoid stalling on a missed branch prediction.

(define-syntax-parse-rule (fxmodulo-M n:expr)
  (let* ([N n]
         ;; First iteration, reduces ≤ 60 bits down to ≤ 36 bits.
         [H₁ (fxrshift N 30)]
         [L₁ (fxand N U)]
         [x₁ (fx+ (fx* H₁ C) L₁)]
         ;; Second iteration, reduces ≤ 36 bits down to ≤ 2³⁰ + 1153
         [H₂ (fxrshift x₁ 30)]
         [L₂ (fxand x₁ U)]
         [x₂ (fx+ (fx* H₂ C) L₂)])
    ;; Now x₂ < 2M, so a conditional subtraction guarantees x₂ < M. This
    ;; should be optimized to a branchless conditional move (cmov).
    (if (fx>= x₂ M) (fx- x₂ M) x₂)))

