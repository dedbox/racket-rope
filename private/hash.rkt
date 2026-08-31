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

(require (for-meta 2 racket/base
                   syntax/parse)
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/define
                     syntax/transformer)
         math
         racket/fixnum
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

;;; Phase-0+1 constants
(define-syntax-parse-rule (define-constant var:id val:expr)
  (define-syntax var (make-variable-like-transformer #'val)))

(define-for-syntax (constant var-id)
  (syntax-e ((syntax-local-value var-id) #'here)))

;; Constants for a 64-bit host
(define-constant U #x3fffffff)          ; 2³⁰ - 1 (the largest 30-bit number)
(define-constant M #x3fffffdd)          ; 2³⁰ - 35 (the largest 30-bit prime)
(define-constant M-1 #x3fffffdc)        ; M - 1 = 2³⁰ - 36
(define-constant X₁ 31)
(define-constant X₂ 257)
(define-constant X₁⁴ #xe1781)
(define-constant X₂⁴ #x406048d)

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

(define-syntax-parse-rule (define-pm-fxmodulo name:id modulus:expr)
  #:do [(define mod-val (constant #'modulus))
        (define c-val   (modulo (expt 2 30) mod-val))]
  #:with m #'modulus
  #:with c (datum->syntax #'here c-val)
  (define-syntax-parse-rule (name n:expr)
    (let* ([N n]
           ;; First iteration, reduces ≤ 60 bits down to ≤ 36 bits.
           [H₁ (fxrshift N 30)]
           [L₁ (fxand N U)]
           [x₁ (fx+ (fx* H₁ c) L₁)]
           ;; Second iteration, reduces ≤ 36 bits down to ≤ 2³⁰ + 1153
           [H₂ (fxrshift x₁ 30)]
           [L₂ (fxand x₁ U)]
           [x₂ (fx+ (fx* H₂ c) L₂)])
      ;; Now x₂ < 2M, so a conditional subtraction guarantees x₂ < M. This
      ;; should be optimized to a branchless conditional move (cmov).
      (if (fx>= x₂ m) (fx- x₂ m) x₂))))

(define-pm-fxmodulo fxmodulo-M M)
(define-pm-fxmodulo fxmodulo-M-1 M-1)

;; M also satisfies Fermat's Little Theorem, which states that for any integer
;; X coprime to M, we have
;;
;;    Xᴹ⁻¹ ≡ 1 (mod M).
;;
;; This property allows us to efficiently calculate modular exponentiation of
;; the base primes by constraining the number of bits in the exponent.

(begin-for-syntax
  (define (base-powers base modulus max-bits)
    (let loop ([n 0] [b base] [acc null])
      (if (= n max-bits)
          (reverse acc)
          (loop (add1 n) (modulo (* b b) modulus) (cons (datum->syntax #'here b) acc))))))

(define-syntax-parse-rule (define-unrolled-fxexpt name:id base:expr)
  #:do [(define base-val (syntax-e (local-expand #'base 'expression #f)))]
  #:with (pₙ ...) (base-powers base-val (constant #'M) 30)
  #:with (shiftₙ ...) (for/list ([n (in-range 30)]) (datum->syntax #'here n))
  (define (name pow)
    (define safe-pow (fxmodulo-M-1 pow))
    (let* ([res 1]
           [res (if (fx= (fxand (fxrshift safe-pow shiftₙ) 1) 1)
                    (fxmodulo-M (fx* res pₙ))
                    res)]
           ...)
      res)))

(define-unrolled-fxexpt fxexpt-X₁-M X₁)
(define-unrolled-fxexpt fxexpt-X₂-M X₂)

(define (hash-combine hl1 hr1 hl2 hr2 right-len)
  (values (fxmodulo-M (fx+ (fxmodulo-M (fx* hl1 (fxexpt-X₁-M right-len))) hr1))
          (fxmodulo-M (fx+ (fxmodulo-M (fx* hl2 (fxexpt-X₂-M right-len))) hr2))))

