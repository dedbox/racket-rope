#lang racket

;; rope/benchmarks/offset-index-bench.rkt

(require (except-in math permutations)
         racket/format
         racket/list
         rope2/rope
         rope2/rope-type
         rope2/string-rope)

(struct token (kind data) #:transparent)

(define (tokens-elem-size vec i)
  (string-length (token-data (vector-ref vec i))))

(define (tokens-chunk-size vec)
  (for/sum ([i (in-range (vector-length vec))])
    (tokens-elem-size vec i)))

(define-rope-type tokens
  #:chunk?        vector?
  #:elem-size     tokens-elem-size
  #:chunk-limit   (λ () 128)
  #:chunk-empty   (λ () (vector))
  #:chunk-count   vector-length
  #:chunk-size    tokens-chunk-size
  #:chunk-slice   vector-copy
  #:chunk-append  (λ (strs) (apply vector-append strs))
  #:chunk-ref     vector-ref)

;; Approximate probability weights for English word lengths 1 to 15+
(define word-lengths '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(define length-probs
  '(0.01 0.08 0.17 0.19 0.15 0.12 0.09 0.07 0.05 0.03 0.02 0.01 0.005 0.003 0.002))

(define length-dist (discrete-dist word-lengths length-probs))

(define (make-string-rope chunks)
  (define (make-str _) (make-string 512 #\a))
  (string->rope (apply string-append (build-list chunks make-str))))

(define (make-tokens-rope chunks)
  (define (make-token _)
    (token 'xxx (make-string (sample length-dist) #\a)))
  (tokens->rope (apply vector (build-list chunks make-token))))

;; =============================================================================
;; Benchmarked Implementations
;; =============================================================================

(define (offset-index-A a0 p0 elem-size)
  (let loop ([a a0] [p p0])
    (if (rope-leaf? a)
        (cond
          [(>= p (rope-leaf-size a)) (sub1 (rope-leaf-count a))]
          [(number? elem-size) (quotient p elem-size)]
          [else (let chunk-loop ([p p] [i 0])
                  (let ([k (elem-size (rope-leaf-chunk a) i)])
                    (if (<= p k) i (chunk-loop (- p k) (add1 i)))))])
        (let ([l (rope-node-left a)])
          (if (< p (rope-size l))
              (loop l p)
              (+ (rope-count l) (loop (rope-node-right a) (- p (rope-size l)))))))))

(define (offset-index-B a0 p0 elem-size)
  (if (number? elem-size)
      (min (quotient p0 elem-size) (sub1 (rope-count a0)))
      (let loop ([a a0] [p p0])
        (if (rope-leaf? a)
            (let chunk-loop ([p p] [i 0])
              (let ([k (elem-size (rope-leaf-chunk a) i)])
                (if (<= p k) i (chunk-loop (- p k) (add1 i)))))
            (let ([l (rope-node-left a)])
              (if (< p (rope-size l))
                  (loop l p)
                  (+ (rope-count l) (loop (rope-node-right a) (- p (rope-size l))))))))))

;; =============================================================================
;; Benchmarking Apparatus
;; =============================================================================

(define (format-result ms)
  (cond
    [(>= ms 1.0e3)  (format "~a sec" (real->decimal-string (/ ms 1.0e3) 2))]
    [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 2))]
    [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
    [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (measure-time proc iters trials)
  (apply min
         (for/list ([_ (in-range trials)])
           (collect-garbage)
           (collect-garbage)
           (define start (current-inexact-monotonic-milliseconds))
           (for/fold ([acc 0]) ([_ (in-range iters)])
             (bitwise-xor acc (proc)))
           (define end (current-inexact-monotonic-milliseconds))
           (/ (- end start) (exact->inexact iters)))))

;; =============================================================================
;; Execution & Tabulation
;; =============================================================================

(define ITERATIONS 5000000)
(define TRIALS     5)

(define (run-benchmark)
  (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
          (~a "Rope Type" #:min-width 9)
          (~a "Chunks" #:min-width 8 #:align 'right)
          (~a "Position" #:min-width 10)
          (~a "Impl A Time" #:min-width 15 #:align 'right)
          (~a "Impl B Time" #:min-width 15 #:align 'right)
          (~a "Δ (A - B)" #:min-width 15 #:align 'right)
          (~a "Speedup (A/B)" #:min-width 15 #:align 'right))
  (printf "|-----------+----------+------------+-----------------+-----------------+-----------------+-----------------|\n")
  
  (for* ([type   (in-list '(string token))]
         [chunks (in-list '(1 10 100 1000))])
    
    (define a0
      (if (eq? type 'string)
          (make-string-rope chunks)
          (make-tokens-rope chunks)))
    
    (define elem-size
      (if (eq? type 'string)
          1
          (λ (chunk i) (string-length (token-data (vector-ref chunk i))))))
    
    (define size (rope-size a0))
    
    (define sweeps
      (list (cons "Start" 0)
            (cons "Mid"   (quotient size 2))
            (cons "End"   (max 0 (- size 2)))))
    
    (for ([pos-pair (in-list sweeps)])
      (define pos-name (car pos-pair))
      (define p0       (cdr pos-pair))
      
      ;; 1. Tier-2 JIT Warmup
      (for ([_ (in-range 10000)])
        (offset-index-A a0 p0 elem-size)
        (offset-index-B a0 p0 elem-size))
      
      ;; 2. Strict Measurement
      (define a-ms (measure-time (λ () (offset-index-A a0 p0 elem-size)) ITERATIONS TRIALS))
      (define b-ms (measure-time (λ () (offset-index-B a0 p0 elem-size)) ITERATIONS TRIALS))
      
      ;; 3. Statistical Analysis
      (define speedup
        (if (zero? b-ms)
            "∞x"
            (format "~ax" (real->decimal-string (/ a-ms b-ms) 2))))
      
      (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
              (~a (string-titlecase (symbol->string type)) #:min-width 9)
              (~a chunks #:min-width 8 #:align 'right)
              (~a pos-name #:min-width 10)
              (~a (format-result a-ms) #:min-width 15 #:align 'right)
              (~a (format-result b-ms) #:min-width 15 #:align 'right)
              (~a (format-result (- a-ms b-ms)) #:min-width 15 #:align 'right)
              (~a speedup #:min-width 15 #:align 'right)))))

(module+ main
  (run-benchmark))
