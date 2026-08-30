#lang racket

;; rope/benchmarks/poly-hash-bench.rkt
;;
;; Isolates hashing cost by rope size and cache state. Also directly
;; measures whether the modulus is staying in fixnum range: fixnum
;; arithmetic and bignum arithmetic are dispatched differently in Racket
;; CS, and a bignum-promoted loop shows up as a sharp constant-factor
;; jump that scales with size in a way pure fixnum arithmetic doesn't.

(require racket/format
         rope2/private/hash
         rope2/string-rope)

(define SIZES '(10 100 1000 10000 100000 1000000 10000000))
(define TRIALS 10)

(define (format-result ms)
  (cond [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 3))]
        [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
        [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (make-rope-of-size n)
  (string-chunk->rope (make-string n #\a)))

(define (time-ms thunk)
  (define start (current-inexact-monotonic-milliseconds))
  (thunk)
  (- (current-inexact-monotonic-milliseconds) start))

(module+ main
  (printf "| ~a | ~a | ~a |\n"
          (~a "Size" #:min-width 8) (~a "cold hash" #:min-width 14 #:align 'right)
          (~a "warm hash (cached)" #:min-width 18 #:align 'right))
  (printf "|-\n")
  (for ([n (in-list SIZES)])
    (define cold-times
      (for/list ([_ (in-range TRIALS)])
        (define r (make-rope-of-size n)) ;; fresh, uncached, every trial
        (time-ms (λ () (string-rope-hash r)))))
    (define r (make-rope-of-size n))
    (string-rope-hash r) ;; prime the cache once
    (define warm-times
      (for/list ([_ (in-range TRIALS)]) (time-ms (λ () (string-rope-hash r)))))
    (printf "| ~a | ~a | ~a |\n"
            (~a n #:min-width 8)
            (~a (format-result (apply min cold-times)) #:min-width 14 #:align 'right)
            (~a (format-result (apply min warm-times)) #:min-width 18 #:align 'right))))
