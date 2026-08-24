#lang racket

;; rope/benchmarks/content-equal-bench.rkt
;;
;; Times rope-content=? / equal? across the scenarios that actually differ
;; in cost: identity, warm-cache true equality, cold-cache true equality,
;; length mismatch (cheapest reject), and same-length content mismatch
;; (hash-mismatch reject vs. hash-collision-then-walk-reject).

(require racket/format rope2/string-rope)

(define SIZES '(100 1000 10000))
(define TRIALS 7)

(define (format-result ms)
  (cond [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 3))]
        [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
        [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

(define (time-ms thunk)
  (define start (current-inexact-monotonic-milliseconds))
  (thunk)
  (- (current-inexact-monotonic-milliseconds) start))

(define (best-of thunk) (apply min (for/list ([_ (in-range TRIALS)]) (time-ms thunk))))

(module+ main
  (printf "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
          (~a "Size" #:min-width 8)
          (~a "eq?" #:min-width 12 #:align 'right)
          (~a "equal, warm" #:min-width 12 #:align 'right)
          (~a "equal, cold" #:min-width 12 #:align 'right)
          (~a "len mismatch" #:min-width 14 #:align 'right)
          (~a "content mismatch" #:min-width 18 #:align 'right))
  (printf "|-\n")
  (for ([n (in-list SIZES)])
    (define a (string->rope (make-string n #\a)))
    (define b-warm (string->rope (make-string n #\a)))
    (string-rope-poly-hash a) (string-rope-poly-hash b-warm) ;; prime both
    (define shorter (string->rope (make-string (sub1 n) #\a)))
    (define different (string->rope (string-append (make-string (sub1 n) #\a) "b")))
    (printf "| ~a | ~a | ~a | ~a | ~a | ~a |\n"
            (~a n #:min-width 8)
            (~a (format-result (best-of (λ () (equal? a a))))                    #:min-width 12 #:align 'right)
            (~a (format-result (best-of (λ () (equal? a b-warm))))               #:min-width 12 #:align 'right)
            (~a (format-result (best-of (λ () (equal? a (string->rope (make-string n #\a)))))) #:min-width 12 #:align 'right)
            (~a (format-result (best-of (λ () (equal? a shorter))))              #:min-width 14 #:align 'right)
            (~a (format-result (best-of (λ () (equal? a different))))           #:min-width 18 #:align 'right))))
