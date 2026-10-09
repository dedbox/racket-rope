#lang racket/base

(module+ test
  (require rope/rope
           "../private/testing.rkt")

  (define (same-length? a b) (= (rope-length a) (rope-length b)))
  (define (never-equal a b) #f)

  (define (leaf n [equal-fun same-length?] [hash-h 0] [hash-p 0])
    (rope-leaf n n hash-h hash-p equal-fun (make-vector n 0)))

  (define (node l r [equal-fun same-length?] [hash-h 0] [hash-p 0])
    (rope-node (+ (rope-length l) (rope-length r))
               (+ (rope-width l) (rope-width r))
               hash-h hash-p equal-fun
               (add1 (max (rope-depth l) (rope-depth r)))
               l r))

  (define core-suite
    (test-suite "core data structure"
      (test-case "rope-depth: 0 for a leaf, 1 + max(children) for a node"
        (check-equal? (rope-depth (leaf 3)) 0)
        (define l (leaf 1))
        (define r (node (leaf 1) (leaf 1)))
        (check-equal? (rope-depth (node l r)) 2))

      (test-case "rope-empty?: zero width, regardless of length"
        (check-true  (rope-empty? (leaf 0)))
        (check-false (rope-empty? (leaf 1))))

      (test-case "rope-chunks: left-to-right order across multiple levels"
        (define a (rope-leaf 1 1 0 0 same-length? (vector 'a)))
        (define b (rope-leaf 1 1 0 0 same-length? (vector 'b)))
        (define c (rope-leaf 1 1 0 0 same-length? (vector 'c)))
        (define tree (node (node a b) c))
        (check-equal? (rope-chunks tree) (list (vector 'a) (vector 'b) (vector 'c))))

      (test-case "equal?/equal-hash-code delegate to equal-fun/hash-h/hash-p, not structure"
        ;; Same shape, same length, but one pair's equal-fun always says no.
        (define a (leaf 3 same-length? 7 11))
        (define b (leaf 3 same-length? 7 11))
        (check-true  (equal? a b))
        (check-equal? (equal-hash-code a) (equal-hash-code b))
        (define c (leaf 3 never-equal 7 11))
        (define d (leaf 3 never-equal 7 11))
        (check-false (equal? c d))
        ;; Different equal-fun (not eq? to each other), same length: the
        ;; equal-proc method requires the two ropes' equal-funs to be the
        ;; *same* function (eq?) before it will even call either one, so
        ;; this must be #f regardless of what same-length? would say --
        ;; two distinct rope "types" are never equal? to each other, even
        ;; with identical length/width/hash.
        (check-false (equal? (leaf 3 same-length?) (leaf 3 never-equal))))))

  (define balance-suite
    (test-suite "Fibonacci balance predicates"
      (test-case "fib-bound matches the offset-by-two Fibonacci sequence"
        (check-equal? (fib-bound 0) 1)
        (check-equal? (fib-bound 1) 2)
        (check-equal? (fib-bound 2) 3)
        (check-equal? (fib-bound 3) 5)
        (check-equal? (fib-bound 4) 8))

      (test-case "strictly-balanced?: true exactly at and above the threshold"
        ;; depth 2 -> threshold is fib-bound(2+2) = fib-bound(4) = 8
        (check-false (rope-strictly-balanced? (node (leaf 3) (node (leaf 2) (leaf 2)))))
        (check-true  (rope-strictly-balanced? (node (leaf 4) (node (leaf 2) (leaf 2))))))

      (test-case "mostly-balanced?: true exactly at and above the threshold"
        ;; depth 2 -> threshold is fib-bound(2) = 3
        (check-false (rope-mostly-balanced? (node (leaf 1) (node (leaf 0) (leaf 1)))))
        (check-true  (rope-mostly-balanced? (node (leaf 2) (node (leaf 0) (leaf 1))))))

      (test-case "both predicates: a length-0 rope is always balanced, any depth"
        (define deep-empty (node (node (leaf 0) (leaf 0)) (node (leaf 0) (leaf 0))))
        (check-true (rope-strictly-balanced? deep-empty))
        (check-true (rope-mostly-balanced?   deep-empty)))))

  (run-suite! (test-suite "rope.rkt" core-suite balance-suite)))
