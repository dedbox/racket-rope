#lang racket/base

(module+ test
  (require (for-syntax racket/base
                       syntax/parse)
           racket/pretty
           racket/string
           racket/vector
           rackunit
           rackunit/text-ui
           rope2/generic
           rope2/rope
           rope2/rope-type
           syntax/parse/define)

  (define (run-suite! suite)
    (define failed (run-tests suite 'verbose))
    (unless (zero? failed)
      (error 'raco-test "~a check(s) failed" failed)))

  (define-syntax (test-property stx)
    (syntax-parse stx
      [(_ name:expr #:trials trials:expr ([id:id gen-expr:expr] ...) body:expr ...+)
       #`(test-case name
           (for ([iteration (in-range trials)])
             (let* ([id gen-expr] ...)
               (with-check-info
                 (['iteration iteration]
                  ['expression (let* ([str (pretty-format '(let () body ...) #:mode 'display)]
                                      [lines (string-split str "\n")]
                                      [indented (cons (car lines)
                                                      (for/list ([line (in-list (cdr lines))])
                                                        (string-append "  " line)))])
                                 (string-info (string-join indented "\n")))]
                  ['id id] ...)
                 #,(syntax/loc stx (check-true (let () body ...)))))))]))

  (define WEIGHTED-CHUNK-LIMIT 8)

  (define (weighted-chunk-compare a b)
    (let loop ([i 0])
      (cond [(and (= i (vector-length a)) (= i (vector-length b))) '=]
            [(= i (vector-length a))                               '<]
            [(= i (vector-length b))                               '>]
            [(< (vector-ref a i) (vector-ref b i))                 '<]
            [(> (vector-ref a i) (vector-ref b i))                 '>]
            [else (loop (add1 i))])))

  (define-rope-type weighted
    #:chunk?        vector?
    #:elem-size     values
    #:chunk-limit   (λ () WEIGHTED-CHUNK-LIMIT)
    #:chunk-empty   vector
    #:chunk-count   vector-length
    #:chunk-size    (λ (chunk) (for/sum ([w (in-vector chunk)]) w))
    #:chunk-slice   (λ (chunk i k) (vector-copy chunk i (+ i k)))
    #:chunk-append  (λ (chunks) (apply vector-append chunks))
    #:chunk-ref     vector-ref
    #:chunk-compare weighted-chunk-compare)

  (define (random-weight)           (add1 (random 4)))
  (define (random-weighted-chunk n) (build-vector n (λ (_) (random-weight))))

  (define generic-ops-suite
    (test-suite "generic"
      (test-case "chunk generic operations agree with their direct definitions"
        (check-equal? (rope-chunk-limit weighted) WEIGHTED-CHUNK-LIMIT)
        (check-true   (rope-chunk? weighted (vector 1 2 3)))
        (check-false  (rope-chunk? weighted "not a vector"))
        (check-equal? (rope-chunk-empty weighted) (vector))
        (check-equal? (rope-chunk-count weighted (vector 1 2 3)) 3)

        (check-equal? (rope-chunk-size weighted (vector 1 2 3)) 6)
        (check-equal? (rope-chunk-slice weighted (vector 1 2 3 4) 1 2) (vector 2 3))
        (check-equal? (rope-chunk-append weighted (list (vector 1 2) (vector 3 4)))
                      (vector 1 2 3 4))
        (check-equal? (rope-chunk-ref weighted (vector 10 20 30) 1) 20))

      (test-case "leaf-constructor / node-constructor return usable constructors"
        (check-equal? (make-rope-leaf weighted (vector 1))
                      (weighted-rope-leaf 1 1 (vector 1)))
        (check-equal? (make-rope-node weighted
                                      (make-rope-leaf weighted (vector 1 2))
                                      (make-rope-leaf weighted (vector 3 4)))
                      (weighted-rope-node 4 10 1
                                          (weighted-rope-leaf 2 3 (vector 1 2))
                                          (weighted-rope-leaf 2 7 (vector 3 4)))))))

  (define core-ops-suite
    (test-suite "empty rope, leaves, concat"
      (test-case "empty rope: zero count/size, empty content, balanced"
        (define e (make-empty-rope weighted))
        (check-equal? (rope-count e) 0)
        (check-equal? (rope-size  e) 0)
        (check-true   (rope-empty? e))
        (check-equal? (rope->weighted e) (vector)))

      (test-case "leaf count ≠ leaf size for weighted elements"
        (define l (make-rope-leaf weighted (vector 1 2 3 4)))
        (check-true   (rope-leaf? l))
        (check-equal? (rope-count l) 4)
        (check-equal? (rope-size  l) 10))

      (test-case "rope-concat derives count/size correctly"
        (define n (rope-concat weighted
                               (make-rope-leaf weighted (vector 1 2))
                               (make-rope-leaf weighted (vector 3 4 5))))
        (check-true   (rope-node? n))
        (check-equal? (rope-count n) 5)
        (check-equal? (rope-size  n) 15)
        (check-equal? (rope->weighted n) (vector 1 2 3 4 5)))))

  (define append-suite
    (test-suite "append"
      (test-property "rope-append agrees with vector-append and stays mostly balanced"
          #:trials 300
          ([a (random-weighted-chunk (random 40))]
           [b (random-weighted-chunk (random 40))])
        (define r (rope-append weighted
                               (list (make-rope-leaf weighted a)
                                     (make-rope-leaf weighted b))))
        (and (= (rope-count r) (+ (vector-length a) (vector-length b)))
             (= (rope-size  r) (+ (weighted-rope-chunk-size a)
                                  (weighted-rope-chunk-size b)))
             (equal? (rope->weighted r) (vector-append a b))
             (rope-mostly-balanced? r)))

      (test-case "empty rope is a left unit and a right unit of rope-concat/lazy"
        (define e (make-empty-rope weighted))
        (define r (make-rope-leaf weighted (vector 1 2 3)))
        (check-equal? (rope->weighted (rope-append weighted (list e r))) (rope->weighted r))
        (check-equal? (rope->weighted (rope-append weighted (list r e))) (rope->weighted r)))

      (test-case "rope-append with no ropes returns the empty rope"
        (check-true (rope-empty? (rope-append weighted null))))

      (test-property "rope-append agrees with vector-append"
          #:trials 50
          ([chunks (for/list ([_ (in-range (add1 (random 9)))])
                     (random-weighted-chunk (random 7)))])
        (define r (rope-append weighted (map (λ (c) (make-rope-leaf weighted c)) chunks)))
        (equal? (rope->weighted r) (apply vector-append chunks)))

      (test-property "rope-rebalance handles non-leaf runs, not just leaves"
          #:trials 100
          ([depth (add1 (random 5))]
           [leaf-chunk (random-weighted-chunk (add1 (random 6)))])
        (define deep
          (for/fold ([r (make-rope-leaf weighted (random-weighted-chunk 1))])
                    ([_ (in-range depth)])
            (rope-append weighted
                         (list r (make-rope-leaf weighted (random-weighted-chunk 1))))))
        (define combined (rope-concat weighted deep (make-rope-leaf weighted leaf-chunk)))
        (rope-mostly-balanced? (rope-rebalance weighted combined)))

      (test-case "sequential appends stay Fibonacci-balanced across many steps"
        (for/fold ([r (make-empty-rope weighted)]) ([i (in-range 800)])
          (define chunk  (random-weighted-chunk (add1 (random 6))))
          (define leaf   (make-rope-leaf weighted chunk))
          (define r+leaf (rope-append weighted (list r leaf)))
          (with-check-info (['iteration i] ['chunk chunk] ['r+leaf r+leaf])
            (check-true (rope-mostly-balanced? r+leaf)))
          r+leaf)
        (void))

      ;; Regressions

      (test-case "rope-rebalance does not skip intervening occupied slots"
        (define chunks (list #(3) #() #(2 1)))
        (define leaves (map (λ (c) (make-rope-leaf weighted c)) chunks))
        (define r (rope-append weighted leaves))
        (define a (rope->weighted r))
        (define b (apply vector-append chunks))
        (check-equal? a b))))

  (run-suite! (test-suite "generic-tests.rkt"
                generic-ops-suite core-ops-suite append-suite)))
