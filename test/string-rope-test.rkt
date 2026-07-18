#lang racket/base

(module+ test
  (require rackunit
           racket/port
           rope/rope
           rope/string
           "private/harness.rkt")

  ;;; -------------------------------------------------------------------------------------------
  ;;; Oracle: string-rope operations vs. built-in string operations
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
      ([s (random-string (random 300))])
    (equal? (rope->string (string->rope s)) s))

  (check-property #:trials 300
      ([a (random-string (random 100))]
       [b (random-string (random 100))])
    (equal? (rope->string (string-rope-append1 (string->rope a) (string->rope b)))
            (string-append a b)))

  (check-property #:trials 50
      ([parts (for/list ([_ (in-range (add1 (random 8)))]) (random-string (random 50)))])
    (equal? (rope->string (apply string-rope-append (map string->rope parts)))
            (apply string-append parts)))

  (check-property #:trials 300
      ([s (random-string (add1 (random 200)))])
    (define n (string-length s))
    (define i (random (add1 n)))
    (define-values (l r) (string-rope-split (string->rope s) i))
    (and (equal? (rope->string l) (substring s 0 i))
         (equal? (rope->string r) (substring s i n))))

  (check-property #:trials 300
      ([s (random-string (add1 (random 200)))])
    (define n (string-length s))
    (define start (random (add1 n)))
    (define old-len (random (add1 (- n start))))
    (define new (random-string (random 20)))
    (equal? (rope->string (string-rope-splice (string->rope s) start old-len new))
            (string-append (substring s 0 start) new (substring s (+ start old-len) n))))

  (check-property #:trials 300
      ([s (random-string (add1 (random 200)))])
    (define n (string-length s))
    (define start (random (add1 n)))
    (define len (random (add1 (- n start))))
    (equal? (rope->string (string-rope-slice (string->rope s) start len))
            (substring s start (+ start len))))

  (test-case "string-rope-offset-index is the identity when width ≡ count"
    (define r (string->rope "hello world"))
    (check-equal? (string-rope-offset-index r 0) 0)
    (check-equal? (string-rope-offset-index r 4) 4))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Leaf-limit boundary
  ;;; -------------------------------------------------------------------------------------------
  (test-case "ropes at, above, and below the leaf limit are structurally sane and correctly tagged"
    (define limit (string-raw-limit))
    (check-equal? limit 512)
    (for ([n (list 0 1 (sub1 limit) limit (add1 limit) (* 2 limit) (add1 (* 2 limit)) 2000)])
      (define s (random-string n))
      (define r (string->rope s))
      (check-equal? (rope-count r) n)
      (check-equal? (rope->string r) s)
      (check-true (string-rope? r))
      (when (> n limit)
        (check-true (> (rope-depth r) 0))
        (check-true (rope-node? r)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Unicode
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 100
      ([s (random-unicode-string (random 80))])
    (equal? (rope->string (string->rope s)) s))

  (test-case "port output matches string->bytes/utf-8, byte for byte, even at tiny buffer sizes"
    (define s (string-append "plain-ascii, " (random-unicode-string 40) ", 🎉🚀"))
    (define expected (string->bytes/utf-8 s))
    (for ([buf-size (list 1 2 3 7 4096)])
      (define port (open-input-string-rope (string->rope s)))
      (define out (open-output-bytes))
      (let loop ()
        (define buf (make-bytes buf-size))
        (define n (read-bytes! buf port))
        (unless (eof-object? n)
          (write-bytes buf out 0 n)
          (loop)))
      (check-equal? (get-output-bytes out) expected)
      (close-input-port port)))

  (test-case "empty string rope port yields immediate eof"
    (check-true (eof-object? (read-byte (open-input-string-rope (string->rope ""))))))

  (test-case "closed string-rope ports report closed and reject further reads"
    (define port (open-input-string-rope (string->rope "abc")))
    (close-input-port port)
    (check-true (port-closed? port))
    (check-exn exn:fail? (λ () (read-byte port))))

  (check-property #:trials 100
      ([s (random-string (random 100))])
    (equal? (for/list ([c (in-string-rope (string->rope s))]) c) (string->list s)))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Contracts previously found buggy — now asserted positively, not as regressions
  ;;; -------------------------------------------------------------------------------------------
  (test-case "string-raw-empty returns the actual empty string, contract now matches"
    (check-equal? (string-raw-empty) ""))

  (test-case "make-string-rope-leaf returns a genuinely tagged leaf"
    (check-true (string-rope-leaf? (make-string-rope-leaf "abc"))))

  (test-case "the string-rope-leaf struct constructor itself now enforces string? on raw"
    (check-not-exn (λ () (string-rope-leaf 3 3 "abc")))
    (check-exn exn:fail:contract? (λ () (string-rope-leaf 1 1 'not-a-string)))))
