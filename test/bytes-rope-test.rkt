#lang racket/base

(module+ test
  (require rackunit
           racket/port
           rope/rope
           rope/bytes
           "private/harness.rkt")

  ;;; -------------------------------------------------------------------------------------------
  ;;; Oracle: bytes-rope operations vs. built-in bytes operations
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 300
      ([b (random-bytes (random 300))])
    (equal? (rope->bytes (bytes->rope b)) b))

  (check-property #:trials 300
      ([a (random-bytes (random 100))]
       [b (random-bytes (random 100))])
    (equal? (rope->bytes (bytes-rope-append1 (bytes->rope a) (bytes->rope b)))
            (bytes-append a b)))

  (check-property #:trials 50
      ([parts (for/list ([_ (in-range (add1 (random 8)))]) (random-bytes (random 50)))])
    (equal? (rope->bytes (apply bytes-rope-append (map bytes->rope parts)))
            (apply bytes-append parts)))

  (check-property #:trials 300
      ([b (random-bytes (add1 (random 200)))])
    (define n (bytes-length b))
    (define i (random (add1 n)))
    (define-values (l r) (bytes-rope-split (bytes->rope b) i))
    (and (equal? (rope->bytes l) (subbytes b 0 i))
         (equal? (rope->bytes r) (subbytes b i n))))

  (check-property #:trials 300
      ([b (random-bytes (add1 (random 200)))])
    (define n (bytes-length b))
    (define start (random (add1 n)))
    (define old-len (random (add1 (- n start))))
    (define new (random-bytes (random 20)))
    (equal? (rope->bytes (bytes-rope-splice (bytes->rope b) start old-len new))
            (bytes-append (subbytes b 0 start) new (subbytes b (+ start old-len) n))))

  (check-property #:trials 300
      ([b (random-bytes (add1 (random 200)))])
    (define n (bytes-length b))
    (define start (random (add1 n)))
    (define len (random (add1 (- n start))))
    (equal? (rope->bytes (bytes-rope-slice (bytes->rope b) start len))
            (subbytes b start (+ start len))))

  (check-property #:trials 200
      ([b (random-bytes (add1 (random 200)))])
    (define n (bytes-length b))
    (define start (random (add1 n)))
    (define k (random (add1 (- n start))))
    (define cur (bytes-cursor-drop (bytes-rope->cursor (bytes->rope b)) start))
    (equal? (rope->bytes (bytes-cursor-take cur k)) (subbytes b start (+ start k))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Full byte-value coverage, including NUL and 0xFF
  ;;; -------------------------------------------------------------------------------------------
  (test-case "the full 0..255 byte range round-trips"
    (define b (list->bytes (for/list ([i (in-range 256)]) i)))
    (check-equal? (rope->bytes (bytes->rope b)) b))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Leaf-limit boundary
  ;;; -------------------------------------------------------------------------------------------
  (test-case "ropes at, above, and below the leaf limit are structurally sane and correctly tagged"
    (define limit (bytes-raw-limit))
    (check-equal? limit 512)
    (for ([n (list 0 1 (sub1 limit) limit (add1 limit) (* 2 limit) (add1 (* 2 limit)) 2000)])
      (define b (random-bytes n))
      (define r (bytes->rope b))
      (check-equal? (rope-count r) n)
      (check-equal? (rope->bytes r) b)
      (check-true (bytes-rope? r))
      (when (> n limit)
        (check-true (> (rope-depth r) 0))
        (check-true (rope-node? r)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Port behavior, tiny buffers included
  ;;; -------------------------------------------------------------------------------------------
  (check-property #:trials 50
      ([b (random-bytes (random 500))])
    (define port (open-input-bytes-rope (bytes->rope b)))
    (define out (open-output-bytes))
    (let loop ()
      (define buf (make-bytes 3))
      (define n (read-bytes! buf port))
      (unless (eof-object? n)
        (write-bytes buf out 0 n)
        (loop)))
    (equal? (get-output-bytes out) b))

  (test-case "empty bytes rope port yields immediate eof"
    (check-true (eof-object? (read-byte (open-input-bytes-rope (bytes->rope #""))))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Contracts previously found buggy — now asserted positively, not as regressions
  ;;; -------------------------------------------------------------------------------------------
  (test-case "bytes-raw-empty returns the actual empty byte string"
    (check-equal? (bytes-raw-empty) #""))

  (test-case "bytes-raw-ref returns a byte, matching its corrected contract"
    (check-equal? (bytes-raw-ref (bytes 65 66 67) 0) 65))

  (test-case "the bytes-rope-leaf struct constructor itself now enforces bytes? on raw"
    (check-not-exn (λ () (bytes-rope-leaf 3 3 #"abc")))
    (check-exn exn:fail:contract? (λ () (bytes-rope-leaf 1 1 'not-bytes))))

  (test-case "bytes-cursor-peek returns a byte, matching its corrected contract"
    (define cur (bytes-rope->cursor (bytes->rope #"A")))
    (check-equal? (bytes-cursor-peek cur) 65))

  (test-case "bytes-cursor-peek past the end returns #f, matching its widened contract"
    (define r (bytes->rope #"A"))
    (check-false (bytes-cursor-peek (bytes-cursor-drop (bytes-rope->cursor r) 1)))))
