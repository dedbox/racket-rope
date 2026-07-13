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
    (equal? (rope->bytes (bytes-rope-append (bytes->rope a) (bytes->rope b)))
            (bytes-append a b)))

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

  ;;; -------------------------------------------------------------------------------------------
  ;;; Full byte-value coverage, including NUL and 0xFF, which strings can't carry directly
  ;;; -------------------------------------------------------------------------------------------
  (test-case "the full 0..255 byte range round-trips"
    (define b (list->bytes (for/list ([i (in-range 256)]) i)))
    (check-equal? (rope->bytes (bytes->rope b)) b))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Leaf-limit boundary: BYTES-LEAF-LIMIT = 512
  ;;; -------------------------------------------------------------------------------------------
  (test-case "ropes at, above, and below the leaf limit are structurally sane"
    (for ([n (list 0 1 511 512 513 1024 1025 2000)])
      (define b (random-bytes n))
      (define r (bytes->rope b))
      (check-equal? (rope-count r) n)
      (check-equal? (rope->bytes r) b)
      (when (> n BYTES-LEAF-LIMIT)
        (check-true (> (rope-depth r) 0)))))

  ;;; -------------------------------------------------------------------------------------------
  ;;; Port behavior, tiny buffers included (this port has no per-char fragmentation concerns,
  ;;; unlike the string port, but small-buffer chunking is still worth confirming).
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
    (check-true (eof-object? (read-byte (open-input-bytes-rope (bytes->rope #"")))))))
