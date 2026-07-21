#lang racket/base

(require racket/contract
         racket/match
         racket/port
         rope/define-rope-type)

(provide (rope-type-out/contract bytes #:raw bytes? #:element byte?)
         (contract-out [empty-bytes-rope      bytes-rope?]
                       [open-input-bytes-rope (bytes-rope? . -> . input-port?)]))

(define-rope-type bytes
  bytes?
  (λ () 512)
  (λ () #"")
  bytes-length
  bytes-length
  subbytes
  (λ (raws) (apply bytes-append raws))
  bytes-ref
  #:compare (λ (a b) (cond [(bytes<? a b) '<] [(bytes=? a b) '=] [else '>])))

(define empty-bytes-rope (make-empty-bytes-rope))

;; Reads one rope element (a byte) per port pull. Each read of k bytes costs
;; O(k). Reading the whole rope costs O(n) overall, without ever flattening it
;; into a single byte string.
(define (open-input-bytes-rope rope)
  (define active-cursor (box (bytes-rope->cursor rope)))
  (define pending-bytes (box #""))

  (define (read-in bstr)
    (when (and (zero? (bytes-length (unbox pending-bytes)))
               (not (bytes-cursor-at-end? (unbox active-cursor))))
      (define cur (unbox active-cursor))
      (define char (bytes-cursor-peek cur))
      (set-box! pending-bytes (bytes char))
      (set-box! active-cursor (bytes-cursor-advance cur)))

    (match (unbox pending-bytes)
      [#"" eof]
      [current-buffer
       (define transfer-len (min (bytes-length bstr) (bytes-length current-buffer)))
       (bytes-copy! bstr 0 (subbytes current-buffer 0 transfer-len))
       (set-box! pending-bytes (subbytes current-buffer transfer-len))
       transfer-len]))

  (define (close)
    (set-box! active-cursor #f)
    (set-box! pending-bytes #""))

  (make-input-port/read-to-peek 'bytes-rope-input-port read-in #f close))

(module* uncontracted #f
  (provide (rope-type-out bytes)
           empty-bytes-rope
           open-input-bytes-rope))

;;; ---------------------------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------------------------

(module+ test
  (require rackunit
           rope/rope
           "private/testing.rkt")

  (define oracle-suite
    (test-suite "bytes-rope vs. built-in bytes operations"
      (test-property "round-trip through bytes->rope/rope->bytes" #:trials 300
          ([b (random-bytes (random 300))])
        (equal? (rope->bytes (bytes->rope b)) b))

      (test-property "append1 matches bytes-append" #:trials 300
          ([a (random-bytes (random 100))] [b (random-bytes (random 100))])
        (equal? (rope->bytes (bytes-rope-append1 (bytes->rope a) (bytes->rope b))) (bytes-append a b)))

      (test-property "variadic append matches bytes-append across N parts" #:trials 50
          ([parts (for/list ([_ (in-range (add1 (random 8)))]) (random-bytes (random 50)))])
        (equal? (rope->bytes (apply bytes-rope-append (map bytes->rope parts))) (apply bytes-append parts)))

      (test-property "split matches subbytes on both sides" #:trials 300
          ([b (random-bytes (add1 (random 200)))])
        (define n (bytes-length b))
        (define i (random (add1 n)))
        (define-values (l r) (bytes-rope-split (bytes->rope b) i))
        (and (equal? (rope->bytes l) (subbytes b 0 i)) (equal? (rope->bytes r) (subbytes b i n))))

      (test-property "splice matches a subbytes/bytes-append oracle" #:trials 300
          ([b (random-bytes (add1 (random 200)))])
        (define n (bytes-length b))
        (define start (random (add1 n)))
        (define old-len (random (add1 (- n start))))
        (define new (random-bytes (random 20)))
        (equal? (rope->bytes (bytes-rope-splice (bytes->rope b) start old-len new))
                (bytes-append (subbytes b 0 start) new (subbytes b (+ start old-len) n))))

      (test-property "slice matches subbytes" #:trials 300
          ([b (random-bytes (add1 (random 200)))])
        (define n (bytes-length b))
        (define start (random (add1 n)))
        (define len (random (add1 (- n start))))
        (equal? (rope->bytes (bytes-rope-slice (bytes->rope b) start len)) (subbytes b start (+ start len))))

      (test-property "cursor-take from a dropped cursor matches subbytes" #:trials 200
          ([b (random-bytes (add1 (random 200)))])
        (define n (bytes-length b))
        (define start (random (add1 n)))
        (define k (random (add1 (- n start))))
        (define cur (bytes-cursor-drop (bytes-rope->cursor (bytes->rope b)) start))
        (equal? (rope->bytes (bytes-cursor-take cur k)) (subbytes b start (+ start k))))))

  (define structure-suite
    (test-suite "structure and coverage"
      (test-case "the full 0..255 byte range round-trips"
        (define b (apply bytes (for/list ([i (in-range 256)]) i)))
        (check-equal? (rope->bytes (bytes->rope b)) b))

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
            (check-true (rope-node? r)))))))

  (define port-suite
    (test-suite "open-input-bytes-rope"
      (test-property "round-trips through the port, byte for byte, at a small buffer size" #:trials 50
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
        (check-true (eof-object? (read-byte (open-input-bytes-rope (bytes->rope #""))))))))

  (define contract-suite
    (test-suite "contracted surface"
      (test-case "bytes-raw-empty / bytes-raw-ref return actual bytes"
        (check-equal? (bytes-raw-empty) #"")
        (check-equal? (bytes-raw-ref (bytes 65 66 67) 0) 65))

      ;; (test-case "the bytes-rope-leaf struct constructor enforces bytes? on raw"
      ;;   (check-not-exn (λ () (bytes-rope-leaf 3 3 #"abc")))
      ;;   (check-exn exn:fail:contract? (λ () (bytes-rope-leaf 1 1 'not-bytes))))

      (test-case "bytes-cursor-peek returns a byte, or #f past the end"
        (check-equal? (bytes-cursor-peek (bytes-rope->cursor (bytes->rope #"A"))) 65)
        (define r (bytes->rope #"A"))
        (check-false (bytes-cursor-peek (bytes-cursor-drop (bytes-rope->cursor r) 1))))))

  (define equal-hash-suite
    (let ()
      ;; Same content built via two structurally different trees, so equal?/hash are exercised
      ;; across leaf boundaries that don't align.
      (define (mixed-build b)
        (bytes-rope-append (bytes->rope (subbytes b 0 3)) (bytes->rope (subbytes b 3))))
      (test-suite "content-based equal?/hash"
        (test-case "equal? is content-based across differing tree shapes"
          (define a (bytes->rope #"hello world"))
          (define b (mixed-build #"hello world"))
          (check-true  (equal? a b))
          (check-false (equal? a (bytes->rope #"hello worlD"))))

        (test-case "equal-hash-code and equal-secondary-hash-code agree whenever equal? does"
          (define a (bytes->rope #"supercalifragilisticexpialidocious"))
          (define b (mixed-build #"supercalifragilisticexpialidocious"))
          (check-equal? (equal-hash-code a) (equal-hash-code b))
          (check-equal? (equal-secondary-hash-code a) (equal-secondary-hash-code b)))

        (test-case "empty ropes are equal regardless of provenance"
          (check-true (equal? empty-bytes-rope (bytes->rope #"")))))))

  (define ordering-suite
    (test-suite "content-based ordering"
      (test-case "bytes-rope<? matches bytes<?"
        (check-true (bytes-rope<? (bytes->rope #"aaa") (bytes->rope #"aab"))))
      (test-case "ordering is unaffected by tree shape"
        (check-eq? (bytes-rope-compare (bytes->rope #"abc")
                                       (bytes-rope-append (bytes->rope #"ab") (bytes->rope #"c")))
                   '=))))

  (run-suite!
   (test-suite "bytes.rkt"
     oracle-suite structure-suite port-suite contract-suite equal-hash-suite ordering-suite)))
