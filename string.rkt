#lang racket/base

(require racket/contract
         racket/match
         racket/port
         rope/define-rope-type)

(provide (rope-type-out/contract string #:raw string? #:element char?)
         (contract-out [empty-string-rope      string-rope?]
                       [open-input-string-rope (string-rope? . -> . input-port?)]
                       [string-rope-ci-compare (string-rope? string-rope? . -> . (or/c '< '= '>))]
                       [string-rope-ci<?       (string-rope? string-rope? . -> . boolean?)]
                       [string-rope-ci>?       (string-rope? string-rope? . -> . boolean?)]
                       [string-rope-ci<=?      (string-rope? string-rope? . -> . boolean?)]
                       [string-rope-ci>=?      (string-rope? string-rope? . -> . boolean?)]))

(define-rope-type string
  string?
  (λ () 512)
  (λ () "")
  string-length
  string-length
  substring
  (λ (raws) (apply string-append raws))
  string-ref
  #:compare (λ (a b) (cond [(string<? a b) '<] [(string=? a b) '=] [else '>])))

(define empty-string-rope (make-empty-string-rope))

;; Reads one rope element (a character), UTF-8 encoded, per port pull. Each
;; read of k bytes costs O(k). Reading the whole rope costs O(n) overall,
;; (since a character is a small constant number of bytes) without ever
;; flattening the rope into a single string.
(define (open-input-string-rope rope)
  (define active-cursor (box (string-rope->cursor rope)))
  (define pending-bytes (box #""))

  (define (read-in bstr)
    (when (and (zero? (bytes-length (unbox pending-bytes)))
               (not (string-cursor-at-end? (unbox active-cursor))))
      (define cur (unbox active-cursor))
      (define char (string-cursor-peek cur))
      (set-box! pending-bytes (string->bytes/utf-8 (string char)))
      (set-box! active-cursor (string-cursor-advance cur)))

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

  (make-input-port/read-to-peek 'string-rope-input-port read-in #f close))

;; Case-insensitive ordering, reusing the same cursor walk (via
;; string-rope-compare-with) the case-sensitive comparisons use, rather than a
;; second gen:ropeable instance.
(define (string-rope-ci-compare-raw a b)
  (cond [(string-ci<? a b) '<] [(string-ci=? a b) '=] [else '>]))

(define (string-rope-ci-compare a b)
  (string-rope-compare-with string-rope-ci-compare-raw a b))

(define (string-rope-ci<? a b) (eq? (string-rope-ci-compare a b) '<))
(define (string-rope-ci>? a b) (eq? (string-rope-ci-compare a b) '>))
(define (string-rope-ci<=? a b) (not (eq? (string-rope-ci-compare a b) '>)))
(define (string-rope-ci>=? a b) (not (eq? (string-rope-ci-compare a b) '<)))

(module* uncontracted #f
  (provide (rope-type-out string)
           empty-string-rope
           open-input-string-rope))

;;; ---------------------------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------------------------

(module+ test
  (require rackunit
           racket/port
           rope/rope
           "private/testing.rkt")

  (define oracle-suite
    (test-suite "string-rope vs. built-in string operations"
      (test-property "round-trip through string->rope/rope->string" #:trials 300
          ([s (random-string (random 300))])
        (equal? (rope->string (string->rope s)) s))

      (test-property "append1 matches string-append" #:trials 300
          ([a (random-string (random 100))] [b (random-string (random 100))])
        (equal? (rope->string (string-rope-append1 (string->rope a) (string->rope b))) (string-append a b)))

      (test-property "variadic append matches string-append across N parts" #:trials 50
          ([parts (for/list ([_ (in-range (add1 (random 8)))]) (random-string (random 50)))])
        (equal? (rope->string (apply string-rope-append (map string->rope parts))) (apply string-append parts)))

      (test-property "split matches substring on both sides" #:trials 300
          ([s (random-string (add1 (random 200)))])
        (define n (string-length s))
        (define i (random (add1 n)))
        (define-values (l r) (string-rope-split (string->rope s) i))
        (and (equal? (rope->string l) (substring s 0 i)) (equal? (rope->string r) (substring s i n))))

      (test-property "splice matches a substring/string-append oracle" #:trials 300
          ([s (random-string (add1 (random 200)))])
        (define n (string-length s))
        (define start (random (add1 n)))
        (define old-len (random (add1 (- n start))))
        (define new (random-string (random 20)))
        (equal? (rope->string (string-rope-splice (string->rope s) start old-len new))
                (string-append (substring s 0 start) new (substring s (+ start old-len) n))))

      (test-property "slice matches substring" #:trials 300
          ([s (random-string (add1 (random 200)))])
        (define n (string-length s))
        (define start (random (add1 n)))
        (define len (random (add1 (- n start))))
        (equal? (rope->string (string-rope-slice (string->rope s) start len)) (substring s start (+ start len))))

      (test-case "offset-index is the identity when width ≡ count"
        (define r (string->rope "hello world"))
        (check-equal? (string-rope-offset-index r 0) 0)
        (check-equal? (string-rope-offset-index r 4) 4))

      (test-property "cursor-take from a dropped cursor matches substring" #:trials 200
          ([s (random-string (add1 (random 200)))])
        (define n (string-length s))
        (define start (random (add1 n)))
        (define k (random (add1 (- n start))))
        (define cur (string-cursor-drop (string-rope->cursor (string->rope s)) start))
        (equal? (rope->string (string-cursor-take cur k)) (substring s start (+ start k))))))

  (define structure-suite
    (test-suite "structure, Unicode, and coverage"
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

      (test-property "astral-plane Unicode round-trips" #:trials 100
          ([s (random-unicode-string (random 80))])
        (equal? (rope->string (string->rope s)) s))

      (test-property "in-string-rope agrees with string->list" #:trials 100
          ([s (random-string (random 100))])
        (equal? (for/list ([c (in-string-rope (string->rope s))]) c) (string->list s)))))

  (define port-suite
    (test-suite "open-input-string-rope"
      (test-case "matches string->bytes/utf-8 byte for byte, even at tiny buffer sizes"
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

      (test-case "closed ports report closed and reject further reads"
        (define port (open-input-string-rope (string->rope "abc")))
        (close-input-port port)
        (check-true (port-closed? port))
        (check-exn exn:fail? (λ () (read-byte port))))))

  (define contract-suite
    (test-suite "contracted surface"
      (test-case "string-raw-empty returns the actual empty string"
        (check-equal? (string-raw-empty) ""))
      (test-case "make-string-rope-leaf returns a genuinely tagged leaf"
        (check-true (string-rope-leaf? (make-string-rope-leaf "abc"))))

      ;; (test-case "the string-rope-leaf struct constructor enforces string? on raw"
      ;;   (check-not-exn (λ () (string-rope-leaf 3 3 "abc")))
      ;;   (check-exn exn:fail:contract? (λ () (string-rope-leaf 1 1 'not-a-string))))

      (test-case "string-cursor-peek returns #f past the end"
        (check-false (string-cursor-peek (string-cursor-drop (string-rope->cursor (string->rope "abc")) 3))))))

  (define equal-hash-suite
    (let ()
      (define (mixed-build s)
        (string-rope-append (string->rope (substring s 0 3)) (string->rope (substring s 3))))
      (test-suite "content-based equal?/hash"
        (test-case "equal? is content-based across differing tree shapes"
          (define a (string->rope "hello world"))
          (define b (mixed-build "hello world"))
          (check-true  (equal? a b))
          (check-false (equal? a (string->rope "hello worlD"))))

        (test-case "equal-hash-code and equal-secondary-hash-code agree whenever equal? does"
          (define a (string->rope "supercalifragilisticexpialidocious"))
          (define b (mixed-build "supercalifragilisticexpialidocious"))
          (check-equal? (equal-hash-code a) (equal-hash-code b))
          (check-equal? (equal-secondary-hash-code a) (equal-secondary-hash-code b)))

        (test-case "empty ropes are equal regardless of provenance"
          (check-true (equal? empty-string-rope (string->rope ""))))

        (test-case "ropes are usable as hash-table keys"
          (define h (hash-set (hash) (string->rope "key") 'value))
          (check-equal? (hash-ref h (mixed-build "key")) 'value)))))

  (define ordering-suite
    (test-suite "content-based ordering"
      (test-case "string-rope<? / string-rope>? / string-rope<=? / string-rope>=? match string<?/etc."
        (define a (string->rope "apple"))
        (define b (string->rope "banana"))
        (check-true  (string-rope<? a b))
        (check-false (string-rope>? a b))
        (check-true  (string-rope<=? a a))
        (check-true  (string-rope>=? b a))
        (check-true  (string-rope<? (string->rope "ab") (string->rope "abc"))))  ; prefixes order by length

      (test-case "ordering is unaffected by tree shape"
        (check-eq? (string-rope-compare (string->rope "hello world")
                                        (string-rope-append (string->rope "hello ") (string->rope "world")))
                   '=))

      (test-case "case-insensitive comparisons"
        (define a (string->rope "Apple"))
        (define b (string->rope "apple"))
        (check-eq? (string-rope-compare a b) '<)        ; case-sensitive: 'A' < 'a'
        (check-eq? (string-rope-ci-compare a b) '=)
        (check-true (string-rope-ci<=? a b))
        (check-true (string-rope-ci>=? a b)))))

  (run-suite!
   (test-suite "string.rkt"
     oracle-suite structure-suite port-suite contract-suite equal-hash-suite ordering-suite)))
