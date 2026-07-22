#lang racket/base

;;; generators.rkt -- reproducible random raw-content generators, and the
;;; concrete `rope-type-ops` instances (see type-ops.rkt) for this project's
;;; pre-defined rope types.

(require racket/contract
         rope/rope
         rope/string
         rope/bytes
         "./type-ops.rkt")

(provide with-seed
         random-string
         random-bytes
         string-ops
         bytes-ops)

;; Runs `thunk` against a fresh, seeded pseudo-random generator, so a
;; benchmark's random fixtures are reproducible run to run.
(define/contract (with-seed seed thunk)
  (-> exact-nonnegative-integer? (-> any) any)
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (thunk)))

(define (random-char) (integer->char (+ 32 (random 95))))

(define/contract (random-string n)
  (-> exact-nonnegative-integer? string?)
  (build-string n (λ (_) (random-char))))

(define/contract (random-bytes n)
  (-> exact-nonnegative-integer? bytes?)
  (apply bytes (for/list ([_ (in-range n)]) (random 256))))

(define string-ops
  (make-rope-type-ops
   #:label          "string"
   #:to-rope        string->rope
   #:to-raw         rope->string
   #:append1        string-rope-append1
   #:empty          empty-string-rope
   #:split          string-rope-split
   #:splice         string-rope-splice
   #:slice          string-rope-slice
   #:offset-index   string-rope-offset-index
   #:to-cursor      string-rope->cursor
   #:cursor-advance string-cursor-advance
   #:cursor-peek    string-cursor-peek
   #:cursor-at-end? string-cursor-at-end?
   #:fold           string-rope-foldl
   #:walk-sequence  (λ (r) (for/sum ([_c (in-string-rope r)]) 1))
   #:compare        string-rope-compare
   #:rope=?         string-rope=?
   #:random-raw     random-string
   #:raw-length     string-length
   #:raw-append     string-append
   #:raw-slice      substring
   #:singletons     (λ (s) (map string (string->list s)))))

(define bytes-ops
  (make-rope-type-ops
   #:label          "bytes"
   #:to-rope        bytes->rope
   #:to-raw         rope->bytes
   #:append1        bytes-rope-append1
   #:empty          empty-bytes-rope
   #:split          bytes-rope-split
   #:splice         bytes-rope-splice
   #:slice          bytes-rope-slice
   #:offset-index   bytes-rope-offset-index
   #:to-cursor      bytes-rope->cursor
   #:cursor-advance bytes-cursor-advance
   #:cursor-peek    bytes-cursor-peek
   #:cursor-at-end? bytes-cursor-at-end?
   #:fold           bytes-rope-foldl
   #:walk-sequence  (λ (r) (for/sum ([_b (in-bytes-rope r)]) 1))
   #:compare        bytes-rope-compare
   #:rope=?         bytes-rope=?
   #:random-raw     random-bytes
   #:raw-length     bytes-length
   #:raw-append     bytes-append
   #:raw-slice      subbytes
   #:singletons     (λ (b) (map bytes (bytes->list b)))))
