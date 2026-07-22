#lang racket/base

;;; type-ops.rkt -- the generic vocabulary shared by every benchmark suite in
;;; this directory. A `rope-type-ops` value packages up just enough of one
;;; `define-rope-type` instance's public API (plus a way to make random raw
;;; content) that a benchmark can be written once and run, unmodified, against
;;; any rope type.

(require rope/rope)     ; for `rope-length`, which works on ropes of any type.

(provide (struct-out rope-type-ops)
         make-rope-type-ops
         typed-rope-from
         fragmented-rope-from
         edited-rope-from
         perturb-raw)

(struct rope-type-ops
  (label            ; string -- used as a bench-name / group prefix
   to-rope          ; raw -> rope
   to-raw           ; rope -> raw
   append1          ; rope rope -> rope
   empty            ; rope, the empty rope of this type
   split            ; rope nat -> (values rope rope)
   splice           ; rope nat nat raw -> rope
   slice            ; rope nat nat -> rope
   offset-index     ; rope nat -> nat
   to-cursor        ; rope -> cursor
   cursor-advance   ; cursor -> cursor
   cursor-peek      ; cursor -> any
   cursor-at-end?   ; cursor -> boolean
   fold             ; (any elem -> any) any rope -> any
   walk-sequence    ; rope -> nat, counts elements via this type's `in-*-rope`
   compare          ; rope rope -> (or/c '< '= '>)
   rope=?           ; rope rope -> boolean?, comparator-based (see rope=?)
   random-raw       ; nat -> raw, `n` random raw elements
   raw-length       ; raw -> nat
   raw-append       ; raw raw -> raw
   raw-slice        ; raw nat nat -> raw
   singletons)      ; raw -> (listof raw), one single-element raw per element
  #:transparent)

;; A keyword constructor, so instantiating a 20-field struct (see
;; generators.rkt) is self-documenting and safe against accidental reordering.
(define (make-rope-type-ops
         #:label          label
         #:to-rope        to-rope
         #:to-raw         to-raw
         #:append1        append1
         #:empty          empty
         #:split          split
         #:splice         splice
         #:slice          slice
         #:offset-index   offset-index
         #:to-cursor      to-cursor
         #:cursor-advance cursor-advance
         #:cursor-peek    cursor-peek
         #:cursor-at-end? cursor-at-end?
         #:fold           fold
         #:walk-sequence  walk-sequence
         #:compare        compare
         #:rope=?         rope=?
         #:random-raw     random-raw
         #:raw-length     raw-length
         #:raw-append     raw-append
         #:raw-slice      raw-slice
         #:singletons     singletons)
  (rope-type-ops label to-rope to-raw append1 empty split splice slice offset-index
                 to-cursor cursor-advance cursor-peek cursor-at-end? fold walk-sequence
                 compare rope=? random-raw raw-length raw-append raw-slice singletons))

;; Same content as `raw`, assembled one element at a time via `append1` (the
;; shape of a rope created incrementally by user input). Exercises the
;; amortized rebalancing path in `rope-append1`.
(define (typed-rope-from ops raw)
  (for/fold ([r (rope-type-ops-empty ops)])
            ([piece (in-list ((rope-type-ops-singletons ops) raw))])
    ((rope-type-ops-append1 ops) r ((rope-type-ops-to-rope ops) piece))))

;; Same content as `raw`, assembled from fixed-size fragments via `append1`
;; (the shape typical created by loading a document in chunks (e.g. streamed
;; off disk or network), rather than parsing it all at once.
(define (fragmented-rope-from ops raw #:fragment-size [k 16])
  (define n ((rope-type-ops-raw-length ops) raw))
  (let loop ([i 0] [r (rope-type-ops-empty ops)])
    (if (>= i n)
        r
        (let ([j (min n (+ i k))])
          (define slice ((rope-type-ops-to-rope ops) ((rope-type-ops-raw-slice ops) raw i j)))
          (loop j ((rope-type-ops-append1 ops) r slice))))))

;; A balanced rope built from `raw`, then subjected to `edits` random small
;; point-edits (replace up to 8 elements starting at a random position).
;; Approximates a live-edited document. Exercises `splice`.
(define (edited-rope-from ops raw #:edits [edits ((rope-type-ops-raw-length ops) raw)])
  (for/fold ([r ((rope-type-ops-to-rope ops) raw)]) ([_ (in-range edits)])
    (define len (rope-length r))
    (define start (random (add1 len)))
    (define old-len (random (add1 (min 8 (- len start)))))
    (define new-raw ((rope-type-ops-random-raw ops) (random 8)))
    ((rope-type-ops-splice ops) r start old-len new-raw)))

;; `raw` with the single element at index `i` replaced by one random element.
(define (perturb-raw ops raw i)
  (define n (max 1 ((rope-type-ops-raw-length ops) raw)))
  (define i* (max 0 (min i (sub1 n))))
  ((rope-type-ops-raw-append ops)
   ((rope-type-ops-raw-slice ops) raw 0 i*)
   ((rope-type-ops-raw-append ops)
    ((rope-type-ops-random-raw ops) 1)
    ((rope-type-ops-raw-slice ops) raw (min n (add1 i*)) n))))
