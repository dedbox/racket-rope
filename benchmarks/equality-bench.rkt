#lang racket

;; rope/benchmarks/equality-bench.rkt
;;
;; Isolates equal? cost by rope size and by *why* two ropes would or
;; wouldn't compare equal. equal? on a rope checks eq?, then hash1/hash2,
;; then (only if those all agree) walks overlapping chunks via
;; *-rope-content=?. The scenarios below are chosen to separate those
;; three tiers:
;;
;;   - identical object   : eq? fires immediately
;;   - same content,
;;     same shape          : hash1/hash2 match; content=? walk still runs,
;;                           but leaf boundaries line up on both sides, so
;;                           each step consumes a whole leaf at once
;;   - same content,
;;     fragmented shape    : hash1/hash2 match; content=? must walk
;;                           genuinely misaligned leaf boundaries end to
;;                           end -- the real stress case for the fallback
;;                           walk, and the only one that should scale with n
;;   - differ at start/end : hash1/hash2 almost certainly disagree (a real
;;                           content difference anywhere collides in a
;;                           60-bit combined hash only astronomically
;;                           rarely), so equal? should return #f in O(1)-ish
;;                           time regardless of n or where the difference is
;;   - rewrapped children  : a brand new top-level node built from `a`'s own
;;                           (rope-node-left a)/(rope-node-right a) -- a
;;                           different, non-eq? object with identical
;;                           content, sharing every leaf with `a`. hash1/
;;                           hash2 match, so this is the one scenario that
;;                           actually reaches the content=? walk while
;;                           having something to skip -- the target case for
;;                           shared-subtree (eq?) skipping in the walk.
;;
;; Raw hashing cost on its own is already covered by poly-hash-bench.rkt;
;; this benchmark is about what equal? does with those hashes once they
;; exist.

;; (require racket/format
;;          rope2/generic-ops
;;          rope2/rope
;;          rope2/type
;;          rope2/string-rope)

;; (define SIZES  '(10 100 1000 10000 100000 1000000 10000000))
;; (define TRIALS 10)

;; (define (format-result ms)
;;   (cond [(>= ms 1.0)    (format "~a msec" (real->decimal-string ms 3))]
;;         [(>= ms 1.0e-3) (format "~a μsec" (real->decimal-string (* ms 1.0e3) 2))]
;;         [else           (format "~a nsec" (real->decimal-string (* ms 1.0e6) 2))]))

;; (define (time-ms thunk)
;;   (define start (current-inexact-monotonic-milliseconds))
;;   (thunk)
;;   (- (current-inexact-monotonic-milliseconds) start))

;; ;; A rope over the same content as (string-chunk->rope s), but built by
;; ;; concatenating deliberately mis-sized pieces (not aligned with the
;; ;; 512-char chunk-limit) so its leaf boundaries land nowhere near the ones
;; ;; chunk->rope would have produced for the same string.
;; (define (fragmented-rope-of s)
;;   (define n (string-length s))
;;   (define piece-size 37) ;; coprime-ish with 512 on purpose
;;   (for/fold ([acc (make-empty-string-rope)])
;;             ([start (in-range 0 n piece-size)])
;;     (define end (min n (+ start piece-size)))
;;     (string-rope-append2 acc (string-chunk->rope (substring s start end)))))

;; ;; A different top-level rope object with identical content, built by
;; ;; re-wrapping `a`'s own two immediate children in a brand new node -- every
;; ;; leaf is shared (eq?) with `a`, only the top node is a fresh allocation.
;; ;; #f if `a` is a single leaf (nothing to rewrap).
;; (define (rewrapped-rope-of a)
;;   (and (rope-node? a)
;;        (string-rope-concat (rope-node-left a) (rope-node-right a))))

;; (define (differing-string s pos)
;;   (define c (string-ref s pos))
;;   (string-append (substring s 0 pos)
;;                  (string (if (char=? c #\a) #\b #\a))
;;                  (substring s (add1 pos))))

;; (define (bench-min thunk)
;;   (apply min (for/list ([_ (in-range TRIALS)]) (time-ms thunk))))

;; (module+ main
;;   (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
;;           (~a "Size"                #:min-width 8)
;;           (~a "identical object"    #:min-width 18 #:align 'right)
;;           (~a "same content/shape"  #:min-width 18 #:align 'right)
;;           (~a "same content/fragmented" #:min-width 22 #:align 'right)
;;           (~a "differ at start"     #:min-width 18 #:align 'right)
;;           (~a "differ at end"       #:min-width 18 #:align 'right)
;;           (~a "rewrapped children"  #:min-width 18 #:align 'right))
;;   (printf "|-\n")

;;   (for ([n (in-list SIZES)])
;;     (define s (make-string n #\a))

;;     (define a (string-chunk->rope s))

;;     (define t-identical (bench-min (λ () (equal? a a))))

;;     (define t-same-shape
;;       (let ([b (string-chunk->rope s)])
;;         (bench-min (λ () (equal? a b)))))

;;     (define t-fragmented
;;       (let ([b (fragmented-rope-of s)])
;;         (bench-min (λ () (equal? a b)))))

;;     (define t-differ-start
;;       (let ([b (string-chunk->rope (differing-string s 0))])
;;         (bench-min (λ () (equal? a b)))))

;;     (define t-differ-end
;;       (let ([b (string-chunk->rope (differing-string s (sub1 n)))])
;;         (bench-min (λ () (equal? a b)))))

;;     (define t-rewrapped
;;       (let ([b (rewrapped-rope-of a)])
;;         (if b (bench-min (λ () (equal? a b))) +nan.0)))

;;     (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
;;             (~a n #:min-width 8)
;;             (~a (format-result t-identical)    #:min-width 18 #:align 'right)
;;             (~a (format-result t-same-shape)   #:min-width 18 #:align 'right)
;;             (~a (format-result t-fragmented)   #:min-width 22 #:align 'right)
;;             (~a (format-result t-differ-start) #:min-width 18 #:align 'right)
;;             (~a (format-result t-differ-end)   #:min-width 18 #:align 'right)
;;             (~a (if (nan? t-rewrapped) "n/a (single leaf)" (format-result t-rewrapped))
;;                 #:min-width 18 #:align 'right))))
