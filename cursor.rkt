#lang racket/base

;; rope/cursor.rkt

(require rope2/rope)

(provide (all-defined-out))

(struct crumb (side left right) #:transparent)

;; -----------------------------------------------------------------------------
;; Immutable Cursor
;; -----------------------------------------------------------------------------

(struct cursor (leaf abs-idx rel-idx path source) #:transparent)

;; Descends to the leftmost leaf. O(depth) = O(log n)
(define (rope->cursor a0 [i0 0])
  (if (rope-empty? a0)
      #f
      (let loop ([a a0] [i i0] [path null])
        (if (rope-leaf? a)
            (cursor a i0 i path a0)
            (let* ([l (rope-node-left a)]
                   [r (rope-node-right a)]
                   [n (rope-length l)])
              (if (< i n)
                  (loop l i       (cons (crumb 'left  l r) path))
                  (loop r (- i n) (cons (crumb 'right l r) path))))))))

;; O(1) amortized
(define (cursor-advance cur [k 1])
  (define a    (cursor-leaf cur))
  (define path (cursor-path cur))
  (define j (+ (cursor-rel-idx cur) k))
  (define n (rope-length a))
  (cond
    [(and (>= j 0) (< j n)) (cursor a (+ (cursor-abs-idx cur) k) j path (cursor-source cur))]
    [(>= j n)               (climb-right cur (- j n) k path)]
    [else                   (climb-left  cur (- j)   k path)]))

(define (climb-right cur k k0 path)
  (define a0 (cursor-source cur))
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'right)
     (climb-right cur k k0 (cdr path))]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
     (descend-forward cur (crumb-right old-cb) k k0 (cons new-cb (cdr path)))]))

(define (descend-forward cur a k k0 path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (< k n)
          (cursor a (+ (cursor-abs-idx cur) k0) k path (cursor-source cur))
          (climb-right cur (- k n) k0 path))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length l)])
        (if (< k m)
            (descend-forward cur l k       k0 (cons (crumb 'left  l r) path))
            (descend-forward cur r (- k m) k0 (cons (crumb 'right l r) path))))))

(define (climb-left cur k k0 path)
  (define a0 (cursor-source cur))
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'left)
     (climb-left cur k k0 (cdr path))]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
     (descend-backward cur (crumb-left old-cb) k k0 (cons new-cb (cdr path)))]))

(define (descend-backward cur a k k0 path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (<= k n)
          (cursor a (+ (cursor-abs-idx cur) k0) (- n k) path (cursor-source cur))
          (climb-left cur (- k n) k0 path))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length r)])
        (if (<= k m)
            (descend-backward cur r k       k0 (cons (crumb 'right l r) path))
            (descend-backward cur l (- k m) k0 (cons (crumb 'left  l r) path))))))

;; O(1) amortized
(define (cursor-retreat cur [k 1]) (cursor-advance cur (- k)))

;; -----------------------------------------------------------------------------
;; Mutable Cursor
;; -----------------------------------------------------------------------------

(struct mutable-cursor (leaf abs-idx rel-idx path source) #:transparent #:mutable)

(define (copy-mutable-cursor cur)
  (mutable-cursor (mutable-cursor-leaf    cur)
                  (mutable-cursor-abs-idx cur)
                  (mutable-cursor-rel-idx cur)
                  (mutable-cursor-path    cur)
                  (mutable-cursor-source  cur)))

(define (cursor->mutable-cursor cur)
  (mutable-cursor (cursor-leaf    cur)
                  (cursor-abs-idx cur)
                  (cursor-rel-idx cur)
                  (cursor-path    cur)
                  (cursor-source  cur)))

(define (mutable-cursor->cursor cur)
  (cursor (mutable-cursor-leaf    cur)
          (mutable-cursor-abs-idx cur)
          (mutable-cursor-rel-idx cur)
          (mutable-cursor-path    cur)
          (mutable-cursor-source  cur)))

(define (rope->mutable-cursor a0 [i0 0])
  (if (rope-empty? a0)
      #f
      (let loop ([a a0] [i i0] [path null])
        (if (rope-leaf? a)
            (mutable-cursor a i0 i path a0)
            (let* ([l (rope-node-left a)]
                   [r (rope-node-right a)]
                   [n (rope-length l)])
              (if (< i n)
                  (loop l i (cons (crumb 'left l r) path))
                  (loop r (- i n) (cons (crumb 'right l r) path))))))))

(define (cursor-advance! cur [k 1])
  (define a    (mutable-cursor-leaf    cur))
  (define path (mutable-cursor-path    cur))
  (define j (+ (mutable-cursor-rel-idx cur) k))
  (define n (rope-length a))
  (cond
    [(and (>= j 0) (< j n))
     (set-mutable-cursor-abs-idx! cur (+ (mutable-cursor-abs-idx cur) k))
     (set-mutable-cursor-rel-idx! cur j)
     cur]
    [(>= j n) (climb-right! cur (- j n) k path)]
    [else     (climb-left!  cur (- j)   k path)]))

(define (climb-right! cur k k0 path)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'right)
     (climb-right! cur k k0 (cdr path))]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
     (descend-forward! cur (crumb-right old-cb) k k0 (cons new-cb (cdr path)))]))

(define (descend-forward! cur a k k0 path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (< k n)
          (begin (set-mutable-cursor-leaf!    cur a)
                 (set-mutable-cursor-abs-idx! cur (+ (mutable-cursor-abs-idx cur) k0))
                 (set-mutable-cursor-rel-idx! cur k)
                 (set-mutable-cursor-path!    cur path)
                 cur)
          (climb-right! cur (- k n) k0 path))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length l)])
        (if (< k m)
            (descend-forward! cur l k       k0 (cons (crumb 'left  l r) path))
            (descend-forward! cur r (- k m) k0 (cons (crumb 'right l r) path))))))

(define (climb-left! cur k k0 path)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'left)
     (climb-left! cur k k0 (cdr path))]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
     (descend-backward! cur (crumb-left old-cb) k k0 (cons new-cb (cdr path)))]))

(define (descend-backward! cur a k k0 path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (<= k n)
          (begin (set-mutable-cursor-leaf!    cur a)
                 (set-mutable-cursor-abs-idx! cur (+ (mutable-cursor-abs-idx cur) k0))
                 (set-mutable-cursor-rel-idx! cur (- n k))
                 (set-mutable-cursor-path!    cur path)
                 cur)
          (climb-left! cur (- k n) k0 path))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length r)])
        (if (<= k m)
            (descend-backward! cur r k       k0 (cons (crumb 'right l r) path))
            (descend-backward! cur l (- k m) k0 (cons (crumb 'left  l r) path))))))
