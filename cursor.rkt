#lang racket/base

;; rope/cursor.rkt

(require rope2/rope)

(provide (all-defined-out))

(struct crumb (side left right) #:transparent)

;; -----------------------------------------------------------------------------
;; Immutable Cursor
;; -----------------------------------------------------------------------------

(struct cursor (leaf index path source) #:transparent)

;; Descends to the leftmost leaf. O(depth) = O(log n)
(define (rope->cursor a0 [i 0])
  (let loop ([a a0] [i i] [path null])
    (if (rope-leaf? a)
        (cursor a i path a0 #f)
        (let* ([l (rope-node-left a)]
               [r (rope-node-right a)]
               [n (rope-length l)])
          (if (< i n)
              (loop l i       (cons (crumb 'left  l r) path))
              (loop r (- i n) (cons (crumb 'right l r) path)))))))

;; O(1) amortized
(define (cursor-advance cur [k 1])
  (define a      (cursor-leaf   cur))
  (define i      (cursor-index  cur))
  (define path   (cursor-path   cur))
  (define a0     (cursor-source cur))
  (define j (+ i k))
  (define n (rope-length a))
  (cond
    [(and (>= j 0) (< j n)) (cursor a j path a0)]
    [(>= j n)               (climb-right path (- j n) a0)]
    [else                   (climb-left  path (- j)   a0)]))

(define (climb-right path k a0)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'right)
     (climb-right (cdr path) k a0)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
     (descend-forward (crumb-right old-cb) k (cons new-cb (cdr path)) a0)]))

(define (descend-forward a k path a0)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (< k n)
          (cursor a k path a0)
          (climb-right path (- k n) a0))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length l)])
        (if (< k m)
            (descend-forward l k       (cons (crumb 'left  l r) path) a0)
            (descend-forward r (- k m) (cons (crumb 'right l r) path) a0)))))

(define (climb-left path k a0)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'left)
     (climb-left (cdr path) k a0)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
     (descend-backward (crumb-left old-cb) k (cons new-cb (cdr path)) a0)]))

(define (descend-backward a k path a0)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (<= k n)
          (cursor a (- n k) path a0)
          (climb-left path (- k n) a0))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length r)])
        (if (<= k m)
            (descend-backward r k       (cons (crumb 'right l r) path) a0)
            (descend-backward l (- k m) (cons (crumb 'left  l r) path) a0)))))

;; O(1) amortized
(define (cursor-retreat cur [k 1]) (cursor-advance cur (- k)))

;; -----------------------------------------------------------------------------
;; Mutable Cursor
;; -----------------------------------------------------------------------------

(struct mutable-cursor (leaf index path source) #:transparent #:mutable)

(define (mutable-cursor->cursor cur)
  (cursor (mutable-cursor-leaf   cur)
          (mutable-cursor-index  cur)
          (mutable-cursor-path   cur)
          (mutable-cursor-source cur)))

(define (rope->mutable-cursor a0 [i 0])
  (let loop ([a a0] [i i] [path null])
    (if (rope-leaf? a)
        (mutable-cursor a i path a0 #f)
        (let* ([l (rope-node-left a)]
               [r (rope-node-right a)]
               [n (rope-length l)])
          (if (< i n)
              (loop l i (cons (crumb 'left l r) path))
              (loop r (- i n) (cons (crumb 'right l r) path)))))))

(define (cursor-advance! cur [k 1])
  (define a    (mutable-cursor-leaf  cur))
  (define i    (mutable-cursor-index cur))
  (define path (mutable-cursor-path  cur))
  (define j (+ i k))
  (define n (rope-length a))
  (cond
    [(and (>= j 0) (< j n)) (set-mutable-cursor-index! cur j) cur]
    [(>= j n)               (climb-right! cur path (- j n))]
    [else                   (climb-left!  cur path (- j))]))

(define (climb-right! cur path k)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'right)
     (climb-right! cur (cdr path) k)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
     (descend-forward! cur (crumb-right old-cb) k (cons new-cb (cdr path)))]))

(define (descend-forward! cur a k path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (< k n)
          (begin (set-mutable-cursor-leaf!  cur a)
                 (set-mutable-cursor-index! cur k)
                 (set-mutable-cursor-path!  cur path)
                 cur)
          (climb-right! cur path (- k n)))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length l)])
        (if (< k m)
            (descend-forward! cur l k       (cons (crumb 'left  l r) path))
            (descend-forward! cur r (- k m) (cons (crumb 'right l r) path))))))

(define (climb-left! cur path k)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'left)
     (climb-left! cur (cdr path) k)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
     (descend-backward! cur (crumb-left old-cb) k (cons new-cb (cdr path)))]))

(define (descend-backward! cur a k path)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (<= k n)
          (begin (set-mutable-cursor-leaf!  cur a)
                 (set-mutable-cursor-index! cur k)
                 (set-mutable-cursor-path!  cur path)
                 cur)
          (climb-left! cur path (- k n)))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length r)])
        (if (<= k m)
            (descend-backward! cur r k       (cons (crumb 'right l r) path))
            (descend-backward! cur l (- k m) (cons (crumb 'left  l r) path))))))
