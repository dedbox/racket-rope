#lang racket/base

;; rope/cursor.rkt

(require rope2/rope)

(provide (all-defined-out))

(struct crumb (side left right) #:transparent)

;; -----------------------------------------------------------------------------
;; Immutable Cursor
;; -----------------------------------------------------------------------------

(struct cursor (leaf index path source dirty?) #:transparent)

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
  (define dirty? (cursor-dirty? cur))
  (define j (+ i k))
  (define n (rope-length a))
  (cond
    [(and (>= j 0) (< j n)) (cursor a j path a0 dirty?)]
    [(>= j n)               (climb-right path (- j n) a0 dirty?)]
    [else                   (climb-left  path (- j)   a0 dirty?)]))

(define (climb-right path k a0 dirty?)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'right)
     (climb-right (cdr path) k a0 dirty?)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
     (descend-forward (crumb-right old-cb) k (cons new-cb (cdr path)) a0 dirty?)]))

(define (descend-forward a k path a0 dirty?)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (< k n)
          (cursor a k path a0 dirty?)
          (climb-right path (- k n) a0 dirty?))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length l)])
        (if (< k m)
            (descend-forward l k       (cons (crumb 'left  l r) path) a0 dirty?)
            (descend-forward r (- k m) (cons (crumb 'right l r) path) a0 dirty?)))))

(define (climb-left path k a0 dirty?)
  (cond
    [(null? path)
     #f]
    [(eq? (crumb-side (car path)) 'left)
     (climb-left (cdr path) k a0 dirty?)]
    [else
     (define old-cb (car path))
     (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
     (descend-backward (crumb-left old-cb) k (cons new-cb (cdr path)) a0 dirty?)]))

(define (descend-backward a k path a0 dirty?)
  (define n (rope-length a))
  (if (rope-leaf? a)
      (if (<= k n)
          (cursor a (- n k) path a0 dirty?)
          (climb-left path (- k n) a0 dirty?))
      (let* ([l (rope-node-left a)]
             [r (rope-node-right a)]
             [m (rope-length r)])
        (if (<= k m)
            (descend-backward r k       (cons (crumb 'right l r) path) a0 dirty?)
            (descend-backward l (- k m) (cons (crumb 'left  l r) path) a0 dirty?)))))

;; O(1) amortized
(define (cursor-retreat cur [k 1]) (cursor-advance cur (- k)))
