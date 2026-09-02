#lang racket/base

;; rope/cursor.rkt

(require rope2/rope)

(provide (all-defined-out))

(struct crumb (side left right) #:transparent)

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
              (loop l i (cons (crumb 'left l r) path))
              (loop r (- i n) (cons (crumb 'right l r) path)))))))

;; O(1) amortized
(define (cursor-advance cur [k 1])
  (define leaf (cursor-leaf cur))
  (define i (+ (cursor-index cur) k))
  (if (< i (rope-length leaf))
      (cursor leaf i (cursor-path cur) (cursor-source cur) (cursor-dirty? cur))
      (let climb ([path (cursor-path cur)])
        (cond
          [(null? path)                 ; end of rope
           #f]
          [(eq? (crumb-side (car path)) 'right)
           (climb (cdr path))]
          [else
           (define old-cb (car path))
           (define new-cb (crumb 'right (crumb-left old-cb) (crumb-right old-cb)))
           (let descend ([a (crumb-right old-cb)] [path (cons new-cb (cdr path))])
             (if (rope-leaf? a)
                 (let ([source (cursor-source cur)]
                       [dirty? (cursor-dirty? cur)])
                   (cursor a (- (rope-length leaf) i) path source dirty?))
                 (let ([l (rope-node-left a)]
                       [r (rope-node-right a)])
                   (descend l (cons (crumb 'left l r) path)))))]))))

;; ;; O(1) amortized
;; (define (cursor-retreat cur [k 1])
;;   (define leaf (cursor-leaf cur))
;;   (define i (- (cursor-index cur) k))
;;   (if (>= i 0)
;;       (cursor leaf i (cursor-path cur) (cursor-source cur) (cursor-dirty? cur))
;;       (let climb ([path (cursor-path cur)])
;;         (cond
;;           [(null? path)                 ; start of rope
;;            #f]
;;           [(eq? (crumb-side (car path)) 'left)
;;            (climb (cdr path))]
;;           [else
;;            (define old-cb (car path))
;;            (define new-cb (crumb 'left (crumb-left old-cb) (crumb-right old-cb)))
;;            (let descend ([a (crumb-left old-cb)] [path (cons new-cb (cdr path))])
;;              (if (rope-leaf? a)
;;                  (let ([source (cursor-source cur)]
;;                        [dirty? (cursor-dirty? cur)])
;;                    (cursor a (+ (rope-length leaf) i) path source dirty?))
;;                  (let ([l (rope-node-left a)]
;;                        [r (rope-node-right a)])
;;                    (descend r (cons (crumb 'right l r) path)))))]))))
