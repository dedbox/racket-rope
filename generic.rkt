#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     rope2/rope-type-descriptor
                     syntax/parse)
         rope2/rope
         syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-operation (gen-id . args) body ...)
  #:with ρ                (format-id this-syntax "ρ")
  #:with chunk?           (format-id this-syntax "chunk?")
  #:with elem-size        (format-id this-syntax "elem-size")
  #:with chunk-limit      (format-id this-syntax "chunk-limit")
  #:with chunk-empty      (format-id this-syntax "chunk-empty")
  #:with chunk-count      (format-id this-syntax "chunk-count")
  #:with chunk-size       (format-id this-syntax "chunk-size")
  #:with chunk-slice      (format-id this-syntax "chunk-slice")
  #:with chunk-append     (format-id this-syntax "chunk-append")
  #:with chunk-ref        (format-id this-syntax "chunk-ref")
  #:with chunk-compare    (format-id this-syntax "chunk-compare")
  #:with chunk-overlap=?  (format-id this-syntax "chunk-overlap=?")
  #:with leaf-constructor (format-id this-syntax "leaf-constructor")
  #:with node-constructor (format-id this-syntax "node-constructor")
  (define-syntax-parse-rule (gen-id ρ . args)
    #:do [(unless (identifier? (attribute gen-id))
            (raise-syntax-error 'define-rope-operation "expected an identifier"
                                this-syntax (attribute gen-id)))
          ;; error reporting helper
          (define (raise-gen-error msg stx)
            (raise-syntax-error 'gen-id msg this-syntax stx))
          ;; format the descriptor's identifier
          (define ρ-stx (format-id (attribute ρ) "rope:~a" (attribute ρ)))
          (unless (identifier? ρ-stx)
            (raise-gen-error "expected an identifier" ρ-stx))
          ;; get the descriptor's value
          (define desc
            (syntax-local-value
             ρ-stx (λ () (raise-gen-error "expected a rope type descriptor" ρ-stx))))
          (unless (rope-type-descriptor? desc)
            (raise-gen-error "expected a rope type descriptor" ρ-stx))]
    #:with chunk?        (rope-type-descriptor-chunk?       desc)
    #:with elem-size     (rope-type-descriptor-elem-size    desc)
    #:with chunk-limit   (rope-type-descriptor-chunk-limit  desc)
    #:with chunk-empty   (rope-type-descriptor-chunk-empty  desc)
    #:with chunk-count   (rope-type-descriptor-chunk-count  desc)
    #:with chunk-size    (rope-type-descriptor-chunk-size   desc)
    #:with chunk-slice   (rope-type-descriptor-chunk-slice  desc)
    #:with chunk-append  (rope-type-descriptor-chunk-append desc)
    #:with chunk-ref     (rope-type-descriptor-chunk-ref    desc)
    #:with chunk-compare (let ([stx (rope-type-descriptor-chunk-compare desc)])
                           (or stx
                               #'(error 'chunk-compare "operation not defined for ~a-rope" 'ρ)))
    #:with chunk-overlap=? (let ([stx (rope-type-descriptor-chunk-overlap=? desc)])
                             (or stx
                                 #'(λ (ac bc ap bp k)
                                     (let loop ([i 0])
                                       (or (= i k)
                                           (and (equal? (chunk-ref ac (+ ap i))
                                                        (chunk-ref bc (+ bp i)))
                                                (loop (add1 i))))))))
    #:with leaf-constructor (rope-type-descriptor-leaf-constructor desc)
    #:with node-constructor (rope-type-descriptor-node-constructor desc)
    (begin body ...)))

(define-rope-operation (rope-chunk?           a)             (chunk?          a))
(define-rope-operation (rope-chunk-limit)                    (chunk-limit))
(define-rope-operation (rope-chunk-empty)                    (chunk-empty))
(define-rope-operation (rope-chunk-count      a)             (chunk-count     a))
(define-rope-operation (rope-chunk-size       a)             (chunk-size      a))
(define-rope-operation (rope-chunk-slice      a s e)         (chunk-slice     a s e))
(define-rope-operation (rope-chunk-append     as)            (chunk-append    as))
(define-rope-operation (rope-chunk-ref        a k)           (chunk-ref       a k))
(define-rope-operation (rope-chunk-compare    a b)           (chunk-compare   a b))
(define-rope-operation (rope-chunk-overlap=?  ac bc ap bp k) (chunk-overlap=? ac bc ap bp k))

(define-rope-operation (rope-elem-size a i)
  (if (number? elem-size) elem-size (elem-size a i)))

(define-rope-operation (make-rope-leaf chunk)
  (leaf-constructor (chunk-count chunk) (chunk-size chunk) chunk))

(define-rope-operation (make-rope-node l r)
  (node-constructor (+ (rope-count l) (rope-count r))
                    (+ (rope-size l) (rope-size r))
                    (add1 (max (rope-depth l) (rope-depth r)))
                    l r))

(define-rope-operation (make-empty-rope) (make-rope-leaf ρ (chunk-empty)))

;; Naive concatenation. O(1)
(define-rope-operation (rope-concat l r) (make-rope-node ρ l r))

;; Concatenation with rebalancing. O(log n) amortized.
(define-rope-operation (rope-append2 l r)
  (cond
    [(zero? (rope-count l)) r]
    [(zero? (rope-count r)) l]
    [else
     (define combined (rope-concat ρ l r))
     (if (rope-balanced? combined)
         combined
         (chunk->rope ρ (rope->chunk ρ combined)))]))

;; O(|as| log n) amortized
(define-rope-operation (rope-append as)
  (for/fold ([l (make-empty-rope ρ)]) ([r (in-list as)])
    (rope-append2 ρ l r)))

;; Splits at an element index. O(log n) amortized
(define-rope-operation (rope-split a0 i0)
  (let loop ([a a0] [i i0])
    (if (rope-leaf? a)
        (let ([count (rope-leaf-count a)]
              [chunk (rope-leaf-chunk a)])
          (values (make-rope-leaf ρ (chunk-slice chunk 0 i))
                  (make-rope-leaf ρ (chunk-slice chunk i count))))
        (let ([l (rope-node-left a)]
              [r (rope-node-right a)])
          (let ([n (rope-count l)])
            (if (<= i n)
                (let-values ([(ll lr) (loop l i)])
                  (values ll (rope-append2 ρ lr r)))
                (let-values ([(rl rr) (loop r (- i n))])
                  (values (rope-append2 ρ l rl) rr))))))))

;;; O(log n)
(define-rope-operation (rope-ref a0 i0)
  (let loop ([a a0] [i i0])
    (cond
      [(rope-leaf? a) (chunk-ref (rope-leaf-chunk a) i)]
      [(rope-node? a)
       (define n (rope-count (rope-node-left a)))
       (if (< i n)
           (loop (rope-node-left a) i)
           (loop (rope-node-right a) (- i n)))])))

;; Finds the left-most element index containing offset p0, clamped to the end
;; of the rope. O(log n)
(define-rope-operation (rope-offset-index a0 p0)
  (let loop ([a a0] [p p0])
    (if (rope-leaf? a)
        (cond
          [(>= p (rope-leaf-size a)) (sub1 (rope-leaf-count a))]
          [(number? elem-size) (quotient p elem-size)]
          [else (let chunk-loop ([p p] [i 0])
                  (let ([k (elem-size (rope-leaf-chunk a) i)])
                    (if (<= p k) i (chunk-loop (- p k) (add1 i)))))])
        (let ([l (rope-node-left a)])
          (if (< p (rope-size l))
              (loop l p)
              (+ (rope-count l) (loop (rope-node-right a) (- p (rope-size l)))))))))

(define-rope-operation (rope-splice a i k chunk)
  (let*-values ([(before rest) (rope-split ρ a i)]
                [(_gone after) (rope-split ρ rest k)])
    (rope-append2 ρ (rope-append2 ρ before (chunk->rope ρ chunk)) after)))

(define-rope-operation (rope-slice a i k)
  (let*-values ([(_before rest) (rope-split ρ a i)]
                [(slice _after) (rope-split ρ rest k)])
    slice))

(define-rope-operation (chunk->rope chunk0)
  (let loop ([chunk chunk0])
    (define n (chunk-count chunk))
    (if (<= n (chunk-limit))
        (make-rope-leaf ρ chunk)
        (let ([mid (quotient n 2)])
          (define l (loop (chunk-slice chunk 0 mid)))
          (define r (loop (chunk-slice chunk mid n)))
          (rope-concat ρ l r)))))

(define-rope-operation (rope->chunk rope)
  (chunk-append (rope-flatten rope)))
