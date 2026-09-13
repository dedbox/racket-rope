#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "./private/class/descriptor.rkt"
                     "./private/stxclasses.rkt"
                     "./private/type/descriptor.rkt")
         syntax/parse/define
         "./private/hash.rkt")

(provide (all-defined-out))

(define-syntax-parse-rule (define-rope-Eq-instance type-id:id
                            (~alt (~optional (~seq #:chunk=? chunk=?:id+fun2))
                                  (~optional (~seq #:chunk-overlap=? chunk-overlap=?:id+fun5))
                                  (~optional (~seq #:elem=? elem=?:id+fun2))
                                  (~optional (~seq #:elem-hash elem-hash:id+fun1)))
                            ...)
  #:do [(define (mk* fmt) (format-id (attribute type-id) fmt (syntax-e #'type-id)))

        (define desc-id (format-id (attribute type-id) "rope:~a" (syntax-e #'type-id)))
        (define desc
          (or (syntax-local-value desc-id (λ () #f))
              (raise-syntax-error 'define-rope-Eq-instance "expected a rope type descriptor"
                                  this-syntax #'type-id)))]

  ;; rope class descriptor
  #:with (~var rope:*:%)   (mk* "rope:~a:Eq")

  ;; type primitives
  #:with *-chunk?       (rope-type-descriptor-chunk?       desc)
  #:with *-chunk-limit  (rope-type-descriptor-chunk-limit  desc)
  #:with *-chunk-empty  (rope-type-descriptor-chunk-empty  desc)
  #:with *-chunk-length (rope-type-descriptor-chunk-length desc)
  #:with *-chunk-width  (rope-type-descriptor-chunk-width  desc)
  #:with *-chunk-ref    (rope-type-descriptor-chunk-ref    desc)
  #:with *-chunk-slice  (rope-type-descriptor-chunk-slice  desc)
  #:with *-chunk-append (rope-type-descriptor-chunk-append desc)
  #:with *-elem-width   (rope-type-descriptor-elem-width   desc)
  #:with *-make-leaf    (rope-type-descriptor-make-leaf    desc)
  #:with *-make-node    (rope-type-descriptor-make-node    desc)

  ;; hashing / equality
  #:with *-chunk-hash             (rope-type-descriptor-chunk-hash        desc)
  #:with *-node-hash              (rope-type-descriptor-node-hash         desc)
  #:with *-rope-content=?         (rope-type-descriptor-rope-content=?    desc)

  ;; class primitives
  #:with *-chunk=?         (mk* "~a-chunk=?")
  #:with *-chunk-overlap=? (mk* "~a-chunk-overlap=?")
  #:with *-elem=?          (mk* "~a-elem=?")
  #:with *-elem-hash       (mk* "~a-elem-hash")

  (begin

    ;; -------------------------------------------------------------------------
    ;; Rope Class Descriptor
    ;; -------------------------------------------------------------------------

    (define-syntax rope:*:% (rope-class-descriptor
                             (list (cons 'x-chunk=?         #'*-chunk=?)
                                   (cons 'x-chunk-overlap=? #'*-chunk-overlap=?)
                                   (cons 'x-elem=?          #'*-elem=?)
                                   (cons 'x-elem-hash       #'*-elem-hash))))

    (define (*-chunk=? c d) ((~? chunk=? equal?) c d))
    (define (*-elem=? x y) ((~? elem=? equal?) x y))
    (define (*-elem-hash x) ((~? elem-hash equal-hash-code) x))

    (define (*-chunk-overlap=? c d ic id k)
      (~? (chunk-overlap=? c d ic id k)
          (for/and ([i (in-range k)])
            (*-elem=? (*-chunk-ref c (+ ic i))
                      (*-chunk-ref d (+ id i))))))))
