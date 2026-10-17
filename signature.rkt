#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "./private/signature.rkt")
         syntax/parse/define)

(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class member-entry
    #:description "a member entry"
    #:attributes (key formals pattern op kind doc)
    (pattern [key:id formals
                     (~alt (~optional (~seq #:realize pattern*:str))
                           (~optional (~seq #:op op*:id))
                           (~optional (~seq #:kind kind*:id))
                           (~optional (~seq #:doc doc*:str)))
                     ...]
             #:fail-unless (or (not (attribute kind*))
                               (memq (syntax-e #'kind*) '(function struct syntax)))
             "expected function, struct, or syntax"
             #:with pattern (or (attribute pattern*)
                                (datum->syntax #'key (default-pattern (syntax-e #'key))))
             #:fail-unless (valid-pattern? (syntax-e #'pattern))
             "a realization pattern needs exactly one ~a and no other ~"
             #:with op (or (attribute op*) #'#f)
             #:with kind (or (attribute kind*) #'function)
             #:with doc (or (attribute doc*) #'#f))))

(define-syntax-parse-rule (define-rope-signature name:id entry:member-entry ...)
  #:fail-when (find-duplicate-key (attribute entry.key)) "duplicate member key"
  (define-syntax name
    (build-signature 'name (list (member-spec 'entry.key 'entry.formals entry.pattern 'entry.op 'entry.kind entry.doc) ...))))

(define-rope-signature rope-signature
  ;; primitives
  [chunk?            (x)                      #:op rope-chunk?]
  [chunk-limit       ()                       #:op rope-chunk-limit]
  [chunk-empty       ()                       #:op rope-chunk-empty]
  [chunk-length      (c)                      #:op rope-chunk-length]
  [chunk-width       (c)                      #:op rope-chunk-width]
  [chunk-ref         (c i)                    #:op rope-chunk-ref]
  [chunk-slice       (c i k)                  #:op rope-chunk-slice]
  [chunk-append      cs                       #:op rope-chunk-append]
  [elem-width        (c i)                    #:op rope-elem-width]

  ;; tree construction
  [rope-leaf         (length width hash-h hash-p equal-fun chunk) #:kind struct]
  [rope-node         (length width hash-h hash-p equal-fun depth left right) #:kind struct]
  [rope-leaf?        (x)]
  [rope-node?        (x)]
  [rope?             (x)]
  ;; smart constructors
  [make-rope-leaf    (c)    #:realize "make-~a-rope-leaf"  #:op make-rope-leaf]
  [make-rope-node    (l r)  #:realize "make-~a-rope-node"  #:op make-rope-node]
  [make-empty-rope   ()     #:realize "make-empty-~a-rope" #:op make-empty-rope]
  ;; hashing
  [elem-hash         (x)                      #:op rope-elem-hash]
  [chunk-hash        (c)                      #:op rope-chunk-hash]
  [node-hash         (l r)                    #:op rope-node-hash]
  [rope-hash         (a)                      #:op rope-hash]
  ;; content-based equality
  [chunk=?           (c d)                    #:op rope-chunk=?]
  [elem=?            (x y)                    #:op rope-elem=?]
  [chunk-overlap=?   (c d ic id k)            #:op rope-chunk-overlap=?]
  [rope=?            (a b)                    #:op rope=?]
  ;; conversions
  [source->rope      (c)    #:realize "source->~a-rope"    #:op source->rope]
  [rope->source      (a)                      #:op rope->source]
  ;; core operations
  [rope-concat       (a b)                    #:op rope-concat]
  [rope-append2      (a b)                    #:op rope-append2]
  [rope-append       (as)                     #:op rope-append]
  [rope-split        (a i)                    #:op rope-split]
  [rope-ref          (a i)                    #:op rope-ref]
  [rope-offset-index (a p)                    #:op rope-offset-index]
  [rope-cut          (a i k)                  #:op rope-cut]
  [rope-slice        (a i k)                  #:op rope-slice]
  [rope-splice       (a i k b)                #:op rope-splice]
  ;; immutable cursors
  [cursor->rope      (cur)  #:realize "cursor->~a-rope"    #:op cursor->rope]
  [cursor-peek       (cur)                    #:op cursor-peek]
  [cursor-split      (cur)                    #:op cursor-split]
  ;; mutable cursors
  [mutable-cursor->rope (cur) #:realize "mutable-~a-cursor->rope" #:op mutable-cursor->rope]
  [mutable-cursor-peek  (cur) #:realize "mutable-~a-cursor-peek"  #:op mutable-cursor-peek]
  ;; folds
  [rope-foldl        (proc init a . as)       #:op rope-foldl]
  [rope-foldr        (proc init a . as)       #:op rope-foldr]
  ;; sequences
  [in-rope           (a)    #:realize "in-~a-rope"   #:kind syntax]
  [in-cursor         (cur)  #:realize "in-~a-cursor" #:kind syntax])
