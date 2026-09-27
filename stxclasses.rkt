#lang racket/base

(require racket/list
         racket/syntax
         syntax/parse
         (for-template racket/base))

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; Type / Class Primitives
;; -----------------------------------------------------------------------------

(define-syntax-class (fun arity)
  #:description "a zero-argument function"
  #:opaque
  #:literals (lambda λ)
  (pattern ((~or lambda λ) (args:id ...) . _)
           #:when (= arity (length (attribute args)))))

(define-syntax-class vector-lit
  #:description "a vector literal"
  #:opaque
  (pattern x #:when (vector? (syntax-e #'x))))

(define-syntax-class hash-lit
  #:description "a hash literal"
  #:opaque
  (pattern x #:when (hash? (syntax-e #'x))))

(define-syntax-class box-lit
  #:description "a box literal"
  #:opaque
  (pattern x #:when (box? (syntax-e #'x))))

(define-syntax-class prefab-lit
  #:description "a prefab struct literal"
  #:opaque
  (pattern x #:when (struct? (syntax-e #'x))))

(define-syntax-class self-quoting-lit
  #:description "a self-quoting literal"
  #:opaque
  ;; built-in syntax classes
  (pattern (~or :number :boolean :string :bytes :char :regexp :byte-regexp))
  ;; custom syntax classes
  (pattern (~or :vector-lit :hash-lit :box-lit :prefab-lit)))

(define-syntax-class quotable-lit
  #:description "a quotable literal"
  #:opaque
  (pattern (~or :self-quoting-lit :keyword :id)))

(define-syntax-class lit
  #:description "a literal value"
  #:opaque
  #:literals (quote)
  ;; self-quoting atomic literals
  (pattern :self-quoting-lit)
  ;; quoted literals
  (pattern (quote (~or :quotable-lit (:quotable-lit ...)))))

(define-syntax-class id+fun1
  #:description "an identifier or a one-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 1)))))

(define-syntax-class id+fun2
  #:description "an identifier or a two-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 2)))))

(define-syntax-class id+fun3
  #:description "an identifier or a three-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 3)))))

(define-syntax-class id+fun5
  #:description "an identifier or a five-argument function"
  #:opaque
  (pattern (~or :id (~var _ (fun 5)))))

(define-syntax-class nat+id+fun0
  #:description "a natural number, an identifier, or a zero-argument function"
  #:opaque
  #:attributes (callable)
  (pattern n:nat
           #:attr callable #'(λ () n))
  (pattern (~and callable (~or :id (~var _ (fun 0))))))

(define-syntax-class nat+id+fun2
  #:description "a natural number, an identifier, or a two-argument function"
  #:opaque
  #:attributes (callable)
  (pattern n:nat
           #:attr callable #'(λ (_x _y) n))
  (pattern (~and callable (~or :id (~var _ (fun 2))))))

(define-syntax-class lit+id+fun0
  #:description "a literal value, an identifier, or a zero-argument function"
  #:opaque
  #:attributes (callable)
  (pattern l:lit
           #:attr callable #'(λ () l))
  (pattern (~and callable (~or :id (~var _ (fun 0))))))

;; ;; -----------------------------------------------------------------------------
;; ;; Operation Name
;; ;; -----------------------------------------------------------------------------

;; (define-syntax-class op*
;;   #:attributes (sym stx id)
;;   (pattern (x:id . id:id)
;;            #:with sym (syntax/loc (attribute id) 'x)
;;            #:with stx (syntax/loc (attribute id) #'id)))

;; ;; -----------------------------------------------------------------------------
;; ;; Operation Arguments
;; ;; -----------------------------------------------------------------------------

;; ;; Splits an argument token that may use syntax-parse's "name:class" colon
;; ;; shorthand. Returns (values name-stx class-stx-or-#f). The class part
;; ;; stays a syntax object (never reduced to a bare symbol): its scope is
;; ;; what makes it resolve to the real syntax-class binding (e.g. id+fun5)
;; ;; wherever the result is later spliced, and that scope traces back to
;; ;; whatever module the argument was originally written in (e.g.
;; ;; total-order.rkt, which requires rope2/stxclasses) -- not to whatever
;; ;; module happens to expand op-args itself.
;; (define (split-op-arg-id id)
;;   (define str (symbol->string (syntax-e id)))
;;   (define idx (for/first ([i (in-range (string-length str))]
;;                           #:when (char=? (string-ref str i) #\:))
;;                 i))
;;   (if idx
;;       (values (datum->syntax id (string->symbol (substring str 0 idx)) id id)
;;               (datum->syntax id (string->symbol (substring str (add1 idx))) id id))
;;       (values id #f)))

;; ;; A single identifier can only carry one scope, so a freshly generated
;; ;; name (needed to avoid colliding with op-args' own pattern variable)
;; ;; and the original class name's scope (needed for it to resolve) can't
;; ;; be remerged into one "name:class" token the way the input was
;; ;; written. Explicit ~var takes them as two separate syntax objects
;; ;; instead -- the same technique class.rkt's own member-binding
;; ;; construction uses for exactly this reason.
;; (define (op-arg-inner-pattern inner-name cls)
;;   (if cls
;;       #`(~var #,inner-name #,cls)
;;       inner-name))

;; (define-splicing-syntax-class op-args
;;   #:description "operation arguments"
;;   ;; Arguments ending with ...
;;   (pattern (~seq arg:id ... last-arg:id (~datum ...))
;;            #:do [(define-values (names classes)
;;                    (for/lists (ns cs) ([a (in-list (append (attribute arg)
;;                                                            (list (attribute last-arg))))])
;;                      (split-op-arg-id a)))]
;;            #:with (name ...) (reverse (cdr (reverse names)))
;;            #:with last-name  (last names)
;;            #:with (inner-name ...) (generate-temporaries (reverse (cdr (reverse names))))
;;            #:with inner-last-name  (generate-temporary (last names))
;;            #:with (inner-pattern ...)
;;              (append
;;               (for/list ([in-name (in-list (syntax->list #'(inner-name ...)))]
;;                          [cls (in-list (reverse (cdr (reverse classes))))])
;;                 (op-arg-inner-pattern in-name cls))
;;               (list (op-arg-inner-pattern #'inner-last-name (last classes)))
;;               (list #'(... ...)))
;;            ;; The left and right sides of the inner #:with clause
;;            #:with rebind-pattern  #'(name ... last-name (... ...))
;;            #:with rebind-value    #'(inner-name ... inner-last-name (... ...)))
;;   ;; Fixed arity arguments
;;   (pattern (~seq arg:id ...)
;;            #:do [(define-values (names classes)
;;                    (for/lists (ns cs) ([a (in-list (attribute arg))])
;;                      (split-op-arg-id a)))]
;;            #:with (name ...)       names
;;            #:with (inner-name ...) (generate-temporaries names)
;;            #:with (inner-pattern ...)
;;              (for/list ([in-name (in-list (syntax->list #'(inner-name ...)))]
;;                         [cls (in-list classes)])
;;                (op-arg-inner-pattern in-name cls))
;;            #:with rebind-pattern      #'(name ...)
;;            #:with rebind-value        #'(inner-name ...)))
