#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     rope2/descriptors
                     rope2/stxclasses
                     syntax/parse)
         syntax/parse/define)

(provide (all-defined-out))

;; -----------------------------------------------------------------------------
;; Class
;; -----------------------------------------------------------------------------

(define-syntax-parse-rule (define-rope-class class-id:id
                            ([member:id stxclass:id (~optional #:optional)] ...)
                            body ...)
  #:with (~var rope:%) (format-id #'class-id "rope:~a" (syntax-e #'class-id))
  (define-syntax rope:%
    (rope-class-descriptor (list (cons #'member #'stxclass) ...) #'(body ...))))

;; -----------------------------------------------------------------------------
;; Instance
;; -----------------------------------------------------------------------------

(begin-for-syntax
  (define type-id-placeholder-rx #px"\\*(?=-(rope|chunk|elem)\\b)")

  (define (subst-placeholders stx type-id)
    (define type-str (symbol->string (syntax-e type-id)))

    (let loop ([stx stx])
      (cond
        [(identifier? stx)
         (define old-name (symbol->string (syntax-e stx)))
         (define new-name (regexp-replace* type-id-placeholder-rx old-name type-str))
         (if (string=? new-name old-name)
             stx
             (datum->syntax stx (string->symbol new-name) stx stx))]
        [(syntax? stx)
         (datum->syntax stx (loop (syntax-e stx)) stx stx)]
        [(pair? stx)
         (cons (loop (car stx)) (loop (cdr stx)))]
        [(vector? stx)
         (list->vector (map loop (vector->list stx)))]
        [else stx]))))

(define-syntax-parse-rule (define-rope-class-instance type-id:id class-id:id
                            (~seq kw:keyword kw-val:expr) ...)
  #:do [(define type-desc-id (format-id #'type-id "rope:~a" (syntax-e #'type-id)))
        (define type-desc
          (or (syntax-local-value type-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected a rope type descriptor"
                                  this-syntax #'type-id)))

        (define equality-desc-id (format-id #'type-id "~a:equality" type-desc-id))
        (define equality-desc
          (or (syntax-local-value equality-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected an equality instance"
                                  this-syntax #'type-id)))
        (define class-desc-id (format-id #'class-id "rope:~a" (syntax-e #'class-id)))
        (define class-desc
          (or (syntax-local-value class-desc-id (λ () #f))
              (raise-syntax-error 'define-rope-class-instance "expected a rope class descriptor")))

        (define (lookup-equality-member x)
          (cdr (assoc x (rope-instance-descriptor-members equality-desc))))

        (define (keyword-syntax->symbol kw-stx)
          (string->symbol (keyword->string (syntax-e kw-stx))))

        ;; class-supplied primitive declarations
        (define (lookup-class-primitive x)
          (cdr (assoc x (rope-class-descriptor-primitives class-desc))))

        ;; instance-supplied primitive bindings
        (define instance-members
          (map cons (map keyword-syntax->symbol (attribute kw)) (attribute kw-val)))

        (define (lookup-instance-member x)
          (cdr (assoc x instance-members)))]

  ;; type members
  #:with *-chunk?          (rope-type-descriptor-chunk?         type-desc)
  #:with *-chunk-limit     (rope-type-descriptor-chunk-limit    type-desc)
  #:with *-chunk-empty     (rope-type-descriptor-chunk-empty    type-desc)
  #:with *-chunk-length    (rope-type-descriptor-chunk-length   type-desc)
  #:with *-chunk-width     (rope-type-descriptor-chunk-width    type-desc)
  #:with *-chunk-ref       (rope-type-descriptor-chunk-ref      type-desc)
  #:with *-chunk-slice     (rope-type-descriptor-chunk-slice    type-desc)
  #:with *-chunk-append    (rope-type-descriptor-chunk-append   type-desc)
  #:with *-elem-width      (rope-type-descriptor-elem-width     type-desc)
  #:with *-make-leaf       (rope-type-descriptor-make-leaf      type-desc)
  #:with *-make-node       (rope-type-descriptor-make-node      type-desc)
  #:with *-chunk=?         (lookup-equality-member 'chunk=?)
  #:with *-chunk-overlap=? (lookup-equality-member 'chunk-overlap=?)
  #:with *-elem=?          (lookup-equality-member 'elem=?)
  #:with *-elem-hash       (lookup-equality-member 'elem-hash)
  #:with *-chunk-hash      (lookup-equality-member 'chunk-hash)
  #:with *-node-hash       (lookup-equality-member 'node-hash)
  #:with *-rope=?          (lookup-equality-member 'rope=?)

  #:with (body* ...) (subst-placeholders (rope-class-descriptor-body class-desc) #'type-id)

  ;; How to bind the current class' primitives in the body?
  ;;
  ;; For example, the total-order class should declare these bindings:
  ;;
  ;; #:with (~var elem<? id+fun2) (lookup-instance-member 'elem<?)
  ;; #:with (~var elem>? id+fun2) (lookup-instance-member 'elem>?)
  ;; #:with ((~optional (~var chunk-compare-overlap id+fun5)))
  ;; (let ([member (lookup-instance-member 'chunk-compare-overlap)])
  ;;   (if member (list member) null))

  (begin body* ...
    ;; Class body goes here. For example, the total-order class should look
    ;; like this (where type-id and class-id are the ones given above in the
    ;; instance definition header):
    ;;
    ;; (define (*-elem<? x y) (elem<? x y))
    ;; (define (*-elem>? x y) (elem>? x y))

    ;; (define *-chunk-compare-overlap
    ;;   (~? chunk-compare-overlap
    ;;       (λ (c d ic id k)
    ;;         (let loop ([i 0])
    ;;           (cond [(= i k) '=]
    ;;                 [(*-elem<? (*-chunk-ref c (+ ic i) (*-chunk-ref d (+id i)))) '<]
    ;;                 [(*-elem>? (*-chunk-ref c (+ ic i) (*-chunk-ref d (+id i)))) '>]
    ;;                 [else (loop (add1 i))])))))

    ;; (define-class-op (rope-compare-with _ _ f:id+fun5 a b)
    ;;   (let loop ([cur-a (rope->mutable-cursor a)]
    ;;              [cur-b (rope->mutable-cursor b)])
    ;;     (cond
    ;;       [(and (not cur-a) (not cur-b)) '=]
    ;;       [(not cur-a) '<]
    ;;       [(not cur-b) '>]
    ;;       [else
    ;;        (define la (mutable-cursor-leaf cur-a))
    ;;        (define lb (mutable-cursor-leaf cur-b))
    ;;        (define pa (mutable-cursor-rel-idx cur-a))
    ;;        (define pb (mutable-cursor-rel-idx cur-b))
    ;;        (define k (min (- (rope-length la) pa) (- (rope-length lb) pb)))
    ;;        (define result (*-chunk-compare-overlap (rope-leaf-chunk la) (rope-leaf-chunk lb) pa pb k))
    ;;        (if (not (eq? result '=))
    ;;            result
    ;;            (loop (cursor-advance! cur-a k) (cursor-advance! cur-b k)))])))

    ;; (define-class-op (rope-compare τ κ a b) (rope-compare-with τ κ chunk-compare-overlap a b))
    ;; (define-class-op (rope<? τ κ a b) (eq? (rope-compare τ κ a b) '<))
    ;; (define-class-op (rope>? τ κ a b) (eq? (rope-compare τ κ a b) '>))
    ;; (define-class-op (rope<=? τ κ a b) (or (rope=? τ a b) (rope<? τ κ a b)))
    ;; (define-class-op (rope>=? τ κ a b) (or (rope=? τ a b) (rope>? τ κ a b)))

    ;; (define (*-rope-compare-with f a b) (rope-compare-with type-id class-id f a b))
    ;; (define (*-rope-compare a b) (rope-compare type-id class-id a b))
    ;; (define (*-rope<? a b) (rope<? type-id class-id a b))
    ;; (define (*-rope>? a b) (rope>? type-id class-id a b))
    ;; (define (*-rope<=? a b) (rope<=? type-id class-id a b))
    ;; (define (*-rope>=? a b) (rope>=? type-id class-id a b))
    ))
