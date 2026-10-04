#lang racket/base

(require racket/set
         racket/syntax
         racket/syntax-srcloc
         syntax/id-set
         syntax/parse
         "./class.rkt"
         "./error.rkt"
         "./type.rkt")

(provide (all-defined-out))

(struct rope-instance (bindings) #:transparent)

(define (describe-instance κ-stx τ-stx)
  (with-caller-sub κ-stx (describe-class κ-stx))
  (with-caller-sub τ-stx (describe-type τ-stx))
  (define κ-name (syntax-e κ-stx))
  (define τ-name (syntax-e τ-stx))
  (define desc-id (format-id τ-stx "rope:~a:~a" κ-name τ-name))
  (or (syntax-local-value desc-id (λ () #f))
      (rope-error "no instance of ~a for ~a" κ-name τ-name)))

(define-splicing-syntax-class instance-args
  #:description "instance arguments"
  #:attributes ([key 1] [val 1])
  (pattern (~seq)
           #:attr (key 1) null
           #:attr (val 1) null)
  (pattern (~seq ([key:id val:expr] ...))))

(define (alist-remq-or-fail v lst)
  (unless (list? lst)
    (raise-argument-error 'alist-remq-or-fail "list?" lst))
  (unless (or (null? lst) (pair? (car lst)))
    (raise-argument-error 'alist-remq-or-fail "(or/c null? pair?)" (car lst)))
  (cond [(null? lst) #f]
        [(eq? (caar lst) v) (cdr lst)]
        [else
         (define lst* (alist-remq-or-fail v (cdr lst)))
         (and lst* (cons (car lst) lst*))]))

(define (alist-rename-or-fail v-old v-new lst)
  (unless (list? lst)
    (raise-argument-error 'alist-remq-or-fail "list?" lst))
  (unless (or (null? lst) (pair? (car lst)))
    (raise-argument-error 'alist-rename-or-fail "(or/c null? pair?)" (car lst)))
  (cond [(null? lst) #f]
        [(eq? (caar lst) v-old) (cons (cons v-new (cdar lst)) (cdr lst))]
        [else
         (define lst* (alist-rename-or-fail v-old v-new (cdr lst)))
         (and lst* (cons (car lst) lst*))]))

(define (resolve-imports τ-stx masks)
  (apply append
         (for/list ([mask (in-list masks)])
           (define κ-stx (mask-spec-class mask))
           (define desc (with-caller-sub τ-stx (describe-instance κ-stx τ-stx)))
           (define all-bindings (rope-instance-bindings desc))
           (define-values (includes excludes renames)
             (validate-mask! (mask-spec-has-include? mask)
                             (mask-spec-includes mask)
                             (mask-spec-excludes mask)
                             (mask-spec-renames mask)))
           (for/fold ([bindings null] #:result (reverse bindings))
                     ([binding (in-list all-bindings)])
             (define name (car binding))
             (cond [(or (and includes (not (hash-has-key? includes name)))
                        (hash-has-key? excludes name))
                    bindings]
                   [(hash-ref renames name (λ () #f))
                    => (λ (renaming)
                         (define new-name (syntax-e (cdr renaming)))
                         (cons new-name (cdr binding)))]
                   [else (cons binding bindings)])))))

(define (validate-mask! has-include? include-ids exclude-ids rename-pairs)
  (define include-names (map syntax-e include-ids))
  (define exclude-names (map syntax-e exclude-ids))
  (define from-ids (map car rename-pairs))
  (define from-names (map syntax-e from-ids))
  (define to-names (map syntax-e (map cdr rename-pairs)))
  (define includes
    (and has-include? (make-hasheq (map cons include-names include-ids))))
  (define excludes (make-hasheq (map cons exclude-names exclude-ids)))
  (define renames (make-hasheq (map cons from-names from-ids)))
  (for ([(name id) (in-hash excludes)])
    (when (and includes (hash-has-key? includes name))
      (with-caller-sub id (rope-error "cannot exclude included parameter"))))
  (for ([(name id) (in-hash renames)])
    (when (and includes (hash-has-key? includes name))
      (with-caller-sub id (rope-error "cannot rename included parameter")))
    (when (hash-has-key? excludes name)
      (with-caller-sub id (rope-error "cannot rename excluded parameter"))))
  (values includes excludes (make-hasheq (map cons from-names to-names))))
