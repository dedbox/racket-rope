#lang racket/base

;; Shared test infrastructure used by every core file's `module+ test`
;; submodule, plus the standalone cross-cutting test files. Nothing in
;; this module is itself exercised by a dedicated test suite -- it is
;; implicitly covered by every file that requires it.

(require (for-syntax racket/base
                     syntax/parse)
         racket/pretty
         racket/string
         racket/vector
         rackunit
         rackunit/text-ui
         syntax/parse/define
         "../rope.rkt"
         "../type.rkt")

(provide (all-defined-out)
         (all-from-out rackunit rackunit/text-ui))

;; -----------------------------------------------------------------------------
;; Suite runner
;; -----------------------------------------------------------------------------

;; Runs a test-suite and fails `raco test` (non-zero exit, clear count) if
;; anything inside it failed, rather than silently reporting via rackunit's
;; own default handler only.
(define (run-suite! suite)
  (define failed (run-tests suite 'verbose))
  (unless (zero? failed) (error 'raco-test "~a check(s) failed" failed)))

;; -----------------------------------------------------------------------------
;; Property-based testing
;; -----------------------------------------------------------------------------

;; Runs `body` `trials` times with fresh random bindings for each `id`, and
;; reports the exact failing inputs (and a pretty-printed form of the
;; expression under test) on failure, rather than just "false was not true".
(define-syntax (test-property stx)
  (syntax-parse stx
    [(_ name:expr #:trials trials:expr ([id:id gen-expr:expr] ...) body:expr ...+)
     #`(test-case name
         (for ([iteration (in-range trials)])
           (let* ([id gen-expr] ...)
             (with-check-info
               (['iteration iteration]
                ['expression (let* ([str (pretty-format '(let () body ...) #:mode 'display)]
                                    [lines (string-split str "\n")]
                                    [indented (cons (car lines)
                                                    (for/list ([line (in-list (cdr lines))])
                                                      (string-append "  " line)))])
                               (string-info (string-join indented "\n")))]
                ['id id] ...)
               #,(syntax/loc stx (check-true (let () body ...)))))))]))

;; -----------------------------------------------------------------------------
;; Testing compile-time failures
;; -----------------------------------------------------------------------------
;;
;; Most of the interesting error paths in this codebase (missing a
;; required field, an unknown #:include/#:exclude/#:rename name, a bad
;; #:default expression, a wrong-arity function argument, ...) fail via
;; raise-syntax-error *during macro expansion* -- i.e. while whatever test
;; submodule calls them is itself being compiled. An ordinary
;; `(check-exn exn:fail? (lambda () ...))` cannot catch this: the
;; exception happens before check-exn's thunk is ever runnable code, and
;; the whole test submodule simply fails to compile instead of reporting
;; one failed check.
;;
;; The fix, generalizing this codebase's own `assert-syntax-class-match!`
;; pattern: attempt the expansion via `local-expand`, inside a macro, and
;; catch the result there -- so the *outcome* (did it fail, and with what
;; message) becomes an ordinary runtime value the enclosing macro can
;; splice in as a literal, which `check-true`/`check-regexp-match`/etc can
;; then test normally.

(begin-for-syntax
  ;; Catches exn:fail broadly, not just exn:fail:syntax. local-expand
  ;; only ever *expands* -- it never evaluates/runs the resulting code --
  ;; so the only exceptions it can raise come from a macro transformer's
  ;; own expansion-time computation (rope-error/raise-syntax-error, but
  ;; also, if something in the expansion path has a bug, an ordinary
  ;; contract violation or arity error). All of these mean "this failed
  ;; to expand" for testing purposes; narrowing to exn:fail:syntax only
  ;; would let a transformer's own internal bug crash the whole test
  ;; file's compilation instead of being reported as one failed check.
  ;; The form is wrapped as (let () form (void)) and expanded in
  ;; 'expression context. That makes it an internal-definition context,
  ;; so definition forms (define-rope-class, define-rope-instance, ...
  ;; which expand to define-syntax and friends) are legal, while
  ;; expression forms (a call to a define-class-op'd macro) work too.
  ;; Expanding a bare definition form directly in 'expression context
  ;; would be rejected for the wrong reason ("not allowed in an
  ;; expression context") and read as a spurious failure.
  (define (try-expand stx)
    (with-handlers ([exn:fail? (λ (e) (cons #f (exn-message e)))])
      (local-expand #`(let () #,stx (void)) 'expression null)
      (cons #t #f))))

;; #t if `form` expands without raising a syntax error, #f otherwise.
;; `form` is never evaluated -- only expanded -- so it is safe to use for
;; forms whose successful expansion would have side effects you don't
;; want in the test.
(define-syntax (expands-ok? stx)
  (syntax-parse stx
    [(_ form) #`(quote #,(car (try-expand #'form)))]))

;; #t if `form` fails to expand with a syntax error, #f if it expands
;; successfully (note the polarity: this is the common case, so it reads
;; as `(check-true (expand-fails? form))`).
(define-syntax (expand-fails? stx)
  (syntax-parse stx
    [(_ form) #`(quote #,(not (car (try-expand #'form))))]))

;; The exn-message string from a failed expansion, or #f if it expanded
;; successfully. Use with check-regexp-match / check-true+regexp-match? to
;; confirm an error names the right thing without pinning its exact
;; wording.
(define-syntax (expand-error-message stx)
  (syntax-parse stx
    [(_ form) #`(quote #,(cdr (try-expand #'form)))]))

;; -----------------------------------------------------------------------------
;; Testing syntax classes directly
;; -----------------------------------------------------------------------------
;;
;; `class` must be an identifier bound (for-syntax, from the caller's
;; point of view) to a define-syntax-class. Usage:
;;
;;   (require (for-syntax rope/stxclasses))
;;   (check-true  (stx-matches? id+fun2 (lambda (x y) x)))
;;   (check-false (stx-matches? id+fun2 (lambda (x) x)))

(define-syntax (stx-matches? stx)
  (syntax-parse stx
    [(_ class:id datum)
     ;; `class` can't simply be written inside a nested syntax-parse
     ;; pattern here: ~var's class slot is resolved as an identifier at
     ;; compile time, not looked up from a variable. So build the probe
     ;; as syntax with the real class identifier spliced in, and let
     ;; local-expand run it -- the same technique private/class.rkt's
     ;; assert-syntax-class-match! uses.
     (define probe
       #`(let-syntax ([probe (λ (s)
                               (syntax-parse (quote-syntax datum)
                                 [(~var _ class) #'(void)]))])
           (probe)))
     #`(quote #,(car (try-expand probe)))]))

;; -----------------------------------------------------------------------------
;; A small synthetic rope type, for testing generic/class/instance
;; machinery without depending on any production type (string, bytes).
;; -----------------------------------------------------------------------------
;;
;; Chunks are vectors, so elements can be anything vector-ref can return;
;; #:elem-width defaults to 1 (fixed-width) but can be overridden to a
;; function for width-weighted coverage (mirrors the old ad hoc
;; "weighted" type that used to live directly in generic-tests.rkt).

(define-syntax-parse-rule
  (define-test-rope-type name:id
    (~optional (~seq #:elem-width ew:expr) #:defaults ([ew #'1]))
    (~optional (~seq #:chunk-limit cl:expr) #:defaults ([cl #'8]))
    (~optional (~seq #:chunk=? chunk=?:expr))
    (~optional (~seq #:elem=? elem=?:expr))
    (~optional (~seq #:elem-hash elem-hash:expr)))
  (define-rope-type name
    #:chunk?       vector?
    #:chunk-limit  cl
    #:chunk-empty  vector
    #:chunk-length vector-length
    #:chunk-ref    vector-ref
    #:chunk-slice  (λ (c i k) (vector-copy c i (+ i k)))
    #:chunk-append (λ (cs) (apply vector-append cs))
    #:elem-width   ew
    (~? (~@ #:chunk=? chunk=?))
    (~? (~@ #:elem=? elem=?))
    (~? (~@ #:elem-hash elem-hash))))

;; -----------------------------------------------------------------------------
;; Raw tree builders -- bypass the type/class system entirely
;; -----------------------------------------------------------------------------
;;
;; cursor.rkt's navigation and rope.rkt's balance predicates never look at
;; a chunk's contents or call any type-descriptor operation; they only
;; read the `rope`/`rope-leaf`/`rope-node` struct fields directly. So
;; their tests build trees by hand, with whatever `equal-fun` the test
;; cares about (or a trivial one when it doesn't), rather than paying for
;; a full define-rope-type.

(define (trivial-equal-fun a b) (eq? a b))

;; chunk : vector; length and width are both (vector-length chunk) --
;; fine for any test that doesn't care about width-weighting specifically.
(define (raw-leaf chunk [equal-fun trivial-equal-fun] [hash-h 0] [hash-p 0])
  (define n (vector-length chunk))
  (rope-leaf n n hash-h hash-p equal-fun chunk))

(define (raw-node l r [equal-fun trivial-equal-fun] [hash-h 0] [hash-p 0])
  (rope-node (+ (rope-length l) (rope-length r))
             (+ (rope-width l) (rope-width r))
             hash-h hash-p equal-fun
             (add1 (max (rope-depth l) (rope-depth r)))
             l r))
