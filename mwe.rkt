#lang racket

(require (for-syntax syntax/parse
                     rope2/stxclasses)
         rope2/class
         rope2/generic/define
         rope2/instance
         rope2/type)

(provide (all-defined-out))

(define-rope-type s1 string?)
;; (define-rope-type s2 string?)

;; (define-rope-class foo
;;   ([a id+fun2]
;;    [b nat #:default 12])
;;   (define (*-rope-a x) (list (+ (a #t #f) b) (*-chunk? x)))
;;   (define *-chunk-b (list b #f #f)))

;; (define-rope-instance foo s1
;;   ([a (λ (x y) 23)]))

;; (define-rope-instance foo s2
;;   ([a (λ (x y) 23)]
;;    [b 45]) )

;; (s1-rope-a "")                          ; '(35 #t)
;; (s2-rope-a #t)                          ; '(68 #f)

;; s1-chunk-b                              ; '(12 #f #f)
;; s2-chunk-b                              ; '(45 #f #f)

;; (define-rope-class bar
;;     ([f id+fun1 #:default *-chunk?]
;;      [g id+fun1])
;;   (define (*-chunk-pred1 x) (f x))
;;   (define (*-chunk-pred2 x) (g x)))

;; (define-rope-instance bar s1
;;   ([g values]))

;; (define-rope-instance bar s2
;;   ([g *-chunk?]))

;; (s1-chunk-pred1 "")                     ; #t
;; (s2-chunk-pred2 123)                    ; #f

;; (define-class-op foo (foo-op1 _ x)
;;   (list (*-chunk? x) b))

;; (foo-op1 s1 987)                        ; '(#f 12)
;; (foo-op1 s2 "")                         ; '(#t 45)

(define-rope-class class1 ()
  (displayln 'CLASS1))

;; (define-rope-instance class1 s1 ()) ; 'CLASS1
;; (define-rope-instance class1 s2 ()) ; 'CLASS1

;; (define-class-op class1 (op1 τ)
;;   (displayln `(BBB κ τ)))

;; (op1 s2)                                ; '(BBB class1 s2)

(define-rope-class class2 ([w nat] [x nat] [y nat] [z nat] [a nat])
  #:require (class1)
  (displayln '(CLASS2 w x y z a)))

;; (define-rope-class class3 ([w nat] [x nat] [y nat] [z nat] [a nat])
;;   #:require (class1)
;;   (displayln '(CLASS3 w x y z a)))

;; (define-rope-class class4 ([w nat])
;;   #:require ([class2 #:except (a) #:rename ([x x2])]
;;              [class3 #:rename ([y y3]) #:except (z)])
;;   (displayln '(CLASS4 w x x2 y y3 z a)))

(define-rope-class class5 ([w nat])
  #:require ([class2 #:only (x y z) #:rename ([z P])])
  (displayln '(CLASS5 w x y P)))

(define-rope-instance class1 s1)

(define-rope-instance class2 s1 ([w 0] [x 1] [y 2] [z 3] [a 4]))
;; (define-rope-instance class3 s1 ([w 5] [x 6] [y 7] [z 8] [a 9]))
;; (define-rope-instance class4 s1 ([w 8]))

;; (define-class-op class4 (op-class4 _)
;;   (displayln '(OP-CLASS4 w x x2 y y3 z a)))

;; (op-class4 s1)

(define-rope-instance class5 s1 ([w 9]))



;; (define-rope-class classX1 ()
;;   #:require ([class1 #:except (k)]))

;; (define-rope-class classX2 ()
;;   #:require ([class2 #:rename ([h c])]))

;; (define-rope-instance class2 s1 ([w 0] [x 1] [y 2] [X 999] [z 3] [a 4]))
