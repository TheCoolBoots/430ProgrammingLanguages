#lang racket
(require rackunit)

; 1. warmup
(define one (lambda (f) (lambda (arg) (f arg))))
(check-equal? ((one (lambda (a) (+ a 1))) 2) 3)

; 2.1
(define two (lambda (function) (lambda (arg) (function (function arg)))))
(check-equal? ((two (lambda (a) (+ a 2))) 2) 6)

; 2.2
(define zero (lambda (function) (lambda (arg) arg)))
(check-equal? ((zero (lambda (a) (+ a 2))) 3) 3)

; 2.3
(define add1 (lambda (nlf)
               (lambda (f)
                 (lambda (x) ((nlf f) (f x))))))

(check-equal? (((add1 one) reverse) '(1 2 3)) '(1 2 3))

; 2.4
(define add (lambda (nf1)
              (lambda (nf2)
                (lambda (f)
                  (lambda (arg) (((nf1 nf2) f) (f arg)))))))

(check-equal? ((((add one) two) reverse) '(1 2 3)) '(3 2 1))

; 2.5
(define tru (lambda (arg1) (lambda (arg2) arg1)))

; 2.6
(define fals (lambda (arg1) (lambda (arg2) arg2)))

; 2.7
(define if (lambda (a) (lambda (b) (lambda (c) ((a b) c)))))

(check-equal? (((if tru) 5) 4) 5)
(check-equal? (((if fals) 5) 4) 4)

; 2.8