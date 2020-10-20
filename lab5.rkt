#lang racket
(require rackunit)

; 1. warmup
(define one (lambda (function) (lambda (arg) (function arg))))
(check-equal? (one (lambda (a) (+ 5 a)) 2) 7)

; 2.1
(define two (lambda (function) (lambda (arg) (function (function arg)))))

; 2.2
(define zero (lambda (function) (lambda (arg) (arg))))

; 2.3
(define add1 (lambda (fun1) ((one fun1) fun1)))

; 2.4
(define add (lambda (fun1) (lambda (fun2) (fun1 fun2))))

; 2.5
(define tru (lambda (arg1) (lambda (arg2) (arg1))))

; 2.6
(define fals (lambda (arg1) (lambda (arg2) (arg2))))

; 2.7
(define if (lambda (a) (lambda (b) (lambda (c) ((a b) c)))))

(check-equal? (((if tru) 5) 4) 5)
(check-equal? (((if fals) 5) 4) 4)

; 2.8