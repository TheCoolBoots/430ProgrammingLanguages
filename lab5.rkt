#lang racket

(require rackunit)

;; 1 Warmup
;; accepts a function and an argument, applies function to argument
(define one (lambda (f) (lambda (x) (f x))))

(check-= ((one length) '(1 2 3)) 3 0)
(check-= ((one (lambda (x) 1234)) 10) 1234 0)

;; 2 Continuing on:
;; 1.
;; accepts a function and an argument, applies function to function on argument
(define two (lambda (f x) (f (f x))))

(check-equal? (two reverse '(1 2 3)) '(1 2 3))
(check-= (two (lambda (x) (* x 2)) 2) 8 0)

;; 2.
;; accepts a function and an argument, returns the argument
(define zero (lambda (f x) x))

(check-equal? (zero reverse '(1 2 3 4)) '(1 2 3 4))
(check-equal? (zero first '(d a v)) '(d a v))

;; 3.
;; applies given function additional time on itself
(define add1 (lambda (f) (f f)))

;; 4.
;;

;; 5.
;; accepts two arguments and returns the first one
(define tru (lambda (a b) a))

(check-equal? (tru "david" 123) "david")
(check-equal? (tru "reko" "david") "reko")

;; 6.
;; accepts two arguments and returns the second one
(define fals (lambda (a b) b))

(check-= (fals "david" 123) 123 0)
(check-equal? (fals "david" "reko") "reko")

