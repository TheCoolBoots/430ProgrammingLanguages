#lang racket

(require rackunit)

(define (f x)
  (lambda (a) (lambda (b) (x a b))))
(check-equal? (((f +) 1) 2) 3)


;;takes a func and arg, applies func to arg
(define one (lambda (f) (lambda (arg) (f arg))))


(check-equal? ((one string?) 'abc) #f)
(check-equal? ((one string-length) "hello") 5)
(check-equal? ((one (lambda (x) 1)) 2) 1)


;;takes a func and arg, applies func to result of applying the func to the arg
(define two (lambda (f) (lambda (arg) (f (f arg)))))

(check-equal? ((two string?) 'abc) #f)
(check-equal? ((two (lambda (x) (+ x 1))) 2) 4)


;;takes a func and arg, returns arg
(define zero (lambda (f) (lambda (arg) arg)))

(check-equal? ((zero string?) 'abc) 'abc)
(check-equal? ((zero symbol?) 123) 123)


;;takes two funcs, returns func that applies first func to second func
;(define add


;;takes two args, returns the first arg
(define tru (lambda (a) (lambda (b) a)))

(check-equal? ((tru 1) 2) 1)


;;takes two args, returns the second arg
(define fals (lambda (a) (lambda (b) b)))

(check-equal? ((fals 1) 2) 2)







