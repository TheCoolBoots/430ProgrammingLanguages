#lang racket

(require rackunit)

;; 2
; I think that this expression will produce 5
( (lambda (x) (+ x 2)) 3)

; I think that this expression will produce 9
((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

;; 3
;; returns a function that adds 'b' to the input 'a'
(define (curried-add a)
  (lambda (b) (+ a b)))

(check-= ((curried-add 4) 10) 14 0)
(check-= ((curried-add 10) 10) 20 0)
(check-= ((curried-add 4) -4) 0 0)

;; 4
;; returns the result of applying two arguments to input function f
(define (curry2 f)
  (lambda (arg1) (lambda (arg2) (f arg1 arg2))))

(check-= (((curry2 +) 2) 4) 6 0)
(check-= (((curry2 -) 10) -12) 22 0)
(check-equal? (((curry2 string-append) "david") "reko") "davidreko")
(check-equal? (((curry2 eq?) "david") "reko") #f)

;; 5
;; returns the result of applying three arguments to input function f
(define (curry3 f)
  (lambda (arg1) (lambda (arg2) (lambda (arg3) (f arg1 arg2 arg3)))))

(check-= ((((curry3 +) 2) 4) 6) 12 0)
(check-= ((((curry3 -) 10) -12) 22) 0 0)
(check-equal? ((((curry3 string-append) "david") "reko") "11") "davidreko11")

;; 6
;; returns true when a match is found between lst and sym
(define (contains? lst sym)
  (cond
    [(empty? lst) #f]
    [(eq? (first lst) sym) #t]
    [else (contains? (rest lst) sym)]))

(check-equal? (contains? '(a b c) 'a) #t)
(check-equal? (contains? '(a b c) 'x) #f)

;; returns a list of booleans that say for each element in qlst if they
;; exist in symlst
(define (in-list-many? symlst qlst)
  (map (lambda (b) (((curry2 contains?) symlst) b)) qlst))

(check-equal? (in-list-many? '(a b c d) '(f a a c)) '(#f #t #t #t))
(check-equal? (in-list-many? '(a b c) '(d e a)) '(#f #f #t))