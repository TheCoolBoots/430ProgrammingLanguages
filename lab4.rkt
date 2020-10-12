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
