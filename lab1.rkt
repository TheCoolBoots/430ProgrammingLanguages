#lang racket

(require rackunit)

; add two numbers together
;(define (add-nums a b)
;  (+ a b))

; returns true if sunny is false or friday is true
(define (==> sunny friday)
  (or (not sunny) friday))

; inserts '_' at the i'th position of string
(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))

; returns the amount of interest generated in a interest period
(define (interest amount)
  (cond
    [(<= amount 1000) (* amount .04)]
    [(<= amount 5000) (* amount .045)]
    [else (* amount .05)]))

(define (divlbs x y)
  (cond
    [(> x y) (/ x y)]
    [else (/ y x)]))



(define myConstant 10)

(check-equal? (* 4 13) 52)
(check-equal? (or false true) true)
(check-equal? (==> false false) true)
(check-equal? (==> true true) true)
(check-equal? (string-insert "" 0) "_")
(check-equal? (string-insert "helloworld" 5) "hello_world")
(check-equal? (string-insert "helloworld" myConstant) "helloworld_")
(check-equal? (interest 1000) 40.0)
(check-equal? (interest 5000) 225.0)
(check-equal? (interest 10000) 500.0)