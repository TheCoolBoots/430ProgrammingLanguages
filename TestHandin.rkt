#lang typed/racket

(require typed/rackunit)

(+ 3 4)

;; add two numbers together
(define (add-nums [a : Real] [b : Real]) : Real
  (+ a b))

(check-equal? (* 4 13) 52)
(check-equal? (or false true) true)
(check-equal? (add-nums 1 1) 2)
(check-equal? (add-nums 1 2) 3)
(check-equal? (add-nums 12 -1) 11)
(check-equal? (add-nums 12 12) 24)