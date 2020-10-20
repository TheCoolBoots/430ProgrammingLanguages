#lang racket

(require rackunit)

;; 1 Warmup
;; accepts a function and an argument, applies function to argument
(define one (lambda (f)
              (lambda (x) (f x))))

(check-= ((one length) '(1 2 3)) 3 0)
(check-= ((one (lambda (x) 1234)) 10) 1234 0)

;; 2 Continuing on:
;; 1.
;; accepts a function and an argument, applies function to function on argument
(define two (lambda (f)
              (lambda (x) (f (f x)))))

(check-equal? ((two reverse) '(1 2 3)) '(1 2 3))
(check-= ((two (lambda (x) (* x 2))) 2) 8 0)

;; 2.
;; accepts a function and an argument, returns the argument
(define zero (lambda (f)
               (lambda (x) x)))

(check-equal? ((zero reverse) '(1 2 3 4)) '(1 2 3 4))
(check-equal? ((zero first) '(d a v)) '(d a v))

;; 3.
;; applies given function additional time on itself
(define add1 (lambda (nlf)
               (lambda (f)
                 (lambda (x) ((nlf f) (f x))))))

(check-equal? (((add1 one) reverse) '(1 2 3)) '(1 2 3))
(check-equal? (((add1 two) reverse) '(1 2 3)) '(3 2 1))

;; 4.
;; applies first arg to second arg "sum" of args amount of times
(define add (lambda (nlf1)
              (lambda (nlf2)
                (lambda (f)
                  (lambda (x) ((nlf1 f) ((nlf2 f) x)))))))

(check-equal? ((((add two) two) reverse) '(1 2 3)) '(1 2 3))
(check-equal? ((((add two) one) reverse) '(1 2 3)) '(3 2 1))
(check-equal? ((((add one) two) reverse) '(1 2 3)) '(3 2 1))

;; 5.
;; accepts two arguments and returns the first one
(define tru (lambda (a) (lambda (b) a)))

(check-equal? ((tru "david") 123) "david")
(check-equal? ((tru "reko") "david") "reko")

;; 6.
;; accepts two arguments and returns the second one
(define fals (lambda (a) (lambda (b) b)))

(check-= ((fals "david") 123) 123 0)
(check-equal? ((fals "david") "reko") "reko")

;; 7.
;;
(define if (lambda (a) (lambda (b) (lambda (c) ((a b) c)))))

(check-= (((if tru) 5) 4) 5 0)
(check-equal? (((if fals) 10) "david") "david")

;; 8.
(module sim-DXUQ4 racket
    (provide
     (rename-out [lambda fn]
                 [my-let let])
     #%module-begin
     #%datum
     #%app
     + - * / = equal? <=
     if)

  '{let
    ;; rewrite one in DXUQ4
    {one = {fn {f}
             {fn {x} {f x}}}}
    
    ;; rewrite two in DXUQ4
    {two = fn {f}
            {fn {x} {f {f x}}}}  

    ;; rewrite add in DXUQ4
    {add = {fn {nlf1}
             {fn {nlf2}
               {fn {f}
                 {fn {x} {{nlf1 f} {{nlf2 f} x}}}}}}}

    ;; write three
    {three = {fn {f}
               {fn {x} {{{{add two} one} f} x}}}}
    
    in
    {fn {x} {three {* x 2} {x}}}}

  (define-syntax my-let
    (syntax-rules (in =)
      [(my-let [v = e] ... in eb)
       ((lambda (v ...) eb) e ...)])))

