#lang racket
(require rackunit)

;((lambda (x) (+ x 2)) 3)


;((lambda (f g) (f (g 3)))
; (lambda (x) (+ x 3))
; (lambda (x) (* x 2)))


; 3 curried-add
(define (curried-add a)
  (lambda (b) (+ a b)))

(check-equal? ((curried-add 3) 4) 7)

; 4 curry2
(define (curry2 f)
  (lambda (M) (lambda (N) (f M N))))

(check-equal? (((curry2 (lambda (a b) (+ a b))) 3) 4) 7)

; 5 curry3
(define (curry3 f)
  (lambda (A) (lambda (B) (lambda (C) (f A B C)))))

(check-equal? ((((curry3 (lambda (a b c) (+ a b c))) 1) 2) 3) 6)

; 6 contains?
(define (contains? lst sym)
  (cond
    [(empty? lst) #f]
    [(equal? (first lst) sym) #t]
    [else (contains? (rest lst) sym)]))

(define (in-list-many? src qry)
  (map ((curry2 contains?) src) qry))

(check-equal? (in-list-many? '(a b c d) '(f a a c)) '(#f #t #t #t))
(check-equal? (in-list-many? '(a b c) '(d e a)) '(#f #f #t))