#lang racket

(require rackunit)


;;takes a number and returns a function that takes a number, then returns
;;the numbers added together
(define (curried-add a)
  (lambda (b) (+ a b)))

(check-equal? ((curried-add 0) 1) 1)
(check-equal? ((curried-add 1) 2) 3)


;;takes a function of two args, returns result of input function on args
(define (curry2 f)
  (lambda (b) (lambda (c) (f b c))))

(check-equal? (((curry2 +) 1) 2) (+ 1 2))
(check-equal? (((curry2 substring) "Ryan") 1) "yan")
(check-equal? (((curry2 string-append) "hello") "world") "helloworld")
  

;;takes a function of three args, returns result of input function on args
(define (curry3 f)
  (lambda (b) (lambda (c) (lambda (d) (f b c d)))))

(check-equal? ((((curry3 +) 1) 2) 3) (+ (+ 1 2) 3))
(check-equal? ((((curry3 substring) "hello_world") 0) 5) "hello")
(check-equal? ((((curry3 string=?) "a") "a") "b") false)
(check-equal? ((((curry3 string=?) "a") "a") "a") true)
(check-equal? ((((curry3 string-append) "a") "b") "c") "abc")


;;takes a list and symbol, returns true when the symbol occurs in list
(define (contains? l sym)
  (cond
    [(empty? l) false]
    [(equal? (first l) sym) true]
    [else (contains? (rest l) sym)]))

(check-equal? (contains? '(a b c d e f) 'e) true)
(check-equal? (contains? '(a b c d e f) 'g) false)


;;takes a list of symbols and list of query symbols, returns list of booleans
(define (in-list-many? syms query)
  (map (lambda (q) (((curry2 contains?) syms) q)) query))

(check-equal? (in-list-many? '(a b c d e) '(f g a)) '(#f #f #t))
(check-equal? (in-list-many? '(a z) '(a a a)) '(#t #t #t))
(check-equal? (in-list-many? '(b) '(a c d)) '(#f #f #f))
  

