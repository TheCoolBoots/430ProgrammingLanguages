#lang racket

(require rackunit)


;;takes a func and arg, applies func to arg
(define one (lambda (f)
              (lambda (arg) (f arg))))

(check-equal? ((one string?) 'abc) #f)
(check-equal? ((one string-length) "hello") 5)
(check-equal? ((one (lambda (x) 1)) 2) 1)


;;takes a func and arg, applies func to result of applying the func to the arg
(define two (lambda (f)
              (lambda (arg) (f (f arg)))))

(check-equal? ((two string?) 'abc) #f)
(check-equal? ((two (lambda (x) (+ x 1))) 2) 4)


;;takes a func and arg, returns arg
(define zero (lambda (f)
               (lambda (arg) arg)))

(check-equal? ((zero string?) 'abc) 'abc)
(check-equal? ((zero symbol?) 123) 123)


;;applies function to argument one more time
(define add1 (lambda (nlf)
               (lambda (f)
                 (lambda (x) ((nlf f) (f x))))))

(check-equal? (((add1 one) reverse) '(1 2 3)) '(1 2 3))
(check-equal? (((add1 two) (lambda (x) (+ x 1))) 1) 4)


;;takes two funcs, returns func that applies first func to second func
(define add (lambda (nlf)
              (lambda (nlf2)
                (lambda (f)
                  (lambda (x) ((nlf f) ((nlf2 f) x)))))))

(check-equal? ((((add one) two) reverse) '(1 2 3)) '(3 2 1))
(check-equal? ((((add two) two) reverse) '(1 2 3)) '(1 2 3))


;;takes two args, returns the first arg
(define tru (lambda (a)
              (lambda (b) a)))

(check-equal? ((tru 1) 2) 1)


;;takes two args, returns the second arg
(define fals (lambda (a)
               (lambda (b) b)))

(check-equal? ((fals 1) 2) 2)


;;takes three arguments, returns either sec or third arg
(define if (lambda (a)
             (lambda (b)
               (lambda (c) ((a b) c)))))

(check-equal? (((if tru) 1) 2) 1)
(check-equal? (((if fals) 1) 2) 2)


(module sim-DXUQ4 racket
    (provide
     (rename-out [lambda fn]
                 [my-let let])
     #%module-begin
     #%datum
     #%app
     + - * / = equal? <=
     if)
 
  ;(let
  ;    (one = (fn (func)
  ;               (fn (arg) (func arg))))
  ;    (two = (fn (func
  ;               (fn (arg) (func (func arg)))))
      
    

  
  (define-syntax my-let
    (syntax-rules (in =)
      [(my-let [v = e] ... in eb)
       ((lambda (v ...) eb) e ...)])))





