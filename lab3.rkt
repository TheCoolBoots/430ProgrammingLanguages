#lang typed/racket
(require typed/rackunit)

; Exercise 1
; parse an Sexp
; return true if Sexp contains a number, 'chris, and a symbol in that order 
(: parse000 (-> Sexp Boolean))
(define (parse000 exp)
  (match exp
      [(list (? real? num) 'chris (? symbol? sym)) #t]
      [other #f]))

(check-equal? (parse000 (list 4 'chris 's)) #t)
(check-equal? (parse000 (list 4 'chris 's 'a)) #f)
(check-equal? (parse000 (list 4 'fe 's)) #f)
(check-equal? (parse000 (list 4)) #f)
(check-equal? (parse000 '()) #f)


; Exercise 2
; parse an Sexp
; return the symbol if Sexp contains a number, 'chris, and a symbol in that order 
(: parse001 (-> Sexp (U Boolean Symbol)))
(define (parse001 exp)
  (match exp
      [(list (? real? num) 'chris (? symbol? sym)) sym]
      [other #f]))

(check-equal? (parse001 (list 4 'chris 's)) 's)
(check-equal? (parse001 (list 4 'chris 's 'k)) #f)
(check-equal? (parse001 (list 4 'fe 's)) #f)
(check-equal? (parse001 (list 4)) #f)
(check-equal? (parse001 '()) #f)


; Exercise 3
; parse an Sexp
; return the list if the Sexp is of length 3 and the 2nd element is a list of real numbers
(: parse002 (-> Sexp (Listof Real)))
(define (parse002 exp)
  (match exp
    [(list x (list (? real? listParam) ...) z) (cast listParam (Listof Real))]
    [other '()]))

(check-equal? (parse002 (list 3 (list 1 2 3 4 1) 'k)) (list 1 2 3 4 1))
(check-equal? (parse002 (list 3 (list 1 2 3 4 'f) 'k)) '())
(check-equal? (parse002 (list 3 (list 1 2 3 4 1) 'k 'b)) '())
(check-equal? (parse002 (list 3 (list 1 2 3 4 1))) '())


; Exercise 4
; returns 'okay if input is number, sends error signal if not
(: ohno (-> Any Symbol))
(define (ohno input)
  (match input
    [(? number? input) 'okay]
    [other (error 'ERROR "expected Number but got ~e" input)]))

(check-exn (regexp (regexp-quote "expected Number but got 's"))
           (lambda () (ohno 's)))
(check-equal? (ohno 3) 'okay)


; Exercise 5
(define-type ArithC (U numC plusC multC))
(struct numC ([n : Number]) #:transparent)
(struct plusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct multC ([l : ArithC] [r : ArithC]) #:transparent)

; interpret an ArithC expression
(: interp (-> ArithC Number))
(define (interp expr)
  (match expr
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(check-equal? (interp (plusC (numC 1) (numC 2))) 3)
(check-equal? (interp (multC (numC 1) (numC 2))) 2)
(check-equal? (interp (numC 3)) 3)
(check-equal? (interp (plusC (numC 1) (multC (numC 3) (numC 2)))) 7)


; Exercise 6
(: num-adds (-> ArithC Integer))
(define (num-adds expr)
  (match expr
    [(numC n) 0]
    [(plusC l r) (+ 1 (num-adds l) (num-adds r))]
    [(multC l r) (+ (num-adds l) (num-adds r))]))

(check-equal? (num-adds (plusC (numC 1) (numC 2))) 1)
(check-equal? (num-adds (multC (numC 1) (numC 2))) 0)
(check-equal? (num-adds (numC 3)) 0)
(check-equal? (num-adds (plusC (numC 1) (plusC (numC 3) (numC 2)))) 2)


; Exercise 7
(define-type ArithS (U numS plusS bminusS multS uminusS))
(struct numS ([n : Number]) #:transparent)
(struct plusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct bminusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct uminusS ([l : ArithS]) #:transparent)
(struct multS ([l : ArithS] [r : ArithS]) #:transparent)

; converts ArithS expression into ArithC expression
(: desugar (-> ArithS ArithC))
(define (desugar as)
  (match as
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(bminusS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))] ; (+ l (* -1 r))
    [(uminusS l) (desugar (bminusS (numS 0) l))]))

(check-equal? (desugar (numS 1)) (numC 1))
(check-equal? (desugar (plusS (numS 1)(numS 2))) (plusC (numC 1)(numC 2)))
(check-equal? (desugar (multS (numS 1)(numS 2))) (multC (numC 1)(numC 2)))
(check-equal? (desugar (bminusS (numS 1)(numS 2)))
              (plusC (numC 1) (multC (numC -1)(numC 2))))
(check-equal? (desugar (uminusS (numS 5)))
              (plusC (numC 0) (multC (numC -1) (numC 5))))


; Exercise 8
#|
Arith Grammar:

  Arith	= num
          |{+ Arith Arith}
          |{* Arith Arith}
          |{- Arith Arith}
          |{- Arith}
|#

; converts Sexpr into ArithS expression
(: parse (-> Sexp ArithS))
(define (parse exp)
  (match exp
    [(? number? num) (numS exp)]
    [(list '+ a1 a2) (plusS (parse a1) (parse a2))]
    [(list '* a1 a2) (multS (parse a1) (parse a2))]
    [(list '- a1 a2) (bminusS (parse a1) (parse a2))]
    [(list '- a1) (uminusS (parse a1))]
    [other (error 'ERROR "Unrecognizable expression")]))



(check-equal? (parse '(+ 2 1)) (plusS (numS 2) (numS 1)))
(check-equal? (parse '(* 2 1)) (multS (numS 2) (numS 1)))
(check-equal? (parse '(- 2 1)) (bminusS (numS 2) (numS 1)))
(check-equal? (parse '(- 2)) (uminusS (numS 2)))

(check-equal? (parse '(+ (* 3 2) 1)) (plusS (multS (numS 3) (numS 2)) (numS 1)))
(check-equal? (parse '(+ 1 (* 2 3))) (plusS (numS 1) (multS (numS 2) (numS 3))))

(check-exn (regexp (regexp-quote "Unrecognizable expression"))
           (lambda () (parse '())))
(check-exn (regexp (regexp-quote "Unrecognizable expression"))
           (lambda () (parse '(^ 1 2))))
(check-exn (regexp (regexp-quote "Unrecognizable expression"))
           (lambda () (parse '(* 1 2 3))))


; Exercise 9
; interprets S-expression
(: top-interp (-> Sexp Number))
(define (top-interp exp)
  (interp (desugar (parse exp))))

(check-equal? (top-interp 3) 3)
(check-equal? (top-interp '(+ 1 2)) 3)
(check-equal? (top-interp '(* (+ 1 2) (- 3))) -9)
(check-equal? (top-interp '(- (+ 3 2) (* (- 5) 3))) 20)

(: parse2 (-> Sexp ArithC))
(define (parse2 exp)
  (match exp
    [(? number? num) (numC exp)]
    [(list '+ a1 a2) (plusC (parse2 a1) (parse2 a2))]
    [(list '* a1 a2) (multC (parse2 a1) (parse2 a2))]
    [(list '- a1 a2) (plusC (parse2 a1) (multC (numC -1) (parse2 a2)))]
    [(list '- a1) (plusC (numC 0) (multC (numC -1) (parse2 a1)))]
    [other (error 'ERROR "Unrecognizable expression")]))

(check-equal? (parse2 '(+ (* 3 2) 1)) (plusC (multC (numC 3) (numC 2)) (numC 1)))
(check-equal? (parse2 '(+ 1 (* 2 3))) (plusC (numC 1) (multC (numC 2) (numC 3))))
(check-equal? (parse2 '(- 2 1)) (plusC (numC 2) (multC (numC -1) (numC 1))))
(check-equal? (parse2 '(- 2)) (plusC (numC 0) (multC (numC -1) (numC 2))))
(check-equal? (parse2 3) (numC 3))
(check-exn (regexp (regexp-quote "Unrecognizable expression"))
           (lambda () (parse2 '(* 1 2 3))))