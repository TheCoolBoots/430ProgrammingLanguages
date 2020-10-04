#lang typed/racket

(require typed/rackunit)

;; returns true for a list that contains a number, 'chris, and a symbol, false otherwise
(define (parse000 [s : Sexp]) : Boolean
  (match s
    [(list (? real? r) 'chris (? symbol? sym)) #t]
    [other #f]))

;; test cases for parse000
(check-equal? (parse000 '(1 chris d)) #t)
(check-equal? (parse000 '(123 chris david)) #t)
(check-equal? (parse000 '(chris 123 d)) #f)
(check-equal? (parse000 '(1 chris 1)) #f)

;; returns symbol for a list that contains a number, chris, and a symbol, false otherwise
(define (parse001 [s : Sexp]) : (U Boolean Symbol)
  (match s
    [(list (? real? r) 'chris (? symbol? sym)) sym]
    [other #f]))

;; test cases for parse001
(check-equal? (parse001 '(1 chris d)) 'd)
(check-equal? (parse001 '(1 chris 1)) #f)
(check-equal? (parse001 '(1234 chris chris)) 'chris)
(check-equal? (parse001 '(chris 1234 david)) #f)

;; returns a list of numbers from a given s-expression
(define (parse002 [s : Sexp]) : (Listof Real)
  (match s
    [(list one (list (? real? reals) ...) three) (cast reals (Listof Real))]
    [other '()]))

;; test cases for parse002
(check-equal? (parse002 '(1 2 3)) '())
(check-equal? (parse002 '(david (1 2 3 4) 2)) '(1 2 3 4))
(check-equal? (parse002 '(david david (1 2 3 4))) '())
(check-equal? (parse002 '(david (23 12) (12 23))) '(23 12))

;; returns 'okay if input is a number, error if not
(define (ohno [s : Any]) : Symbol
  (match s
    [(? real?) 'okay]
    [other (error 'ohno "expected real, got ~e" s)]))

;; test cases for ohno
(check-equal? (ohno 1) 'okay)
(check-equal? (ohno 123456) 'okay)
(check-exn (regexp (regexp-quote "expected real, got 'd"))
           (lambda () (ohno 'd)))
(check-exn (regexp (regexp-quote "expected real, got 'd123"))
           (lambda () (ohno 'd123)))


;; ArithC Language Definition
(define-type ArithC (U NumC PlusC MultC))
(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ArithC] [r : ArithC]) #:transparent)
(struct MultC ([l : ArithC] [r : ArithC]) #:transparent)

;; interprets ArithC types values and produces a result, copied from textbook
(define (interp [a : ArithC]) : Real
  (match a
    [(NumC n) n]
    [(PlusC l r) (+ (interp l) (interp r))]
    [(MultC  l r) (* (interp l) (interp r))]))

;; interp test cases
(check-= (interp (NumC 1)) 1 0)
(check-= (interp (MultC (NumC 3) (NumC 4))) 12 0)
(check-= (interp (PlusC (MultC (NumC 4) (NumC 10)) (NumC 12))) 52 0)

;; returns the number of additions in a ArithC value
(define (num-adds [a : ArithC]) : Real
  (match a
    [(NumC n) 0]
    [(PlusC l r) (+ 1 (num-adds l) (num-adds r))]
    [(MultC l r) (+ (num-adds l) (num-adds r))]))

;; num-adds test cases
(check-= (num-adds (NumC 12)) 0 0)
(check-= (num-adds (PlusC (PlusC (NumC 1) (NumC 2)) (PlusC (NumC 4) (NumC 5)))) 3 0)
(check-= (num-adds (MultC (PlusC (NumC 3) (NumC 2)) (NumC 3))) 1 0)
(check-= (num-adds (MultC (NumC 3) (NumC 4))) 0 0)

;; ArithS Definition
(define-type ArithS (U NumS PlusS BMinusS MultS UMinusS))
(struct NumS ([n : Real]) #:transparent)
(struct PlusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct BMinusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct MultS ([l : ArithS] [r : ArithS]) #:transparent)
(struct UMinusS ([n : ArithS]) #:transparent)

;; takes an ArithS value and converts it to an ArithC value for evaluation
(define (desugar [as : ArithS]) : ArithC
  (match as
    [(NumS n) (NumC n)]
    [(PlusS l r) (PlusC (desugar l) (desugar r))]
    [(MultS l r) (MultC (desugar l) (desugar r))]
    [(BMinusS l r) (PlusC (desugar l) (MultC (NumC -1) (desugar r)))]
    [(UMinusS n) (MultC (NumC -1) (desugar n))]))

; desugar test cases
(check-equal? (desugar (NumS 10)) (NumC 10))
(check-equal? (desugar (PlusS (NumS 10) (NumS 20))) (PlusC (NumC 10) (NumC 20)))
(check-equal? (desugar (MultS (NumS 10) (NumS 2))) (MultC (NumC 10) (NumC 2)))
(check-equal? (desugar (BMinusS (NumS 20) (NumS 23))) (PlusC (NumC 20) (MultC (NumC -1) (NumC 23))))
(check-equal? (desugar (UMinusS (NumS 20))) (MultC (NumC -1) (NumC 20)))

;; parses an Sexp to form an ArithS value for desugaring
(define (parse1 [s : Sexp]) : ArithS
  (match s
    [(? real?) (NumS s)]
    [(list '+ l r) (PlusS (parse1 l) (parse1 r))]
    [(list '* l r) (MultS (parse1 l) (parse1 r))]
    [(list '- l r) (BMinusS (parse1 l) (parse1 r))]
    [(list '- n) (UMinusS (parse1 n))]
    [other (error 'parse1 "input is not well-formed, got ~e" s)]))

; parse test cases
(check-equal? (parse1 10) (NumS 10))
(check-equal? (parse1 '{+ 1 2}) (PlusS (NumS 1) (NumS 2)))
(check-equal? (parse1 '{* 2 3}) (MultS (NumS 2) (NumS 3)))
(check-equal? (parse1 '{- 3 4}) (BMinusS (NumS 3) (NumS 4)))
(check-equal? (parse1 '{- 45}) (UMinusS (NumS 45)))

;; accepts an Sexp and returns a real number using the defined language
(define (top-interp [s : Sexp]) : Real
  (interp (desugar (parse1 s))))

; test cases for top-interp
(check-= (top-interp 10) 10 0)
(check-= (top-interp '{+ 10 1}) 11 0)
(check-= (top-interp '{* 12 2}) 24 0)
(check-= (top-interp '{- 15 4}) 11 0)
(check-= (top-interp '{- 13}) -13 0)
(check-exn (regexp (regexp-quote "input is not well-formed, got '(/ 1 1)"))
           (lambda () (top-interp '{/ 1 1})))
(check-= (top-interp '{+ {* 2 3} {+ {* 4 5} {* 8 2}}}) 42 0)
(check-= (top-interp '{* {- 23} 2}) -46 0)
(check-= (top-interp '{+ {* {- 4 2} {- {- 3}}} {* 2 {+ 4 3}}}) 20 0)
(check-= (top-interp '{* 0 {* {* {* 23 45} 543} 1000}}) 0 0)

;; returns ArithC based on Sexp, parse2 = parse1 + desugar
(define (parse2 [s : Sexp]) : ArithC
  (match s
    [(? real?) (NumC s)]
    [(list '+ l r) (PlusC (parse2 l) (parse2 r))]
    [(list '* l r) (MultC (parse2 l) (parse2 r))]
    [(list '- l r) (PlusC (parse2 l) (MultC (NumC -1) (parse2 r)))]
    [(list '- n) (MultC (NumC -1) (parse2 n))]
    [other (error 'parse1 "input is not well-formed, got ~e" s)]))

; test cases for parse2
(check-equal? (parse2 10) (NumC 10))
(check-equal? (parse2 '{+ 1 2}) (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse2 '{* 2 3}) (MultC (NumC 2) (NumC 3)))
(check-equal? (parse2 '{- 3 4}) (PlusC (NumC 3) (MultC (NumC -1) (NumC 4))))
(check-equal? (parse2 '{- 45}) (MultC (NumC -1) (NumC 45)))

;; accepts an Sexp and returns a real with parse2
(define (top-interp2 [s : Sexp]) : Real
  (interp (parse2 s)))

; test cases for top-interp2
(check-= (top-interp2 10) 10 0)
(check-= (top-interp2 '{+ 10 1}) 11 0)
(check-= (top-interp2 '{* 12 2}) 24 0)
(check-= (top-interp2 '{- 15 4}) 11 0)
(check-= (top-interp2 '{- 13}) -13 0)
(check-exn (regexp (regexp-quote "input is not well-formed, got '(/ 1 1)"))
           (lambda () (top-interp2 '{/ 1 1})))
(check-= (top-interp2 '{+ {* 2 3} {+ {* 4 5} {* 8 2}}}) 42 0)
(check-= (top-interp2 '{* {- 23} 2}) -46 0)
(check-= (top-interp2 '{+ {* {- 4 2} {- {- 3}}} {* 2 {+ 4 3}}}) 20 0)
(check-= (top-interp2 '{* 0 {* {* {* 23 45} 543} 1000}}) 0 0)