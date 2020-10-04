#lang typed/racket

(require typed/rackunit)

;; Asgn 2

;; language definitions
(define-type ExprC (U NumC PlusC MultC))
(struct NumC ([num : Real]) #:transparent)
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)

(struct FundefC () #:transparent)

;; parse
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (NumC s)]
    [(list '+ l r) (PlusC (parse l) (parse r))]
    [(list '* l r) (MultC (parse l) (parse r))]
    [other (error 'parse "DXUQ: input is not well-formed, got ~e" s)]
    ))

; parse test cases
(check-equal? (parse 1) (NumC 1))
(check-equal? (parse '{+ 1 2}) (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse '{* 10 20}) (MultC (NumC 10) (NumC 20)))
(check-exn (regexp (regexp-quote "DXUQ: input is not well-formed, got "))
           (lambda () (parse '{1 1 1})))

;; parse-fundef
(define (parse-fundef [s : Sexp]) : FundefC
  (FundefC))

;; parse-prog
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  '())

;; interp
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  -1)

;; interp-fns
(define (interp-fns [funs : (Listof FundefC)]) : Real
  -1)

;; returns a real number as a result of a given program
(define (top-interp [fun-sexps : Sexp]) : Real
  (interp-fns (parse-prog fun-sexps)))