#lang typed/racket

(require typed/rackunit)

; definitions for ExprC types
(define-type ExprC (U IdC AppC CondC LamC NumC StrC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct AppC ([target : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct CondC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([ids : (Listof ExprC)] [body : ExprC]) #:transparent)

(define fixed-set (list 'a 'b 'c 'd 'e 'f 'g 'h))
;; returns a random symbol from a fixed set of symbols
(define (random-symbol) : Symbol
  (list-ref fixed-set (random 0 8)))

;; returns a random non-self-referential expression
(define (random-base-term) : ExprC
  (match (random 0 3)
    [0 (NumC (random 0 1000))]
    [1 (StrC (~v (random 0 1000)))]
    [2 (IdC (random-symbol))]))

;; returns a random expression
(define (random-term [depth : Real]) : ExprC
  (cond
    [(= depth 0) (random-base-term)]
    [else (match (random 0 3)
            [0 (AppC (random-term (- depth 1)) (list (random-term(random 0 3))))]
            [1 (CondC (random-term (- depth 1)) (random-term (- depth 1)) (random-term (- depth 1)))]
            [2 (LamC (list (random-term(random 0 3))) (random-term (- depth 1)))])]))

;; takes in a parsed expression and returns the concrete syntax
(define (unparse [exp : ExprC]) : Sexp
  (match exp
    [(NumC n) n]
    [(StrC n) n]
    [(IdC n) n]
    [(AppC target args) (cons (unparse target) (map (lambda (a) (unparse a)) args))] 
    [(CondC if then else) (cons 'if (cons (unparse if) (cons (unparse then) (cons (unparse else) '()))))]
    [(LamC ids body) (cons 'fn (cons (map (lambda (a) (unparse a)) ids) (cons (unparse body) '())))]
    ))

(check-equal? (unparse (NumC 1000)) 1000)
(check-equal? (unparse (StrC "david")) "david")
(check-equal? (unparse (IdC 's)) 's)
(check-equal? (unparse (AppC (IdC 's) (list (IdC 'a) (IdC 'c)))) '(s a c))
(check-equal? (unparse (CondC (IdC 'd) (IdC 's) (IdC 'a))) '(if d s a))
(check-equal? (unparse (LamC (list (IdC 's) (IdC 's1)) (NumC 10))) '(fn (s s1) 10))

;; quiz
(define (quiz) : ExprC
  (define expr (random-term (random 0 3)))
  (printf (~v (unparse expr)))
  expr)

(define secret (quiz))