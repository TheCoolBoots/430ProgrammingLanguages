#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U numC stringC ifC idC appC lamC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol]) #:transparent)                                     
(struct appC ([fun : ExprC] [l : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

(define syms (list 'a 'b 'c 'd 'e 'f 'g 'h))

;;gets random symbol from list syms
(define (random-symbol) : Symbol
  (list-ref syms (random 8)))


;;gets a random base expression
(define (random-base-term) : ExprC
  (match (random 3)
    [0 (numC (random 100))]
    [1 (stringC "hello")]
    [2 (idC (random-symbol))]))

;;takes a max-depth and returns an expression
(define (random-term [depth : Integer]) : ExprC
  (cond
    [(equal? depth 0) (random-base-term)]
    [else (match (random 3)
            [0 (appC (random-term (- depth 1)) (get-terms (random 4) (- depth 1)))]
            [1 (lamC (get-syms (- depth 1)) (random-term (- depth 1)))]
            [2 (ifC (random-term (- depth 1)) (random-term (- depth 1)) (random-term (- depth 1)))])]))


;;takes arity and depth and returns list of terms with length arity
(define (get-terms [arity : Integer] [depth : Integer]) : (Listof ExprC)
  (cond
    [(equal? arity 0) '()]
    [else (cons (random-term (- depth 1)) (get-terms (- arity 1) depth))]))


;;creates list of symbols of length arity
(define (get-syms [arity : Real]) : (Listof Symbol)
  (cond
    [(equal? arity 0) '()]
    [else (cons (random-symbol) (get-syms (- arity 1)))]))


;;takes an expression and returns an s-expr
(define (unparse [exp : ExprC]) : Sexp
  (match exp
    [(numC n) n]
    [(idC s) s]
    [(stringC s) s]
    [(ifC if then else) (cons 'if (cons (unparse if) (cons (unparse then) (cons (unparse else) '()))))]
    [(appC fun l) (cons (unparse fun) (map (lambda (x) (unparse x)) l))]
    [(lamC args body) (cons 'fn (cons args (cons (unparse body) '())))]))


(check-equal? (unparse (numC 5)) 5)
(check-equal? (unparse (idC 'a)) 'a)
(check-equal? (unparse (stringC "world")) "world")
(check-equal? (unparse (ifC (numC 0) (idC 'a) (idC 'b))) '(if 0 a b))
(check-equal? (unparse (appC (idC 'a) (list (numC 1) (numC 2)))) '(a 1 2))
(check-equal? (unparse (lamC (list 'a 'b 'c) (numC 0))) '(fn (a b c) 0))


;;creates random expression, prints concrete syntax, returns expr
(define (quiz) : ExprC
  (define expr (random-term (random 3)))
  (print (unparse expr))
  expr)

;;answer to quiz
(define secret (quiz))
