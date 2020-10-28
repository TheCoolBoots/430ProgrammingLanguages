#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U numC stringC ifC idC fnC appC lamC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct fnC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [l : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)

(define syms (list 'a 'b 'c 'd 'e 'f 'g 'h))

(define (random-symbol) : Symbol
  (define rand (random 8))
  (list-ref syms rand))

(define (random-base-term) : ExprC
  (define rand2 (random 3))
  (cond
    [(= 0 rand2) (numC (random 100))]
    [(= 1 rand2) (stringC "hello")]
    [else (idC (random-symbol))]))


(define (random-term [depth : Real]) : ExprC
  (define arity (random 4))
  (cond
    [(= depth 0) (random-base-term)]
    [else (match (random 3)
            [0 (cond
                 [(= 0 arity) (appC (random-term (- depth 1)) '())]
                 [(= 1 arity) (appC (random-term (- depth 1)) (list (random-term (- depth 1))))]
                 [(= 2 arity) (appC (random-term (- depth 1)) (list (random-term (- depth 1))
                                                                    (random-term (- depth 1))))]
                 [else (appC (random-term (- depth 1)) (list (random-term (- depth 1))
                                                             (random-term (- depth 1))
                                                             (random-term (- depth 1))))])]
            [1 (cond
                 [(= 0 arity) (lamC '() (random-term (- depth 1)))]
                 [(= 1 arity) (lamC (list (random-symbol)) (random-term (- depth 1)))]
                 [(= 2 arity) (lamC (list (random-symbol) (random-symbol)) (random-term (- depth 1)))]
                 [else (lamC (list (random-symbol) (random-symbol) (random-symbol)) (random-term (- depth 1)))])]
            [2 (ifC (random-term (- depth 1)) (random-term (- depth 1)) (random-term (- depth 1)))])]))


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


(define (quiz) : Sexp
  (unparse (random-term (random 3))))


