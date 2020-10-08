#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for DXUQ2 types
(define-type ExprC (U numC binopC ifleq0C idC appC))
(struct numC ([n : Real])                                       #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC])          #:transparent)
(struct ifleq0C ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct appC ([func : Symbol] [args : (Listof ExprC)])                    #:transparent)

(struct fundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

(define keywords (list '+ '- '* '/ 'ifleq0 'fundef 'undef))


; takes in s-expr and parses to create DXUQ2
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? r) (numC r)]
    [(list (? symbol? sym) l r) (parseBinary s)]
    [(list 'ifleq0 test then else) (ifleq0C (parse test) (parse then) (parse else))]
    [(? symbol? sym) (cond
                       [(DXUQ2-keyword? sym keywords) (error "invalid format DXUQ")]
                       [else (idC sym)]
                     )]
    [(list (? symbol? sym)) (appC sym '())]
    [(list (? symbol? sym) arg) (appC sym (cons (parse arg) '()))]
    [other (error "invalid format DXUQ")]))

(check-equal? (parse '((five))) (appC 'five '()))
(check-equal? (parse '((five 2))) (appC 'five (list (numC 2))))



; parses a binary operator expression into a DXUQ2 expression
(: parseBinary (-> Sexp ExprC))
(define (parseBinary exp)
  (match exp
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]))


; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(: DXUQ2-keyword? (-> Symbol (Listof Symbol) Boolean))
(define (DXUQ2-keyword? target keywords)
  (cond
    [(empty? keywords) #f]
    [(eq? (first keywords) target) #t]
    [else (DXUQ2-keyword? target (rest keywords))]))













