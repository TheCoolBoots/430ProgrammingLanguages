#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U numC binopC ifleq0C idC appC))
(struct numC ([n : Real])                                       #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC])          #:transparent)
(struct ifleq0C ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct appC ([func : Symbol] [args : (Listof ExprC)])                    #:transparent)

(struct fundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

(define keywords (list '+ '- '* '/ 'ifleq0 'fundef 'undef))
(define binops (list '+ '- '* '/))


; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(: DXUQ3-keyword? (-> Symbol (Listof Symbol) Boolean))
(define (DXUQ3-keyword? target keywords)
  (cond
    [(empty? keywords) #f]
    [(eq? (first keywords) target) #t]
    [else (DXUQ3-keyword? target (rest keywords))]))


; parses a binary operator expression into a ExprC expression
(: parseBinary (-> Sexp ExprC))
(define (parseBinary exp)
  (match exp
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]))


; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? r) (numC r)]
    [(list (? symbol? sym) l r) #:when (DXUQ3-keyword? sym binops) (parseBinary s)]
    [(list 'ifleq0 test then else) (ifleq0C (parse test) (parse then) (parse else))]
    [(? symbol? sym) (cond
                       [(DXUQ3-keyword? sym keywords) (error "invalid format DXUQ")]
                       [else (idC sym)]
                     )]
    [(list (? symbol? sym)) (appC sym '())]                         ; {functionName}
    [(list (? symbol? sym) arg ...) (appC sym (parseArgList arg))]  ; {functionName ExprC ExprC ...}
    [other (error "invalid format DXUQ")]))


; parses a list of arguments for a function call, returns ExprC
(: parseArgList (-> Sexp (Listof ExprC)))
(define (parseArgList exp)
  (match exp
    [(list a) (cons (parse (first exp)) '())]
    [(list a b ...) (cons (parse a) (parseArgList b))]))


(check-equal? (parse 2) (numC 2))
(check-equal? (parse '(+ 1 2)) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '(- 1 2)) (binopC '- (numC 1) (numC 2)))
(check-equal? (parse '(* 1 2)) (binopC '* (numC 1) (numC 2)))
(check-equal? (parse '(/ 1 2)) (binopC '/ (numC 1) (numC 2)))
(check-equal? (parse 'krill) (idC 'krill))
(check-equal? (parse '(five)) (appC 'five '()))
(check-equal? (parse '(five 2)) (appC 'five (list (numC 2))))
(check-equal? (parse '(ifleq0 1 2 3)) (ifleq0C (numC 1) (numC 2) (numC 3)))
(check-equal? (parse '(five 2 3 4)) (appC 'five (list (numC 2) (numC 3) (numC 4))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(1 2 3 4 5))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ fundef 3))))



; substitute given value recursively into given function
(: subst (-> ExprC Symbol ExprC ExprC))
(define (subst what for in)
  (match in
    [(numC n) in]
    [(binopC op l r) (binopC op (subst what for l) (subst what for r))]
    [(ifleq0C test then else) (ifleq0C (subst what for test)
                                     (subst what for then)
                                     (subst what for else))]
    [(idC sym) (cond
                [(symbol=? sym for) what]
                [else in])]
    [(appC funcName params) (appC funcName (subst-helper what for params))]))


; substitute given value into list of arguments
(: subst-helper (-> ExprC Symbol (Listof ExprC) (Listof ExprC)))
(define (subst-helper what for inList)
  (cond
    [(empty? inList) '()]
    [else (cons (subst what for (first inList)) (subst-helper what for (rest inList))) ]))


;;subst test cases
(check-equal? (subst (numC 5) 'f (binopC '+ (idC 'f) (numC 5))) (binopC '+ (numC 5) (numC 5)))
(check-equal? (subst (numC 2) 'f (numC 5)) (numC 5))
(check-equal? (subst (numC 0) 'x (ifleq0C (idC 'x) (numC 1) (numC 2))) (ifleq0C (numC 0) (numC 1) (numC 2)))
(check-equal? (subst (idC 'z) 'x (idC 'x)) (idC 'z))
(check-equal? (subst (idC 'z) 'x (idC 'f)) (idC 'f))
(check-equal? (subst (numC 3) 'x (appC 'f (list (idC 'x) (idC 'k)))) (appC 'f (list (numC 3) (idC 'k))))
(check-equal? (subst (numC 3) 'x (appC 'f (list (idC 'k) (idC 'x)))) (appC 'f (list (idC 'k) (numC 3))))


; returns the function with the name that matches the given symbol
(: get-fundef (-> Symbol (Listof fundefC) fundefC))
(define (get-fundef sym funs)
  (cond
    [(empty? funs) (error "invalid format DXUQ")]
    [(equal? sym (fundefC-name (first funs))) (first funs)]
    [else (get-fundef sym (rest funs))]))


; interprets a DXUQ2 expression and returns a real
(: interp (-> ExprC (Listof fundefC) Real))
(define (interp exp funs)
  (match exp
    [(numC n) n]
    [(binopC op l r) (interp-binop op l r funs)]
    [(ifleq0C test then else) (interp-ifleq0 funs test then else)]
    [(appC funcName params) (define fd (get-fundef funcName funs))
                     (interp (interp-subst-params params (fundefC-args fd) (fundefC-body fd) funs) funs)]
    [(idC sym) (error "invalid format DXUQ")]))


; interprets a ifleq0 into a Real
(: interp-ifleq0 (-> (Listof fundefC) ExprC ExprC ExprC Real))
(define (interp-ifleq0 funs expIf expThen expElse)
 (cond
   [(<= (interp expIf funs) 0) (interp expThen funs)]
   [else (interp expElse funs)]))


; determine what bin op to perform given symbol and two DXUQ2
(: interp-binop (-> Symbol ExprC ExprC (Listof fundefC) Real))
(define (interp-binop op l r funcs)
  (match op
    ['+ (+ (interp l funcs) (interp r funcs))]
    ['* (* (interp l funcs) (interp r funcs))]
    ['- (- (interp l funcs) (interp r funcs))]
    ['/ (/ (interp l funcs) (interp r funcs))]
    [other (error "invalid format DXUQ")]))


; for each argument in function with funcName,
; subsitute the argument with the corresponding param in params
(: interp-subst-params (-> (Listof ExprC) (Listof Symbol) ExprC (Listof fundefC) ExprC))
(define (interp-subst-params params args targetBody funs)
  (cond
    [(xor (empty? params) (empty? args)) (error "number of parameters does not match number of arguments")]
    [(empty? params) targetBody]
    [else (define newBody (subst (numC (interp (first params) funs)) (first args) targetBody))
          (interp-subst-params (rest params) (rest args) newBody funs)]))


(check-equal? (interp-subst-params (list (numC 5) (numC 3)) (list 'k 'l) (binopC '+ (idC 'k) (idC 'l)) '()) (binopC '+ (numC 5) (numC 3)))
(define testFunctionList (list
                          (fundefC 'test1 '(a b) (binopC '+ (idC 'a) (idC 'b)))
                          (fundefC 'test2 '(c d) (binopC '* (idC 'c) (idC 'd)))
                          ))
(check-equal? (interp (appC 'test1 (list (numC 1) (numC 3))) testFunctionList) 4)







