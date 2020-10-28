#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for DXUQ2 types
(define-type ExprC (U numC binopC ifleq0C idC appC))
(struct numC ([n : Real])                                       #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC])          #:transparent)
(struct ifleq0C ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct appC ([func : Symbol] [arg : ExprC])                    #:transparent)

(struct fundefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

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
    [(list (? symbol? sym) arg) (appC sym (parse arg))]
    [other (error "invalid format DXUQ")]))


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


; parses a s-expression into a fundef
(: parse-fundef (-> Sexp fundefC))
(define (parse-fundef s)
  (match s
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body) (fundefC name arg (parse body))]
    [other (error "invalid format DXUQ")]))


; parses an s-expression into a list of fundef
(: parse-prog (-> Sexp (Listof fundefC)))
(define (parse-prog s)
  (match s
    ['() (error "invalid format DXUQ")]
    [(list a) (cons (parse-fundef a) '())]
    [(? list? l) (cons (parse-fundef (first l)) (parse-prog (rest l)))]
    [other (error "invalid format DXUQ")]))


; takes a listof functions and returns a real
(: interp-fns (-> (Listof fundefC) Real))
(define (interp-fns funs)
  (define main (filter is_main? funs))
  (define functionList (filter not_main? funs))
  (cond
    [(empty? main) (error "invalid format DXUQ")]
    [else (interp (subst (numC 0) 'init (fundefC-body (first main))) functionList)]))


; interprets a DXUQ2 expression and returns a real
(: interp (-> ExprC (Listof fundefC) Real))
(define (interp exp funs)
  (match exp
    [(numC n) n]
    [(binopC op l r) (interp-binop op l r funs)]
    [(ifleq0C test then else) (interp-ifleq0 funs test then else)]
    [(appC func arg) (define fd (get-fundef func funs))
                     (interp (subst (numC (interp arg funs)) (fundefC-arg fd) (fundefC-body fd)) funs)]
    [(idC sym) (error "invalid format DXUQ")]))


; parses a list of function definitions and interprets main
; returns a real number
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


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
    [(appC func arg) (appC func (subst what for arg))]))


; returns the function with the name that matches the given symbol
(: get-fundef (-> Symbol (Listof fundefC) fundefC))
(define (get-fundef sym funs)
  (cond
    [(empty? funs) (error "invalid format DXUQ")]
    [(equal? sym (fundefC-name (first funs))) (first funs)]
    [else (get-fundef sym (rest funs))]))


; predicate that checks if given value is a main function
(define (is_main? [fn : Any]) : Boolean
  (match fn
    [(fundefC 'main 'init body) #t]
    [other #f]))


;;predicate that checks if the functions in list are not main
(define (not_main? [fn : Any]) : Boolean
  (not (is_main? fn)))


;;top-interp test cases
(check-equal? (top-interp '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {f 2}}}) 16)
(check-equal? (top-interp '{{fundef {f x} {+ 5 14}}
                     {fundef {main init} {f 2}}}) 19)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* z 2}}
                            {fundef {main init} {f2 3}}}) 6)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* {- z 1} 2}}
                            {fundef {main init} {f2 3}}}) 4)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* {- z 1} {f 1}}}
                            {fundef {main init} {f2 3}}}) 12)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f1 l} {/ 2 2}}
                            {fundef {f0 l} 5}
                            {fundef {f2 z} {* {- z 1} {f 1}}}
                            {fundef {main init} {f0 3}}}) 5)


;;interp test cases
(check-equal? (interp (numC 1) (parse-prog '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (binopC '* (numC 2) (numC 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 6)
(check-equal? (interp (ifleq0C (numC 0) (numC 1) (numC 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (ifleq0C (numC 1) (numC 1) (numC 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 2)
(check-equal? (interp (appC 'f (numC 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 17)
(check-equal? (interp (appC 'f (binopC '+ (numC 1) (numC 1))) (parse-prog  '{{fundef {f x} {/ x 1}}})) 2)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (idC 'a) (parse-prog '{{fundef {f x} {+ x 14}}}))))


;;interp-fns test cases
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {f 2}}})) 16)
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {+ 1 2}}})) 3)
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {+ 1 2}}})) 3)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}}))))


;;parse test cases
(check-equal? (parse 1) (numC 1))
(check-equal? (parse '(+ 1 2)) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '(- 1 2)) (binopC '- (numC 1) (numC 2)))
(check-equal? (parse '(+ (+ 1 2) 3)) (binopC '+ (binopC '+ (numC 1) (numC 2)) (numC 3)))
(check-equal? (parse '(* (* 1 2) 3)) (binopC '* (binopC '* (numC 1) (numC 2)) (numC 3)))
(check-equal? (parse '(ifleq0 1 2 3)) (ifleq0C (numC 1) (numC 2) (numC 3)))
(check-equal? (parse '(ifleq0 (+ 1 2) 2 3)) (ifleq0C (binopC '+ (numC 1) (numC 2)) (numC 2) (numC 3)))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse '(addone 1)) (appC 'addone (numC 1)))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(1 2 3 4 5))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ / 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ + 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ - 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ undef 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ ifleq0 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ * 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ fundef 3))))


;;parse-fundef test cases
(check-equal? (parse-fundef '{fundef {addone x} {+ x 1}}) (fundefC 'addone 'x (binopC '+ (idC 'x) (numC 1))))
(check-equal? (parse-fundef '{fundef {main init} {f 2}}) (fundefC 'main 'init (appC 'f (numC 2))))
(check-equal? (parse-fundef '{fundef {mult x} {* x 2}}) (fundefC 'mult 'x (binopC '* (idC 'x) (numC 2))))
(check-equal? (parse-fundef '{fundef {main init} {ifleq0 0 1 2}})
              (fundefC 'main 'init (ifleq0C (numC 0) (numC 1) (numC 2))))
(check-equal? (parse-fundef '{fundef {sub z} {- z 1}}) (fundefC 'sub 'z (binopC '- (idC 'z) (numC 1))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-fundef '(1 x (+ x 1)))))


;;parse-prog test cases
(check-equal? (parse-prog '((fundef (f x) (+ x 14))))
              (list (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3))))
              (list (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 'z (binopC '* (numC 2) (numC 3)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3)) (fundef (f3 y) (/ y 1))))
              (list (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 'z (binopC '* (numC 2) (numC 3)))
                    (fundefC 'f3 'y (binopC '/ (idC 'y) (numC 1)))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog 'a)))


;;interp-binop test cases
(check-equal? (interp-binop '- (numC 1) (numC 0) (list
 (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14))))) 1)
(check-equal? (interp-binop '/ (numC 0) (numC 1) (list
 (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14))))) 0)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp-binop '^ (numC 1) (numC 2) (list
 (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))))))


;;get-fundef test cases
(check-equal? (get-fundef 'f2 (list (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 'z (binopC '* (numC 2) (numC 3)))))
              (fundefC 'f2 'z (binopC '* (numC 2) (numC 3))))
(check-equal? (get-fundef 'f (list (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 'z (binopC '* (numC 2) (numC 3)))))
              (fundefC 'f 'x (binopC '+ (idC 'x) (numC 14))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (get-fundef 'x '())))


;;subst test cases
(check-equal? (subst (numC 5) 'f (binopC '+ (idC 'f) (numC 5))) (binopC '+ (numC 5) (numC 5)))
(check-equal? (subst (numC 2) 'f (numC 5)) (numC 5))
(check-equal? (subst (numC 0) 'x (ifleq0C (idC 'x) (numC 1) (numC 2))) (ifleq0C (numC 0) (numC 1) (numC 2)))
(check-equal? (subst (idC 'z) 'x (idC 'x)) (idC 'z))
(check-equal? (subst (idC 'z) 'x (idC 'f)) (idC 'f))
(check-equal? (subst (numC 3) 'x (appC 'f (idC 'x))) (appC 'f (numC 3)))


;;is_main? test cases
(check-equal? (is_main? (fundefC 'main 'init (binopC '+ (numC 1) (numC 2)))) true)
(check-equal? (is_main? (fundefC 'f 'x (binopC '+ (idC 'x) (numC 1)))) false)


;;not_main? test cases
(check-equal? (not_main? (fundefC 'main 'init (binopC '+ (numC 1) (numC 2)))) false)
(check-equal? (not_main? (fundefC 'f 'x (binopC '+ (idC 'x) (numC 1)))) true)















