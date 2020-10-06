#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for DXUQ2 types
(define-type DXUQ2 (U num binop ifleq0 id app))
(struct num ([n : Real])                                       #:transparent)
(struct binop ([op : Symbol] [l : DXUQ2] [r : DXUQ2])          #:transparent)
(struct ifleq0 ([test : DXUQ2] [then : DXUQ2] [else : DXUQ2])  #:transparent)
(struct id ([s : Symbol])                                      #:transparent)
(struct app ([func : Symbol] [arg : DXUQ2])                    #:transparent)

(struct fundef ([name : Symbol] [arg : Symbol] [body : DXUQ2]) #:transparent)

(define keywords (list '+ '- '* '/ 'ifleq0 'fundef 'undef))


; takes in s-expr and parses to create DXUQ2
(: parse (-> Sexp DXUQ2))
(define (parse s)
  (match s
    [(? real? r) (num r)]
    [(list (? symbol? sym) l r) (parseBinary s)]
    [(list 'ifleq0 test then else) (ifleq0 (parse test) (parse then) (parse else))]
    [(? symbol? sym) (cond
                       [(DXUQ2-keyword? sym keywords) (error "invalid format DXUQ")]
                       [else (id sym)]
                     )]
    [(list (? symbol? sym) arg) (app sym (parse arg))]
    [other (error "invalid format DXUQ")]))


; parses a binary operator expression into a DXUQ2 expression
(: parseBinary (-> Sexp DXUQ2))
(define (parseBinary exp)
  (match exp
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '/ l r) (binop '/ (parse l) (parse r))]))


; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(: DXUQ2-keyword? (-> Symbol (Listof Symbol) Boolean))
(define (DXUQ2-keyword? target keywords)
  (cond
    [(empty? keywords) #f]
    [(eq? (first keywords) target) #t]
    [else (DXUQ2-keyword? target (rest keywords))]))


; parses a s-expression into a fundef
(: parse-fundef (-> Sexp fundef))
(define (parse-fundef s)
  (match s
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body) (fundef name arg (parse body))]
    [other (error "invalid format DXUQ")]))


; parses an s-expression into a list of fundef
(: parse-prog (-> Sexp (Listof fundef)))
(define (parse-prog s)
  (match s
    ['() (error "invalid format DXUQ")]
    [(list a) (cons (parse-fundef a) '())]
    [(? list? l) (cons (parse-fundef (first l)) (parse-prog (rest l)))]
    [other (error "invalid format DXUQ")]))


; takes a listof functions and returns a real
(: interp-fns (-> (Listof fundef) Real))
(define (interp-fns funs)
  (define main (filter is_main? funs))
  (define functionList (filter not_main? funs))
  (cond
    [(empty? main) (error "invalid format DXUQ")]
    [else (interp (subst (num 0) 'init (fundef-body (first main))) functionList)]))


; interprets a DXUQ2 expression and returns a real
(: interp (-> DXUQ2 (Listof fundef) Real))
(define (interp exp funs)
  (match exp
    [(num n) n]
    [(binop op l r) (interp-binop op l r funs)]
    [(ifleq0 test then else) (interp-ifleq0 funs test then else)]
    [(app func arg) (interp-function func arg funs)]
    [(id sym) (error "invalid format DXUQ")]))


; parses a list of function definitions and interprets main
; returns a real number
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


; interprets a function call into a Real
(: interp-function (-> Symbol DXUQ2 (Listof fundef) Real))
(define (interp-function func arg funs)
  (define fd (get-fundef func funs))
  (interp (subst (num (interp arg funs)) (fundef-arg fd) (fundef-body fd)) funs))


; interprets a ifleq0 into a Real
(: interp-ifleq0 (-> (Listof fundef) DXUQ2 DXUQ2 DXUQ2 Real))
(define (interp-ifleq0 funs expIf expThen expElse)
 (cond
   [(<= (interp expIf funs) 0) (interp expThen funs)]
   [else (interp expElse funs)]))


; determine what bin op to perform given symbol and two DXUQ2
(: interp-binop (-> Symbol DXUQ2 DXUQ2 (Listof fundef) Real))
(define (interp-binop op l r funcs)
  (match op
    ['+ (+ (interp l funcs) (interp r funcs))]
    ['* (* (interp l funcs) (interp r funcs))]
    ['- (- (interp l funcs) (interp r funcs))]
    ['/ (/ (interp l funcs) (interp r funcs))]
    [other (error "invalid format DXUQ")]))


; substitute given value recursively into given function
(: subst (-> DXUQ2 Symbol DXUQ2 DXUQ2))
(define (subst what for in)
  (match in
    [(num n) in]
    [(binop op l r) (binop op (subst what for l) (subst what for r))]
    [(ifleq0 test then else) (ifleq0 (subst what for test)
                                     (subst what for then)
                                     (subst what for else))]
    [(id sym) (cond
                [(symbol=? sym for) what]
                [else in])]
    [(app func arg) (app func (subst what for arg))]))


; returns the function with the name that matches the given symbol
(: get-fundef (-> Symbol (Listof fundef) fundef))
(define (get-fundef sym funs)
  (cond
    [(empty? funs) (error "invalid format DXUQ")]
    [(equal? sym (fundef-name (first funs))) (first funs)]
    [else (get-fundef sym (rest funs))]))


; predicate that checks if given value is a main function
(define (is_main? [fn : Any]) : Boolean
  (match fn
    [(fundef 'main 'init body) #t]
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
(check-equal? (interp (num 1) (parse-prog '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (binop '* (num 2) (num 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 6)
(check-equal? (interp (ifleq0 (num 0) (num 1) (num 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (ifleq0 (num 1) (num 1) (num 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 2)
(check-equal? (interp (app 'f (num 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 17)
(check-equal? (interp (app 'f (binop '+ (num 1) (num 1))) (parse-prog  '{{fundef {f x} {/ x 1}}})) 2)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (id 'a) (parse-prog '{{fundef {f x} {+ x 14}}}))))


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
(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (num 1) (num 2)))
(check-equal? (parse '(- 1 2)) (binop '- (num 1) (num 2)))
(check-equal? (parse '(+ (+ 1 2) 3)) (binop '+ (binop '+ (num 1) (num 2)) (num 3)))
(check-equal? (parse '(* (* 1 2) 3)) (binop '* (binop '* (num 1) (num 2)) (num 3)))
(check-equal? (parse '(ifleq0 1 2 3)) (ifleq0 (num 1) (num 2) (num 3)))
(check-equal? (parse '(ifleq0 (+ 1 2) 2 3)) (ifleq0 (binop '+ (num 1) (num 2)) (num 2) (num 3)))
(check-equal? (parse 'x) (id 'x))
(check-equal? (parse '(addone 1)) (app 'addone (num 1)))
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
(check-equal? (parse-fundef '{fundef {addone x} {+ x 1}}) (fundef 'addone 'x (binop '+ (id 'x) (num 1))))
(check-equal? (parse-fundef '{fundef {main init} {f 2}}) (fundef 'main 'init (app 'f (num 2))))
(check-equal? (parse-fundef '{fundef {mult x} {* x 2}}) (fundef 'mult 'x (binop '* (id 'x) (num 2))))
(check-equal? (parse-fundef '{fundef {main init} {ifleq0 0 1 2}})
              (fundef 'main 'init (ifleq0 (num 0) (num 1) (num 2))))
(check-equal? (parse-fundef '{fundef {sub z} {- z 1}}) (fundef 'sub 'z (binop '- (id 'z) (num 1))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-fundef '(1 x (+ x 1)))))


;;parse-prog test cases
(check-equal? (parse-prog '((fundef (f x) (+ x 14))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3)) (fundef (f3 y) (/ y 1))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))
                    (fundef 'f3 'y (binop '/ (id 'y) (num 1)))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog 'a)))


;;interp-binop test cases
(check-equal? (interp-binop '- (num 1) (num 0) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 1)
(check-equal? (interp-binop '/ (num 0) (num 1) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 0)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp-binop '^ (num 1) (num 2) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14)))))))


;;get-fundef test cases
(check-equal? (get-fundef 'f2 (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f2 'z (binop '* (num 2) (num 3))))
(check-equal? (get-fundef 'f (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f 'x (binop '+ (id 'x) (num 14))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (get-fundef 'x '())))


;;subst test cases
(check-equal? (subst (num 5) 'f (binop '+ (id 'f) (num 5))) (binop '+ (num 5) (num 5)))
(check-equal? (subst (num 2) 'f (num 5)) (num 5))
(check-equal? (subst (num 0) 'x (ifleq0 (id 'x) (num 1) (num 2))) (ifleq0 (num 0) (num 1) (num 2)))
(check-equal? (subst (id 'z) 'x (id 'x)) (id 'z))
(check-equal? (subst (id 'z) 'x (id 'f)) (id 'f))
(check-equal? (subst (num 3) 'x (app 'f (id 'x))) (app 'f (num 3)))


;;is_main? test cases
(check-equal? (is_main? (fundef 'main 'init (binop '+ (num 1) (num 2)))) true)
(check-equal? (is_main? (fundef 'f 'x (binop '+ (id 'x) (num 1)))) false)


;;not_main? test cases
(check-equal? (not_main? (fundef 'main 'init (binop '+ (num 1) (num 2)))) false)
(check-equal? (not_main? (fundef 'f 'x (binop '+ (id 'x) (num 1)))) true)















