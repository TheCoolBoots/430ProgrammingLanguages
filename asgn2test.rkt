#lang typed/racket

(require typed/rackunit)

;;create DXUQ2 language
(define-type DXUQ2 (U num binop ifleq0 id app))
(struct num ([n : Real]) #:transparent)
(struct binop ([op : Symbol] [l : DXUQ2] [r : DXUQ2]) #:transparent)
(struct ifleq0 ([test : DXUQ2] [then : DXUQ2] [else : DXUQ2]) #:transparent)
(struct fundef ([name : Symbol] [arg : Symbol] [body : DXUQ2]) #:transparent)
(struct id ([s : Symbol]) #:transparent)
(struct app ([func : Symbol] [arg : DXUQ2]) #:transparent)


;;takes in s-expr and parses to create DXUQ2
(define (parse [s : Sexp]) : DXUQ2
  (match s
    [(? real? r) (num s)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '/ l r) (binop '/ (parse l) (parse r))]
    [(list 'ifleq0 test then else) (ifleq0 (parse test) (parse then) (parse else))]
    [(? symbol? sym) (match sym
                       ['+  (error "invalid format DXUQ")]
                       ['-  (error "invalid format DXUQ")]
                       ['*  (error "invalid format DXUQ")]
                       ['/  (error "invalid format DXUQ")]
                       ['undef  (error "invalid format DXUQ")]
                       ['ifleq0  (error "invalid format DXUQ")]
                       ['fundef (error "invalid format DXUQ")]
                       [other (id sym)])]
    [(list (? symbol? sym) arg) (app sym (parse arg))]
    [other (error "invalid format DXUQ")]))


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

;;takes in s-expr and parses to make function
(define (parse-fundef [s : Sexp]) : fundef
  (match s
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body) (fundef name arg (parse body))]
    ;[(list 'fundef (list (? symbol? name) (? symbol? arg)) (? list? l)) (fundef name arg (parse l))] 
    [other (error "invalid format DXUQ")]))


(check-equal? (parse-fundef '{fundef {addone x} {+ x 1}}) (fundef 'addone 'x (binop '+ (id 'x) (num 1))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-fundef '(1 x (+ x 1)))))



;;takes in s-expr and parses to make list of functions
(define (parse-prog [s : Sexp]) : (Listof fundef)
  (match s
    [(list) (error "invalid format DXUQ")]
    [(list a) (cons (parse-fundef a) '())]
    [(list a b) (cons (parse-fundef a) (cons (parse-fundef b) '()))]
    [(? list? l) (cons (parse-fundef (first l)) (parse-prog (rest l)))]
    [other (error "invalid format DXUQ")]))

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



;;determine what bin op to perform given symbol and two DXUQ2
(define (getBinOp [op : Symbol] [l : DXUQ2] [r : DXUQ2] [funcs : (Listof fundef)]) : Real
  (match op
    ['+ (+ (interpBinOps l funcs) (interpBinOps r funcs))]
    ['* (* (interpBinOps l funcs) (interpBinOps r funcs))]
    ['- (- (interpBinOps l funcs) (interpBinOps r funcs))]
    ['/ (/ (interpBinOps l funcs) (interpBinOps r funcs))]
    [other (error "invalid format DXUQ")]))



(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (getBinOp '^ (num 1) (num 2) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14)))))))

;;interpret DXUQ2 and outputs a real
(define (interpBinOps [dx : DXUQ2] [funcs : (Listof fundef)]) : Real
  (match dx
    [(num n) n]
    [(app func arg) (define fd (get-fundef func funcs))
                    (interpBinOps (subst arg (fundef-arg fd) (fundef-body fd)) funcs)]
    [(binop op l r) (getBinOp op l r funcs)]
    [(ifleq0 test then else) (if (< (interpBinOps test funcs) 1) (interpBinOps then funcs) (interpBinOps else funcs))]))



(check-equal? (interpBinOps (num 5) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 5)
(check-equal? (interpBinOps (binop '+ (num 2) (num 3)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 5)
(check-equal? (interpBinOps (binop '* (num 5) (num 10)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 50)
(check-equal? (interpBinOps (binop '- (num 23) (num 11)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 12)
(check-equal? (interpBinOps (binop '/ (num 10) (num 5)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 2)
(check-equal? (interpBinOps (ifleq0 (num 1) (num 2) (num 3)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 3)
(check-equal? (interpBinOps (ifleq0 (num -1) (num 2) (num 3)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 2)



(check-equal? (getBinOp '- (num 1) (num 0) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 1)
(check-equal? (getBinOp '/ (num 0) (num 1) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 0)


;;get the function definition that matches the symbol
(define (get-fundef [sym : Symbol] [funs : (Listof fundef)]) : fundef
  (cond
    [(empty? funs) (error "invalid format DXUQ")]
    [(cons? funs) (cond
                    [(equal? sym (fundef-name (first funs))) (first funs)]
                    [else (get-fundef sym (rest funs))])]))

(check-equal? (get-fundef 'f2 (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f2 'z (binop '* (num 2) (num 3))))
(check-equal? (get-fundef 'f (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f 'x (binop '+ (id 'x) (num 14))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (get-fundef 'x '())))


;;substitute the value into the functions
(define (subst [what : DXUQ2] [for : Symbol] [in : DXUQ2]) : DXUQ2
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
    
(check-equal? (subst (num 5) 'f (binop '+ (id 'f) (num 5))) (binop '+ (num 5) (num 5)))
(check-equal? (subst (num 2) 'f (num 5)) (num 5))
(check-equal? (subst (num 0) 'x (ifleq0 (id 'x) (num 1) (num 2))) (ifleq0 (num 0) (num 1) (num 2)))
(check-equal? (subst (id 'z) 'x (id 'x)) (id 'z))
(check-equal? (subst (id 'z) 'x (id 'f)) (id 'f))
(check-equal? (subst (num 3) 'x (app 'f (id 'x))) (app 'f (num 3)))


(define (is_main? [fn : Any]) : Boolean
  (match fn
    [(fundef 'main 'init body) #t]
    [other #f]))

(check-equal? (is_main? (fundef 'main 'init (binop '+ (num 1) (num 2)))) true)
(check-equal? (is_main? (fundef 'f 'x (binop '+ (id 'x) (num 1)))) false)


(define (not_main? [fn : Any]) : Boolean
  (match fn
    [(fundef 'main 'init body) #f]
    [other #t]))

(check-equal? (not_main? (fundef 'main 'init (binop '+ (num 1) (num 2)))) false)
(check-equal? (not_main? (fundef 'f 'x (binop '+ (id 'x) (num 1)))) true)


(define (interp-fns [funs : (Listof fundef)]) : Real
   (define main (filter is_main? funs))
   (define notMain(filter not_main? funs))
  (if (= (length main) 0) (error "invalid format DXUQ")
      (interp (subst (num 0) 'init (fundef-body (first main))) notMain)))
  ;(interp2 (subst (num 0) 'init (fundef-body (first main))) notMain))


   
(define (interp [exp : DXUQ2] [funs : (Listof fundef)]) : Real
  (match exp
    [(num n) n]
    [(binop op l r) (getBinOp op l r funs)]
    [(ifleq0 test then else) (if (< (interp test funs) 1) (interp then funs) (interp else funs))]
    [(app func arg) (define fd (get-fundef func funs)) (interp (subst arg
                                                                       (fundef-arg fd)
                                                                       (fundef-body fd))
                                                                funs)]
    [(id sym) (error "invalid format DXUQ")]))

(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {f 2}}})) 16)
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {+ 1 2}}})) 3)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}}))))

(check-equal? (interp (num 1) (parse-prog '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (binop '* (num 2) (num 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 6)
(check-equal? (interp (ifleq0 (num 0) (num 1) (num 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (ifleq0 (num 1) (num 1) (num 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 2)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (id 'a) (parse-prog '{{fundef {f x} {+ x 14}}}))))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{{fundef {f x} {+ x 14}}
                     {fundef {main init} {f 2}}}) 16)
    
;(define (top-interp [s : Sexp])
;  (interp (parse s)))

(check-equal? (interpBinOps (app 'f (num 2)) (list
 (fundef 'f 'x (binop '+ (id 'x) (num 14))))) 16)


