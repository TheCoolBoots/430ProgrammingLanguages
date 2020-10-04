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
    [(list (? symbol? sym) l r) (binop sym (parse l) (parse r))]
    [(list test then else) (ifleq0 (parse test) (parse then) (parse else))]
    [(? symbol? sym) (id sym)]
    [(list (? symbol? sym) arg) (app sym (parse arg))]
    [other (error "invalid format")]))


(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2)) (binop '+ (num 1) (num 2)))
(check-equal? (parse '(+ (+ 1 2) 3)) (binop '+ (binop '+ (num 1) (num 2)) (num 3)))
(check-equal? (parse '(* (* 1 2) 3)) (binop '* (binop '* (num 1) (num 2)) (num 3)))
(check-equal? (parse '(1 2 3)) (ifleq0 (num 1) (num 2) (num 3)))
(check-equal? (parse '((+ 1 2) 2 3)) (ifleq0 (binop '+ (num 1) (num 2)) (num 2) (num 3)))
(check-equal? (parse 'x) (id 'x))
(check-equal? (parse '(addone 1)) (app 'addone (num 1)))
(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (parse '(1 2 3 4 5))))

;;takes in s-expr and parses to make function
(define (parse-fundef [s : Sexp]) : fundef
  (match s
    [(list 'fundef (list (? symbol? name) (? symbol? arg)) body) (fundef name arg (parse body))]
    [other (error "invalid format")]))


(check-equal? (parse-fundef '{fundef {addone x} {+ x 1}}) (fundef 'addone 'x (binop '+ (id 'x) (num 1))))
(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (parse-fundef '(1 x (+ x 1)))))



;;takes in s-expr and parses to make list of functions
(define (parse-prog [s : Sexp]) : (Listof fundef)
  (match s
    [(list) (error "invalid format")]
    [(list a) (cons (parse-fundef a) '())]
    [(list a b) (cons (parse-fundef a) (cons (parse-fundef b) '()))]
    [(? list? l) (cons (parse-fundef (first l)) (parse-prog (rest l)))]
    [other (error "invalid format")]))

(check-equal? (parse-prog '((fundef (f x) (+ x 14))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3)) (fundef (f3 y) (/ y 1))))
              (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))
                    (fundef 'f3 'y (binop '/ (id 'y) (num 1)))))
(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (parse-prog '())))
(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (parse-prog 'a)))



;;determine what bin op to perform given symbol and two DXUQ2
(define (getBinOp [op : Symbol] [l : DXUQ2] [r : DXUQ2]) : Real
  (match op
    ['+ (+ (interp l) (interp r))]
    ['* (* (interp l) (interp r))]
    ['- (- (interp l) (interp r))]
    ['/ (/ (interp l) (interp r))]
    [other (error "invalid format")]))

(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (getBinOp '^ (num 1) (num 2))))

;;interpret DXUQ2 and outputs a real
;(define (interp [dx : DXUQ2]) : Real
;  (match dx
;    [(num n) n]
;    [(binop op l r) (getBinOp op l r)]
;    [(ifleq0 test then else) (if (< (interp test) 1) (interp then) (interp else))]))

(check-equal? (interp (num 5)) 5)
(check-equal? (interp (binop '+ (num 2) (num 3))) 5)
(check-equal? (interp (binop '* (num 5) (num 10))) 50)
(check-equal? (interp (binop '- (num 23) (num 11))) 12)
(check-equal? (interp (binop '/ (num 10) (num 5))) 2)
(check-equal? (interp (ifleq0 (num 1) (num 2) (num 3))) 3)
(check-equal? (interp (ifleq0 (num -1) (num 2) (num 3))) 2)

(define (get-fundef [sym : Symbol] [funs : (Listof fundef)]) : fundef
  (cond
    [(empty? funs) (error "invalid format")]
    [(cons? funs) (cond
                    [(equal? sym (fundef-name (first funs))) (first funs)]
                    [else (get-fundef sym (rest funs))])]))

(check-equal? (get-fundef 'f2 (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f2 'z (binop '* (num 2) (num 3))))
(check-equal? (get-fundef 'f (list (fundef 'f 'x (binop '+ (id 'x) (num 14)))
                    (fundef 'f2 'z (binop '* (num 2) (num 3)))))
              (fundef 'f 'x (binop '+ (id 'x) (num 14))))
(check-exn (regexp (regexp-quote "invalid format"))
           (lambda () (get-fundef 'x '())))



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



(define (interp-fns [funs : (Listof fundef)]) : Real
  
    

    

   
(define (interp2 [exp : DXUQ2] [funs : (Listof fundef)]) : Real
  (match exp
    [(num n) n]
    [(binop op l r) (getBinOp op l r)]
    [(ifleq0 test then else) (if (< (interp2 test funs) 1) (interp2 then funs) (interp2 else funs))]
    [(app func arg) (define fd (get-fundef func funs)) (interp2 (subst arg
                                                                       (fundef-arg fd)
                                                                       (fundef-body fd))
                                                                funs)]
    [(id sym) (error "invalid format")]))

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

    
;(define (top-interp [s : Sexp])
;  (interp (parse s)))


