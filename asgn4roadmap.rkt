#lang typed/racket

(require typed/rackunit)

;; Finished project, passes all test cases created by our group
;; as well as the handin server.

(define-type DXUQ4 (U numC stringC ifC idC fnC appC lamC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : DXUQ4] [then : DXUQ4] [else : DXUQ4])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct fnC ([ids : (Listof Symbol)] [body : DXUQ4]) #:transparent)
(struct appC ([fun : DXUQ4] [l : (Listof DXUQ4)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : DXUQ4]) #:transparent)


(define-type Value (U numV funV strV primV boolV closV))
(struct numV ([n : Real]) #:transparent)
(struct funV ([ids : (Listof Symbol)] [body : DXUQ4]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct primV ([op : Symbol] [body : (Listof DXUQ4)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : DXUQ4] [env : Env]) #:transparent)


(struct fundef ([name : Symbol] [args : (Listof Symbol)] [body : DXUQ4]) #:transparent)
(struct Bind ([name : Symbol] [val : Value]) #:transparent)
(define-type Env (Listof Bind))
(define mt-env '())
(define extend-env cons)

(define top-env (list (Bind 'true (boolV true)) (Bind 'false (boolV false))
                      (Bind '+ ))

(define keywords (list 'let 'in 'if 'fn))

; parses a list of function definitions and interprets main
; returns a real number
;(: top-interp (Sexp -> Real))
;(define (top-interp fun-sexps)
;  (interp-fns (parse-prog fun-sexps) top-env))

; interprets a DXUQ2 expression and returns a real
(: interp (-> DXUQ4 Env Value))
(define (interp exp env)
  (match exp
    [(numC n) (numV n)]
    [(lamC arg body) (closV arg body env)]
    [(appC fun args) (define funval (interp fun env)) (define argval (interp-args args env))
                     (define new-env (extend-env2 (closV-env (cast funval closV)) (closV-args (cast funval closV)) argval))
                     (interp (closV-body (cast funval closV)) new-env)]
    [(idC sym) (lookup sym env)]))

(define (extend-env2 [env : Env] [args : (Listof Symbol)] [vals : (Listof Value)]) : Env
  (cond
    [(xor (empty? args) (empty? vals)) (error "invalid format DXUQ")]
    [(empty? args) env]
    [else (cons (Bind (first args) (first vals)) (extend-env2 env (rest args) (rest vals)))]))

;;helper for appC interp-args
(define (interp-args [args : (Listof DXUQ4)] [env : Env]) : (Listof Value)
  (cond
    [(empty? args) '()]
    [else (cons (interp (first args) env) (interp-args (rest args) env))]))

;;take a symbol and environment, return value
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error "Name not found DXUQ")]
    [else (cond
            [(equal? for (Bind-name (first env))) (Bind-val (first env))]
            [else (lookup for (rest env))])]))


;;takes a value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (~v n)]
    [(strV s) (~v s)]
    [(primV op l) "#<primop>"]
    [(closV arg body env) "#<procedure>"]
    [(boolV val) (cond
                   [val "true"]
                   [else "false"])]))


;;takes an s-expr, returns a DXUQ4
(define (parse [s : Sexp]) : DXUQ4
  (match s
    [(? real? r) (numC r)]
    
    [(? symbol? sym) (cond
                       [(DXUQ4-keyword? sym keywords) (error "invalid format DXUQ4 sym")]
                       [else (idC sym)])]
    [(? string? str) (stringC str)]
    [(list 'let (list (? symbol? s) '= expr) ... 'in expr2) (numC 0)]
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))]
    [(list 'fn (list (? symbol? syms) ...) body) (lamC (cast syms (Listof Symbol)) (parse body))]
    
    [(list expr expr2 ...) (cond
                       [(empty? expr) (error "invalid format DXUQ4")]
                       [else (appC (parse expr) (map (lambda (a) (parse a)) expr2))])]
    [else (error "invalid format DXUQ")]))


; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(: DXUQ4-keyword? (-> Symbol (Listof Symbol) Boolean))
(define (DXUQ4-keyword? target keywords)
  (cond
    [(empty? keywords) #f]
    [(eq? (first keywords) target) #t]
    [else (DXUQ4-keyword? target (rest keywords))]))

