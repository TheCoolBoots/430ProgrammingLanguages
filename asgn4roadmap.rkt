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
(struct primV ([op : Symbol]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : DXUQ4] [env : Env]) #:transparent)


(struct fundef ([name : Symbol] [args : (Listof Symbol)] [body : DXUQ4]) #:transparent)
(struct Bind ([name : Symbol] [val : Value]) #:transparent)
(define-type Env (Listof Bind))
(define mt-env '())
;(define extend-env cons)


(define top-env (list (Bind 'true (boolV true)) (Bind 'false (boolV false))
                      (Bind '+ (primV '+)) (Bind '- (primV '-)) (Bind '* (primV '*))
                      (Bind '/ (primV '/)) (Bind '<= (primV '<=)) (Bind 'equal? (primV 'equal?))
                      (Bind 'error (primV 'error))))

(define keywords (list 'let 'in 'if 'fn))
;(define ops (list (idC '+) (idC '-) (idC '*) (idC '/) (idC '<=) (idC 'equal?)
 ;                 '+ '- '* '/ '<= 'equal?))

; parses a list of function definitions and interprets main
; returns a real number
;(: top-interp (Sexp -> Real))
;(define (top-interp fun-sexps)
;  (interp-fns (parse-prog fun-sexps) top-env))

(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))



; interprets a DXUQ2 expression and returns a real
(: interp (-> DXUQ4 Env Value))
(define (interp exp env)
  (match exp
    [(numC n) (numV n)]
    [(stringC s) (strV s)]
    [(lamC arg body) (closV arg body env)]
    [(ifC test then else) (define b (interp test env))
                          (match b
                            [(boolV bool) (cond
                                            [(equal? b (boolV #t)) (interp then env)]
                                            [else (interp else env)])]
                            [other (error "invalid format DXUQ")])]          
    [(appC fun args) (define interpretedBody (interp fun env))
     (match interpretedBody
       [(closV arg body old-env)
        (define interpretedParams (map (lambda ([param : DXUQ4]) (interp param old-env)) args))
        (define new-env (extend-env2 old-env arg interpretedParams ))
        (interp body new-env)]
       [(primV symbol) (interp-primV symbol (map (lambda ([param : DXUQ4]) (interp param env)) args))]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(idC sym) 
                 (lookup sym env)]))
   
  
    


; make a binding for each symbol in appC-lamC-ids with each of the arguments in appC-args
; add the list of bindings to the env
; return a cloV with appC target, args, and new env
;(: gen-cloV (-> (Listof Symbol) (Listof DXUQ4) Env closV))
;(define (gen-cloV app body env)
;  (define new-env (extend-env app body env))
;  (closV app (first body) new-env))


; helper function for gen-cloV; returns extended environment
;(: extend-env (-> (Listof Symbol) (Listof DXUQ4) Env Env))
;(define (extend-env symbols args env)
;  (cond
;    [(xor (empty? symbols) (empty? args)) (error "Different numbers of symbols and args DXUQ")]
;    [(empty? symbols) env]
;    [else (cons (Bind (first symbols) (interp (first args) env)) (extend-env (rest symbols) (rest args) env))]))




(define (extend-env2 [env : Env] [args : (Listof Symbol)] [vals : (Listof Value)]) : Env
  (cond
    [(xor (empty? args) (empty? vals)) (error "invalid format DXUQ")]
    [(empty? args) env]
    [else (cons (Bind (first args) (first vals)) (extend-env2 env (rest args) (rest vals)))]))

(check-equal? (extend-env2 '() (list 'a 'b) (list (numV 1) (numV 2)))
              (list (Bind 'a (numV 1)) (Bind 'b (numV 2))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (extend-env2 '() '() (list (numV 1)))))


;;helper for appC interp-args
(define (interp-args [args : (Listof DXUQ4)] [env : Env]) : (Listof Value)
  (cond
    [(empty? args) '()]
    [else (cons (interp (first args) env) (interp-args (rest args) env))]))

(check-equal? (interp-args (list (numC 1) (stringC "hello")) '())
              (list (numV 1) (strV "hello")))
 


; interprets a primV into a value
(: interp-primV (-> Symbol (Listof Value) Value))
(define (interp-primV primSymbol params)
  (match primSymbol
    ['+ (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for + DXUQ")]
          [else (interp-add params)])]
    ['- (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for - DXUQ")]
          [else (interp-sub params)])]
    ['* (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for * DXUQ")]
          [else (interp-mult params)])]
    ['/ (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for / DXUQ")]
          [else (interp-div params)])]
    ['<= (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for <= DXUQ")]
          [else (interp-leq params)])]
    ['equal? (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for equal? DXUQ")]
          [else (cond
                  [(or (primV? (first params)) (primV? (second params))) (boolV false)]
                  [(or (closV? (first params)) (closV? (second params))) (boolV false)]
                  [else (boolV (equal? (first params) (second params)))])])]
    ['error (cond
          [(not (equal? (length params) 1)) (error "Invalid number of arguments for equal? DXUQ")]
          [else (error (string-append "User Error DXUQ: " (serialize (first params))))])]))

(check-equal? (interp-primV 'equal? (list (primV '+) (primV '+))) (boolV false))


; interprets addition primitive
(: interp-add (-> (Listof Value) numV))
(define (interp-add args)
  (cond
    [(empty? args) (numV 0)]
    [else (match (first args)
            [(numV n) (numV (+ n (numV-n (interp-add (rest args)))) )]
            [other (error "Invalid operands for DXUQ +")] )] ))


; interprets subtraction primitive
(: interp-sub (-> (Listof Value) numV))
(define (interp-sub args)
  (cond
    [(andmap numV? args)
     (define newArgs
       (cons (first args) (map (lambda ([arg : numV]) (numV (* -1 (numV-n arg)))) (rest args)) ) )
     (interp-add newArgs)]
    [else (error "Invalid operands for DXUQ -")]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) numV))
(define (interp-mult args)
  (cond
    [(empty? args) (numV 1)]
    [else (match (first args)
            [(numV n) (numV (* n (numV-n (interp-mult (rest args)))) )]
            [other (error "Invalid operands for DXUQ *")] )] ))


; interprets division primitive
(: interp-div (-> (Listof Value) numV))
(define (interp-div args)
  (cond
    [(and (andmap numV? args) (andmap (lambda ([n : numV]) (not (eq? (numV-n n) 0))) (rest args)))
     (define newArgs (cons (first args) (map (lambda ([arg : numV]) (numV (/ 1 (numV-n arg)))) (rest args)) ) )
     (interp-mult newArgs)]
    [else (error "Invalid operands for DXUQ /")]))


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) boolV))
(define (interp-leq args)
  (cond
    [(and (numV? (first args)) (numV? (second args)))
     (boolV (<= (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV)))) ]
    [else (error "Invalid operands for DXUQ <=")]))


; interprets a DXUQ if statement and returns a Value
;(: interp-cond (-> DXUQ4 DXUQ4 DXUQ4 Env Value))
;(define (interp-cond if then else env)
;  (match (interp if env)
;    [(boolV val) (cond
;                   [val (interp then env)]
;                   [else (interp else env)])]
;    [other (error "Invalid operands for DXUQ if")]))

;(check-equal? interp



;;take a symbol and environment, return value
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error "Name not found DXUQ")]
    [else (cond
            [(equal? for (Bind-name (first env))) (Bind-val (first env))]
            [else (lookup for (rest env))])]))

(check-equal? (lookup 'x (list (Bind 'a (numV 1)) (Bind 'x (numV 2)))) (numV 2))
(check-exn (regexp (regexp-quote "Name not found DXUQ"))
           (lambda () (lookup 'a (list))))


;;takes a value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (~v n)]
    [(strV s) (~v s)]
    [(primV op) "#<primop>"]
    [(closV arg body env) "#<procedure>"]
    [(boolV val) (cond
                   [val "true"]
                   [else "false"])]))
(check-equal? (serialize (numV 1)) "1")
(check-equal? (serialize (strV "hello")) "\"hello\"")
(check-equal? (serialize (boolV true)) "true")
(check-equal? (serialize (boolV false)) "false")
(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (closV '(a) (appC (numC 0) (list (numC 1))) '())) "#<procedure>")


;;takes an s-expr, returns a DXUQ4
(define (parse [s : Sexp]) : DXUQ4
  (match s
    [(? real? r) (numC r)]
    
    [(? symbol? sym) (cond
                       [(DXUQ4-keyword? sym keywords) (error "invalid format DXUQ")]
                       [else (idC sym)])]
    [(? string? str) (stringC str)]
    ;[(list 'let (list (? symbol? s) '= expr) ... 'in expr2) (numC 0)]
    [(list 'let mappings ... 'in body) (parse-let mappings body)]
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))]
    [(list 'fn (list (? symbol? syms) ...) body) (cond
                                                   [(equal? (check-duplicates syms) #f)
                                                    (lamC (cast syms (Listof Symbol)) (parse body))]
                                                   [else (error "invalid format DXUQ")])]
    
    [(list expr expr2 ...) (cond
                       [(empty? expr) (error "invalid format DXUQ")]
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


; desugars a let statement into a function application (appC)
(: parse-let (-> (Listof Any) Sexp DXUQ4))
(define (parse-let mappings body)
  (define ids (map (lambda (mapping) (match mapping
                                       [(list (? symbol? s) '= expr) s]
                                       [other (error "Invalid formatting for let statement DXUQ")])) mappings))
  (define args (map (lambda (mapping) (match mapping
                                        [(list (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ids body) args) Sexp)))




(check-equal? (parse 1) (numC 1))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse "hello") (stringC "hello"))
(check-equal? (parse '(fn (a) 1)) (lamC '(a) (numC 1)))
(check-equal? (parse '(let (z = (+ 9 14)) (y = 98) in (+ z y)))
              (appC
 (lamC '(z y) (appC (idC '+) (list (idC 'z) (idC 'y))))
 (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse 'if)))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(()))))
(check-exn (regexp (regexp-quote "Invalid formatting for let statement DXUQ"))
           (lambda () (parse-let '((a != 1)) '(+ a 1))))


(check-equal? (interp (numC 1) '()) (numV 1))
(check-equal? (interp (parse '(fn (a) (+ 1 a))) '())
              (closV '(a) (appC (idC '+) (list (numC 1) (idC 'a))) '()))
(check-equal? (interp (parse '(+ 1 2)) top-env) (numV 3))
(check-equal? (interp (parse '(* 1 2)) top-env) (numV 2))
(check-equal? (interp (parse '(- 2 1)) top-env) (numV 1))
(check-equal? (interp (parse '(/ 2 1)) top-env) (numV 2))
(check-equal? (interp (parse '(<= 1 2)) top-env) (boolV #t))
(check-equal? (interp (parse '(equal? "a" "a")) top-env) (boolV #t))
(check-equal? (interp (parse '((((fn (a) (fn (b) (fn (c) (a b c)))) +) 1) 2)) top-env)
              (numV 3))
(check-equal? (interp (parse '(if ((fn (a) (<= a 1)) 0) 1 2)) top-env) (numV 1))
(check-equal? (interp (parse '(if ((fn (a) (<= a 1)) 2) 1 2)) top-env) (numV 2))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (parse '(if ((fn (a) (+ a 1)) 1) 1 2)) top-env)))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ <="))
           (lambda () (interp-leq (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ +"))
           (lambda () (interp-add (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ -"))
           (lambda () (interp-sub (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ *"))
           (lambda () (interp-mult (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ /"))
           (lambda () (interp-div (list (strV "a") (strV "b")))))
;(check-exn (regexp (regexp-quote "Invalid operands for DXUQ if"))
;           (lambda () (interp-cond (numC 1) (numC 1) (numC 1) '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(fn (a a) (+ a a)))))


(check-exn (regexp (regexp-quote "Invalid number of arguments for + DXUQ"))
           (lambda () {top-interp '{+ 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for - DXUQ"))
           (lambda () {top-interp '{- 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for * DXUQ"))
           (lambda () {top-interp '{* 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for / DXUQ"))
           (lambda () {top-interp '{/ 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for <= DXUQ"))
           (lambda () {top-interp '{<= 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for equal? DXUQ"))
           (lambda () {top-interp '{equal? 1}}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for equal? DXUQ"))
           (lambda () {top-interp '{error 1 1 2}}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '{+ let if}}))
(check-exn (regexp (regexp-quote "Applied arguments to non-function DXUQ"))
           (lambda () {top-interp '{5 4 3}}))
(check-exn (regexp (regexp-quote "User Error DXUQ: \"hi\""))
           (lambda () {top-interp '{error "hi"}}))



;(check-exn (regexp (regexp-quote "User Error DXUQ: \"help\""))
;           (lambda () (interp-primV 'error (list (stringC "help")))))

(check-equal? (top-interp '((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")
(check-equal? (top-interp '((((fn (a) (fn (b) (fn (c) (a b c)))) +) 1) 2)) "3")
(check-equal? (top-interp '(equal? (fn (a) (+ a 1)) (fn (a) (+ a 1)))) "false")

