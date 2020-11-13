#lang typed/racket

(require typed/rackunit)



(define-type ExprC (U numC stringC ifC idC fnC appC lamC assignC recC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct fnC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [l : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC] [t : (Listof Type)]) #:transparent)
(struct assignC ([id : Symbol] [body : ExprC]) #:transparent)
(struct recC ([name : Symbol] [args : (Listof Symbol)] [argT : (Listof Type)]
                              [ret : Type] [body : ExprC] [use : ExprC]) #:transparent)

(define-type Value (U numV funV strV primV boolV closV arrayV nullV))
(struct numV ([n : Real]) #:transparent)
(struct funV ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct arrayV ([loc : Integer] [length : Integer]) #:transparent)
(struct nullV () #:transparent)

(define-type Type (U numT boolT strT funT))
(struct numT () #:transparent)
(struct boolT () #:transparent)
(struct strT() #:transparent)
(struct funT([t : (Listof Type)] [ret : Type]) #:transparent)


(struct fundef ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)
(define-type Env (Mutable-HashTable Symbol (Boxof Value)))


;(define-type Store (Mutable-HashTable Integer Value))
(define keywords (list 'if ': '= 'let 'in 'rec 'fn '->))


;;takes sexp and returns a string
(define (top-interp [s : Sexp]) : String
  (define expr (parse s))
  (type-check expr base-tenv)
  (define top-env (hash-copy reset-env))
  (serialize (interp expr top-env)))

    

; interprets a DXUQ2 expression and returns a real
(: interp (-> ExprC Env Value))
(define (interp exp env)
  (match exp
    [(numC n) (numV n)]
    [(stringC s) (strV s)]
    [(lamC arg body types) (closV arg body env)]
    [(ifC test then else) (define b (interp test env))
                          (cond
                              [(equal? b (boolV #t)) (interp then env)]
                              [else (interp else env)])]         
    [(appC fun args) (define interpretedBody (interp fun env))
     (match interpretedBody
       [(closV ids clo-body clo-env)
        (define interpretedParams (map (lambda ([param : ExprC]) (interp param env)) args))
        (define new-env (extend-env clo-env ids interpretedParams))
        (interp clo-body new-env)]
       [(primV symbol) (symbol (map (lambda ([param : ExprC]) (interp param env)) args))]
       )]
    [(idC sym) 
                 (lookup sym env)]))

;;extend env
(define (extend-env [env : Env] [args : (Listof Symbol)] [vals : (Listof Value)]): Env
  (cond
    [(xor (empty? args) (empty? vals)) (error "Different numbers of ids and args DXUQ")]
    [(empty? args) env]
    [else (hash-set! env (first args) (box (first vals))) (extend-env env (rest args) (rest vals))]))




;;take a symbol and environment, return value
(define (lookup [for : Symbol] [env : Env]) : Value
  (define t (hash-keys env))
  (cond
    [(equal? (member for t) #f) (error "symbol not found in env DXUQ")]
    [else (unbox (hash-ref env for))]))



; interprets addition primitive
(: interp-add (-> (Listof Value) numV))
(define (interp-add args)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for + DXUQ")]
    [else (numV (+ (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV))))]))



; interprets subtraction primitive
(: interp-sub (-> (Listof Value) numV))
(define (interp-sub args)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for - DXUQ")]
      [else (numV (- (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV))))]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) numV))
(define (interp-mult args)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for * DXUQ")]
      [else (numV (* (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV))))]))



; interprets division primitive
(: interp-div (-> (Listof Value) Value))
(define (interp-div args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for / DXUQ")]
      [else (cond
              [(equal? (numV-n (cast (second args) numV)) 0) (error "divide by 0 DXUQ")]
              [else (numV (/ (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV))))])]))





; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) Value))
(define (interp-leq args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for <= DXUQ")]
      [else (boolV (<= (numV-n (cast (first args) numV)) (numV-n (cast (second args) numV))))]))

       


(define (interp-neq [vals : (Listof Value)]) : Value
  (cond
    [(not (equal? (length vals) 2)) (error "invalid number of args DXUQ")]
    [else (boolV (equal? (first vals) (second vals)))]))




(define (interp-seq [vals : (Listof Value)]) : Value
  (cond
    [(not (equal? (length vals) 2)) (error "invalid num args DXUQ")]
    [else (boolV (equal? (first vals) (second vals)))]))

(: interp-substring (-> (Listof Value) Value))
(define (interp-substring args)
  (match args
    [(list (strV str) (numV (? natural? n1)) (numV (? natural? n2)))
     (cond
       [(and (< n1 n2) (>= n1 0) (<= n2 (string-length str))) (strV (substring str n1 n2))]
       [else (error "Invalid substring operation DXUQ")])]
    [other (error "Invalid substring operation DXUQ")]))


;;takes an s-expr, returns a ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) (numC r)]
    
    [(? symbol? sym) (cond
                       [(ExprC-keyword? sym keywords) (error "invalid format DXUQ word")]
                       [else (idC sym)])]
    [(? string? str) (stringC str)]
    [(list 'let (list ty sym '= sym2) ... 'in body) (parse-let2 ty sym sym2 body)]
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))]
    [(list 'fn (list (list ty (? symbol? syms2)) ...) body) (cond
                                                   [(equal? (check-duplicates syms2) #f)
                                                    (lamC (cast syms2 (Listof Symbol))
                                                          (parse body)
                                                          (map (lambda (x) (parse-type x))
                                                               (cast ty (Listof Sexp))))]
                                                   [else (error "invalid format DXUQ")])]
    [(list 'rec (list (list (? symbol? s) (list t (? symbol? s2)) ...) ': t2 expr) expr2)
     (recC  s (cast s2 (Listof Symbol))
            (map (lambda ([x : Sexp]) (parse-type x))
                 (cast t (Listof Sexp))) (parse-type t2) (parse expr) (parse expr2))]
    [(list expr expr2 ...) (cond
                       [(empty? expr) (error "invalid format DXUQ")]
                       [else (appC (parse expr) (map (lambda (a) (parse a)) expr2))])]
    
    [else (error "invalid format DXUQ test")]))


(define (parse-let2 [t : (Listof Any)] [l : (Listof Any)] [l2 : (Listof Any)] [body : Sexp]) : ExprC
  (define ids (cast l (Listof Symbol)))
  (parse (cast (cons (list 'fn (let-helper t ids) body) l2) Sexp)))


(define (let-helper [t : (Listof Any)] [var : (Listof Symbol)]) : Sexp
  (cond
    [(and (empty? t) (empty? var)) '()]
    [else (cast (cons (list (first t) (first var)) (let-helper (rest t) (rest var))) Sexp)]))
  
  
    


;;parse the types
(define (parse-type [s : Sexp]) : Type
  (match s
    ['num (numT)]
    ['bool (boolT)]
    ['str (strT)]
    [(list ty ... '-> ret) (funT (map (lambda (t) (parse-type t))
                                      (cast ty (Listof Sexp)))
                                 (parse-type ret))]
    [other (error "invalid types DXUQ")]))
    
(check-equal? (parse-type 'num) (numT))
(check-equal? (parse-type 'bool) (boolT))
(check-equal? (parse-type 'str) (strT))
(check-equal? (parse-type '(num num -> num)) (funT (list (numT) (numT)) (numT)))

; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(: ExprC-keyword? (-> Symbol (Listof Symbol) Boolean))
(define (ExprC-keyword? target keywords)
  (cond
    [(empty? keywords) #f]
    [(eq? (first keywords) target) #t]
    [else (ExprC-keyword? target (rest keywords))]))






;;type checker
(define (type-check [expr : ExprC] [te : TEnv]) : Type
  (match expr
    [(numC n) (numT)]
    [(stringC s) (strT)]
    [(idC s) (lookup-type s te)]
    [(lamC args body t) (define new-env (extend-type-env te args t)) (funT t (type-check body new-env))]
    [(recC name args argT ret body use) (define new-env (extend-type-env  te (list name) (list (funT argT ret))))
                                        (cond
                                          [(not (equal? ret (type-check body (extend-type-env new-env args argT))))
                                           (error "body return type not correct DXUQ")]
                                          [else (type-check use new-env)])]
    [(appC fun l) (define ft (type-check fun te))
                  (define la (map (lambda ([x : ExprC]) (type-check x te)) l))
                  (cond
                    [(not (funT? ft)) (error "not a function DXUQ")]
                    [(not (equal? (funT-t ft) la))
                     (error "app arg type mismatch DXUQ")]
                    [else (funT-ret ft)])]
    [(ifC test then else) (cond
                            [(equal? (type-check test te) (boolT))
                             (define a (type-check then te))
                             (define b (type-check else te))
                             (if (equal? a b) a (error "invalid format DXUQ"))]
                            [else (error "test does not return boolean DXUQ")])]))

;;lookup type in environment
(define (lookup-type [s : Symbol] [te : TEnv]) : Type
  (cond
    [(empty? te) (error "symbol not found in type-env DXUQ")]
    [else (cond
            [(equal? s (BindT-s (first te))) (BindT-t (first te))]
            [else (lookup-type s (rest te))])]))

;;extend type environment
(define (extend-type-env [te : TEnv] [syms : (Listof Symbol)] [t : (Listof Type)]) : TEnv
  (cond
    [(empty? syms) te]
    [else (cons (BindT (first syms) (first t)) (extend-type-env te (rest syms) (rest t)))]))

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


(struct BindT ([s : Symbol] [t : Type]) #:transparent)
(define-type TEnv (Listof BindT))
(define base-tenv (list (BindT 'num (numT))
                        (BindT 'bool (boolT))
                        (BindT 'str (strT))
                        (BindT '+ (funT (list (numT) (numT)) (numT)))
                        (BindT '* (funT (list (numT) (numT)) (numT)))
                        (BindT '/ (funT (list (numT) (numT)) (numT)))
                        (BindT '- (funT (list (numT) (numT)) (numT)))
                        (BindT '<= (funT (list (numT) (numT)) (boolT)))
                        (BindT 'num-eq? (funT (list (numT) (numT)) (boolT)))
                        (BindT 'str-eq? (funT (list (strT) (strT)) (boolT)))
                        (BindT 'substring (funT (list (strT) (numT) (numT)) (strT)))
                        (BindT 'true (boolT))
                        (BindT 'false (boolT))))
                        
(define top-env (ann (make-hash
                      (list (cons '+ ((inst box Value) (primV interp-add)))
                            (cons '- ((inst box Value) (primV interp-sub)))
                            (cons '/ ((inst box Value) (primV interp-div)))
                            (cons '* ((inst box Value) (primV interp-mult)))
                            (cons '<= ((inst box Value) (primV interp-leq)))
                            (cons 'num-eq? ((inst box Value) (primV interp-neq)))
                            (cons 'str-eq? ((inst box Value) (primV interp-seq)))
                            (cons 'substring ((inst box Value) (primV interp-substring)))
                            (cons 'true ((inst box Value) (boolV true)))
                            (cons 'false ((inst box Value) (boolV false)))
                            )) Env))

(define reset-env (ann (make-hash
                      (list (cons '+ ((inst box Value) (primV interp-add)))
                            (cons '- ((inst box Value) (primV interp-sub)))
                            (cons '/ ((inst box Value) (primV interp-div)))
                            (cons '* ((inst box Value) (primV interp-mult)))
                            (cons '<= ((inst box Value) (primV interp-leq)))
                            (cons 'num-eq? ((inst box Value) (primV interp-neq)))
                            (cons 'str-eq? ((inst box Value) (primV interp-seq)))
                            (cons 'substring ((inst box Value) (primV interp-substring)))
                            (cons 'true ((inst box Value) (boolV true)))
                            (cons 'false ((inst box Value) (boolV false)))
                            )) Env))


(check-exn (regexp (regexp-quote "Different numbers of ids and args DXUQ"))
           (lambda () (extend-env top-env '() (list (numV 1) (numV 2)))))
(check-exn (regexp (regexp-quote "symbol not found in env DXUQ"))
           (lambda () (lookup 'xyz top-env)))
(check-equal? (type-check (numC 5) base-tenv) (numT))
(check-equal? (type-check (stringC "hello") base-tenv) (strT))
(check-equal? (type-check (idC 'num) base-tenv) (numT))
(check-equal? (type-check (appC (idC '+) (list (numC 1) (numC 2))) base-tenv)
              (numT))
(check-equal? (type-check (parse '(if (<= 0 1) 1 2)) base-tenv) (numT))
(check-exn (regexp (regexp-quote "test does not return boolean DXUQ"))
           (lambda () (type-check (ifC (numC 1) (numC 2) (stringC "hello")) base-tenv)))
(check-exn (regexp (regexp-quote "app arg type mismatch DXUQ"))
           (lambda () (type-check (appC (idC '+) (list (numC 1) (stringC "a"))) base-tenv)))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (type-check (parse '(if (<= 1 2) 1 "hello")) base-tenv)))
(check-exn (regexp (regexp-quote "not a function DXUQ"))
           (lambda () (type-check (parse '(1 2 3)) base-tenv)))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda ()  (parse '(fn ([num x][num x]) (+ x x)))))
(check-exn (regexp (regexp-quote "invalid types DXUQ"))
           (lambda ()  (parse '(fn ((((num -> 14)
                                      (str -> num) -> (bool -> bool)) a)) 8))))

(check-equal? (lookup-type 'num base-tenv) (numT))
(check-equal? (lookup-type 'str base-tenv) (strT))
(check-exn (regexp (regexp-quote "symbol not found in type-env DXUQ"))
           (lambda () (lookup-type 'n base-tenv)))


(check-equal? (extend-type-env '() '(a b) (list (numT) (numT))) (list (BindT 'a (numT))
                                                                      (BindT 'b (numT))))

(check-equal? (type-check (parse '(fn () (+ 1 2))) base-tenv) (funT (list) (numT)))
(check-equal? (type-check (parse '(fn ([num x]) (+ x 1))) base-tenv) (funT (list (numT)) (numT)))


                          

(check-equal? (parse 1) (numC 1))
(check-equal? (parse 'a) (idC 'a))
(check-equal? (parse "hello") (stringC "hello"))
(check-equal? (parse '(fn ([num a]) 1)) (lamC '(a) (numC 1) (list (numT))))
(check-equal? (parse '(let (num z = (+ 9 14)) (num y = 98) in (+ z y)))
              (appC
 (lamC '(z y) (appC (idC '+) (list (idC 'z) (idC 'y))) (list (numT) (numT)))
 (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse 'if)))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(()))))
(check-exn (regexp (regexp-quote "Invalid number of arguments for + DXUQ"))
           (lambda () (interp-add (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "Invalid number of arguments for * DXUQ"))
           (lambda () (interp-mult (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "Invalid number of arguments for / DXUQ"))
           (lambda () (interp-div (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "divide by 0 DXUQ"))
           (lambda () (interp-div (list (numV 1) (numV 0)))))
(check-exn (regexp (regexp-quote "Invalid number of arguments for - DXUQ"))
           (lambda () (interp-sub (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "invalid number of args DXUQ"))
           (lambda () (interp-neq (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "Invalid number of arguments for <= DXUQ"))
           (lambda () (interp-leq (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "invalid num args DXUQ"))
           (lambda () (interp-seq (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () (interp-substring (list (numV 1) (numV 2) (numV 3)))))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () (interp-substring (list (strV "test") (numV 0) (numV 7)))))

(check-equal? (interp (numC 1) top-env) (numV 1))
(check-equal? (interp (parse '(fn ([num a]) (+ 1 a))) top-env)
              (closV '(a) (appC (idC '+) (list (numC 1) (idC 'a))) top-env))
(check-equal? (interp (parse '(+ 1 2)) top-env) (numV 3))
(check-equal? (interp (parse '(* 1 2)) top-env) (numV 2))
(check-equal? (interp (parse '(- 2 1)) top-env) (numV 1))
(check-equal? (interp (parse '(/ 2 1)) top-env) (numV 2))
(check-equal? (interp (parse '(<= 1 2)) top-env) (boolV #t))
;(check-equal? (interp (parse '(equal? "a" "a")) top-env) (boolV #t))
(check-equal? (interp (parse '((((fn ([num a]) (fn ([num b]) (fn ([num c]) (a b c)))) +) 1) 2)) top-env)
              (numV 3))
(check-equal? (interp (parse '(if ((fn ([num a]) (<= a 1)) 0) 1 2)) top-env) (numV 1))
(check-equal? (interp (parse '(if ((fn ([num a]) (<= a 1)) 2) 1 2)) top-env) (numV 2))

(check-equal? (top-interp '((fn ([num x] [num y]) (+ x y)) 1 2)) "3")
(check-equal? (top-interp '((fn ([num a] [num b]) (num-eq? a b)) 1 1)) "true")
(check-equal? (top-interp '((fn ([str a] [str b]) (str-eq? a b)) "a" "b")) "false")
(check-equal? (top-interp '((fn ([str a] [num b] [num c]) (substring a b c)) "hello" 0 3))
              "\"hel\"")



(check-equal? (type-check (parse '{rec {{square-helper [num n]} : num
      {if {<= n 0} 0 {+ n {square-helper {- n 2}}}}}
  {let {{num -> num} square =
        {fn {[num n]} {square-helper {- {* 2 n} 1}}}}
    in
    {square 13}}}) base-tenv) (numT))

(check-exn (regexp (regexp-quote "body return type not correct DXUQ"))
           (lambda () (type-check (parse '{rec {{square-helper [num n]} : num
      {if {<= n 0} "help" "test"}}
  {let {{num -> num} square =
        {fn {[num n]} {square-helper {- {* 2 n} 1}}}}
    in
    {square 13}}}) base-tenv)))

(check-equal? (serialize (numV 1)) "1")
(check-equal? (serialize (strV "hello")) "\"hello\"")
(check-equal? (serialize (boolV true)) "true")
(check-equal? (serialize (boolV false)) "false")
(check-equal? (serialize (primV interp-add)) "#<primop>")
(check-equal? (serialize (closV '(a) (appC (numC 0) (list (numC 1))) top-env)) "#<procedure>")