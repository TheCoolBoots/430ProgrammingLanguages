#lang typed/racket

(require typed/rackunit)

;;not passing while/in-order test case

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
(define keywords (list 'let 'in 'if 'fn ':=))


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
                          (match b
                            [(boolV bool) (cond
                                            [(equal? b (boolV #t)) (interp then env)]
                                            [else (interp else env)])]
                            [other (error "invalid format DXUQ")])]          
    [(appC fun args) (define interpretedBody (interp fun env))
     (match interpretedBody
       [(closV ids clo-body clo-env)
        (define interpretedParams (map (lambda ([param : ExprC]) (interp param env)) args))
        (define new-env (extend-env clo-env ids interpretedParams))
        (interp clo-body new-env)]
       [(primV symbol) (symbol (map (lambda ([param : ExprC]) (interp param env)) args))]
       [other (error "Applied arguments to non-function DXUQ")])]
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
    [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (numV (+ n m))]
                        [other (error "Invalid operands for + DXUQ")])]
            [other (error "Invalid operands for + DXUQ")])]))

; interprets subtraction primitive
(: interp-sub (-> (Listof Value) numV))
(define (interp-sub args)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for - DXUQ")]
      [else (match (first args)
              [(numV n) (match (second args)
                          [(numV m) (numV (- n m))]
                          [other (error "Invalid operands for - DXUQ")])]
              [other (error "Invalid operands for - DXUQ")])]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) numV))
(define (interp-mult args)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for * DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (numV (* n m))]
                        [other (error "Invalid operands for * DXUQ")])]
            [other (error "Invalid operands for * DXUQ")])]))



; interprets division primitive
(: interp-div (-> (Listof Value) Value))
(define (interp-div args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for / DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV (? natural? m)) #:when (> m 0) (numV (/ n m))]
                        [other (error "Invalid operands for / DXUQ")])]
            [other (error "Invalid operands for / DXUQ")])]))


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) Value))
(define (interp-leq args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for <= DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (boolV (<= n m))]
                        [other (error "Invalid operands for <= DXUQ")])]
            [other (error "Invalid operands for <= DXUQ")])]))

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
    [(list (? symbol? id) ':= body) (cond
                          [(ExprC-keyword? id keywords) (error "invalid format DXUQ")]
                          [else (assignC id (parse body))])]
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
            (map (lambda ([x : Sexp]) (parse-type x)) (cast t (Listof Sexp))) (parse-type t2) (parse expr) (parse expr2))]
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
                                 (parse-type ret))]))
    
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


; desugars a let statement into a function application (appC)
(: parse-let (-> (Listof Any) Sexp ExprC))
(define (parse-let mappings body)
  (define ids (map (lambda (mapping) (match mapping
                                       [(list (? symbol? s) '= expr) s]
                                       [other (error "Invalid formatting for let statement DXUQ")])) mappings))
  (define args (map (lambda (mapping) (match mapping
                                        [(list (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ids body) args) Sexp)))




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


(check-equal? (lookup-type 'num base-tenv) (numT))
(check-equal? (lookup-type 'str base-tenv) (strT))
(check-exn (regexp (regexp-quote "symbol not found in type-env DXUQ"))
           (lambda () (lookup-type 'n base-tenv)))


(check-equal? (extend-type-env '() '(a b) (list (numT) (numT))) (list (BindT 'a (numT))
                                                                      (BindT 'b (numT))))

(check-equal? (type-check (parse '(fn () (+ 1 2))) base-tenv) (funT (list) (numT)))
(check-equal? (type-check (parse '(fn ([num x]) (+ x 1))) base-tenv) (funT (list (numT)) (numT)))


                          
#|
;;test coverage
(check-equal? (allocate top-store '()) 17)
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

;;test cases interp
(check-equal? (interp (numC 1) '() top-store) (numV 1))
(check-equal? (interp (parse '(fn (a) (+ 1 a))) '() top-store)
              (closV '(a) (appC (idC '+) (list (numC 1) (idC 'a))) '()))
(check-equal? (interp (parse '(+ 1 2)) top-env top-store) (numV 3))
(check-equal? (interp (parse '(* 1 2)) top-env top-store) (numV 2))
(check-equal? (interp (parse '(- 2 1)) top-env top-store) (numV 1))
(check-equal? (interp (parse '(/ 2 1)) top-env top-store) (numV 2))
(check-equal? (interp (parse '(<= 1 2)) top-env top-store) (boolV #t))
(check-equal? (interp (parse '(equal? "a" "a")) top-env top-store) (boolV #t))
(check-equal? (interp (parse '((((fn (a) (fn (b) (fn (c) (a b c)))) +) 1) 2)) top-env top-store)
              (numV 3))
(check-equal? (interp (parse '(if ((fn (a) (<= a 1)) 0) 1 2)) top-env top-store) (numV 1))
(check-equal? (interp (parse '(if ((fn (a) (<= a 1)) 2) 1 2)) top-env top-store) (numV 2))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (parse '(if ((fn (a) (+ a 1)) 1) 1 2)) top-env top-store)))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () (interp-leq (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () (interp-add (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () (interp-sub (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () (interp-mult (list (strV "a") (strV "b")))))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () (interp-div (list (strV "a") (strV "b")))))
;(check-exn (regexp (regexp-quote "Invalid operands for DXUQ if"))
;           (lambda () (interp-cond (numC 1) (numC 1) (numC 1) '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(fn (a a) (+ a a)))))

;;test cases for binops
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
(check-exn (regexp (regexp-quote "Invalid number of arguments for error DXUQ"))
           (lambda () {top-interp '{error 1 1 2}}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '{+ let if}}))
(check-exn (regexp (regexp-quote "Applied arguments to non-function DXUQ"))
           (lambda () {top-interp '{5 4 3}}))
(check-exn (regexp (regexp-quote "User Error DXUQ: \"hi\""))
           (lambda () {top-interp '{error "hi"}}))



;(check-exn (regexp (regexp-quote "User Error DXUQ: \"help\""))
;           (lambda () (interp-primV 'error (list (stringC "help")))))

;;test cases for top-interp
(check-equal? (top-interp '((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")
(check-equal? (top-interp '((((fn (a) (fn (b) (fn (c) (a b c)))) +) 1) 2)) "3")
(check-equal? (top-interp '(equal? (fn (a) (+ a 1)) (fn (a) (+ a 1)))) "false")

(check-equal? (interp (appC (idC '+) (list (numC 1) (numC 2))) top-env top-store) (numV 3))
(check-exn (regexp (regexp-quote "Invalid number of arguments for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (numC 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (stringC "a") (numC 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (numC 1) (stringC "a"))) top-env top-store}))

; tests for -
(check-equal? (interp (appC (idC '-) (list (numC 1) (numC 2))) top-env top-store) (numV -1))
(check-exn (regexp (regexp-quote "Invalid number of arguments for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (numC 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (stringC "a") (numC 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (numC 1) (stringC "a"))) top-env top-store}))

; tests for *
(check-equal? (interp (appC (idC '*) (list (numC 1) (numC 2))) top-env top-store) (numV 2))
(check-exn (regexp (regexp-quote "Invalid number of arguments for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (numC 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (stringC "a") (numC 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (numC 1) (stringC "a"))) top-env top-store}))

; tests for /
(check-equal? (interp (appC (idC '/) (list (numC 2) (numC 2))) top-env top-store) (numV 1))
(check-exn (regexp (regexp-quote "Invalid number of arguments for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (numC 1))) top-env top-store}))
;(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
;           (lambda () {interp (appC (idC '/) (list (boolV #t) (numV 2))) top-env top-store}))
;(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
;           (lambda () {interp (appC (idC '/) (list (numV 1) (boolV #t))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (numC 1) (numC 0))) top-env top-store}))

; tests for <=
(check-equal? (interp (appC (idC '<=) (list (numC 1) (numC 2))) top-env top-store) (boolV #t))
(check-exn (regexp (regexp-quote "Invalid number of arguments for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (numC 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (stringC "a") (numC 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (numC 1) (stringC "a"))) top-env top-store}))

; tests for equal?
(check-equal? (interp (appC (idC 'equal?) (list (numC 1) (numC 2))) top-env top-store) (boolV #f))
(check-exn (regexp (regexp-quote "Invalid number of arguments for equal? DXUQ"))
           (lambda () {interp (appC (idC 'equal?) (list (numC 1))) top-env top-store}))


;; test cases for top-interp 
(check-equal? (top-interp '{if {<= 3 5} 3 5}) "3")
(check-equal? (top-interp '{if true 3 5}) "3")
(check-equal? (top-interp '{if false "hi" "bye"}) "\"bye\"")
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
(check-exn (regexp (regexp-quote "Invalid number of arguments for error DXUQ"))
           (lambda () {top-interp '{error 1 1 2}}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '{+ let if}}))

(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {top-interp '{+ true 5}}))

(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {top-interp '{- true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {top-interp '{* true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {top-interp '{/ true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {top-interp '{<= true 5}}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '{if 5 true 5}}))

(check-exn (regexp (regexp-quote "Different numbers of ids and args DXUQ"))
           (lambda () {top-interp '{{fn {a} {+ a 1}} 1 2}}))
(check-exn (regexp (regexp-quote "Name not found DXUQ"))
           (lambda () {top-interp '{{fn {a} {+ b 1}} 1}}))

(check-equal? (top-interp '{equal? "hello" "hello"}) "true")
(check-equal? (top-interp '{equal? "hello" "bye"}) "false")
{check-equal? (top-interp '{let {b = 5} {a = 5} in {+ a b}}) "10"}
(check-exn (regexp (regexp-quote "Invalid formatting for let statement DXUQ"))
           (lambda () {top-interp '{let {a = 12 1} in {+ a 2}}}))
(check-exn (regexp (regexp-quote "Invalid formatting for let statement DXUQ"))
           (lambda () {top-interp '{let {a =} in {+ a 2}}}))

(check-exn (regexp (regexp-quote "Applied arguments to non-function DXUQ"))
           (lambda () {top-interp '{5 4 3}}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '{}}))
(check-exn (regexp (regexp-quote "User Error DXUQ: \"hi\""))
           (lambda () {top-interp '{error "hi"}}))
(check-equal? (top-interp '(fn () 9)) "#<procedure>")
(check-equal? (top-interp '+) "#<primop>")
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '(fn (x x) 3)}))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {top-interp '(fn (3 4 5) 6)}))
(check-equal? (top-interp '((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")

(check-equal? (top-interp '{a := 2}) "")

(check-equal? (top-interp '{let
                              {p = {new-array 10 2}}
                            in
                            {aref p 3}}) "2")
(check-equal? (top-interp '{let
                               {p = {array "david" "reko"}}
                             in
                             p}) "#<array>")
(check-equal? (top-interp '{let
                               {p = {array "david" "reko"}}
                             in
                             {begin
                               {aset! p 1 2}
                               {aref p 1}}}) "2")

(check-equal? (top-interp '{let {fact = "bogus"}
  in
 {begin {fact := {fn {n} {if {<= n 0} 1 {* n {fact {- n 1}}}}}}
   {fact 12}}}) "479001600")

;;array test cases
(check-equal? (lookup 's (list (Bind 'a 1) (Bind 'b 2) (Bind 's 3))) 3)
(check-exn (regexp (regexp-quote "Name not found DXUQ"))
           (lambda () {lookup 's (list)}))
;(check-equal? (interp-equal (list (cloV (idC 's) '() '()) (cloV (idC 'a) '() '()))) (boolV #f))
(check-exn (regexp (regexp-quote "Invalid operands new-array DXUQ"))
           (lambda () {interp-new-array (list (strV "d") (strV "d"))}))
(check-exn (regexp (regexp-quote "Invalid new-array DXUQ"))
           (lambda () {interp-new-array (list (strV "d") (strV "d") (numV 3))}))
(check-exn (regexp (regexp-quote "Invalid operands for aref DXUQ"))
           (lambda () {interp-aref (list (strV "d") (strV "d"))}))
(check-exn (regexp (regexp-quote "Invalid number of operands for aref DXUQ"))
           (lambda () {interp-aref (list (strV "d") (strV "d") (numV 3))}))
(check-exn (regexp (regexp-quote "Invalid operands for aset! DXUQ"))
           (lambda () {interp-aset! (list (strV "d") (strV "d") (strV "david"))}))
(check-exn (regexp (regexp-quote "Invalid number of operands for aset! DXUQ"))
           (lambda () {interp-aset! (list (strV "d") (numV 3))}))
(check-equal? (interp-substring (list (strV "david") (numV 1) (numV 3))) (strV "av"))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () {interp-substring (list (strV "d") (strV "d"))}))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () {interp-substring (list (strV "d") (numV 4) (numV 3))}))
(check-exn (regexp (regexp-quote "Index out of bounds DXUQ"))
           (lambda () {interp-aref (list (arrayV 2 3) (numV 100))}))
(check-exn (regexp (regexp-quote "Index out of bounds DXUQ"))
           (lambda () {interp-aset! (list (arrayV 2 3) (numV 100) (strV "david"))}))

(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () {parse '(:= := 5)}))
;invalid format DXUQ
(check-equal? (top-interp (quasiquote {let {while = (unquote while)} in
                            {let {in-order = (unquote in-order)} in {in-order {array 1 2 3} 3}}})) "true")
|#
(check-equal? (serialize (numV 1)) "1")
(check-equal? (serialize (strV "hello")) "\"hello\"")
(check-equal? (serialize (boolV true)) "true")
(check-equal? (serialize (boolV false)) "false")
(check-equal? (serialize (closV '(a) (appC (numC 0) (list (numC 1))) top-env)) "#<procedure>")