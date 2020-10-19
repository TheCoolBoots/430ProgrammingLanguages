#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC Value))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([target : lamC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value]) #:transparent)

(define-type Value (U numV strV primV boolV cloV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([target : lamC] [args : (Listof ExprC)] [clo-env : Env]) #:transparent)

(define primitives (list '+ '- '* '/ '<= 'equal? 'true 'false 'error))


; interprets a DXUQ expression into a Value
(: interp (-> ExprC Env Value))
(define (interp exp env)
  (match exp
    [(idC sym) (interp (lookup-env sym env) env)]
    ; look up symbol in environment, return binding if symbol exists in environment
    [(appC function params) (define closure (gen-cloV exp env)) (interp closure (cloV-clo-env closure))]
    ; return a cloV with env extended to include mappings from lamC ids to params
    ; [(appC function params) (gen-cloV exp env)]
    ; return a cloV with env extended to include mappings from lamC ids to params
    [(condC if then else) (interp-cond if then else env)]
    ; if(if), then return then, else return else
    [(lamC ids body) (interp body env)]          ; not sure what to do here
    [(primV op args) (interp-prim op args env)]
    [(cloV target args clo-env) (interp target clo-env)]
    [(numV val) exp]
    [(strV val) exp]
    [(boolV val) exp]))


; interprets a primitive operator into a single value
(: interp-prim (-> Symbol (Listof ExprC) Env Value))
(define (interp-prim sym args env)
  (match sym
    ['+ (cond
          [(<= (length args) 1) (error "Invalid number of arguments for + DXUQ")]
          [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                (interp-add interpreted-args)])]
    ['- (cond
          [(<= (length args) 1) (error "Invalid number of arguments for - DXUQ")]
          [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                (interp-sub interpreted-args)])]
    ['* (cond
          [(<= (length args) 1) (error "Invalid number of arguments for * DXUQ")]
          [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                (interp-mult interpreted-args)])]
    ['/ (cond
          [(<= (length args) 1) (error "Invalid number of arguments for / DXUQ")]
          [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                (interp-div interpreted-args)])]
    ['<= (cond
           [(not (eq? (length args) 2)) (error "Invalid number of arguments for <= DXUQ")]
           [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                (interp-leq interpreted-args)])]
    ['equal? (cond
           [(<= (length args) 1) (error "Invalid number of arguments for equal? DXUQ")]
           [else (define interpreted-args (map (lambda ([arg : ExprC]) (interp arg env)) args))
                 (define firstElement (first interpreted-args))
                (boolV (andmap (lambda (a) (equal? a firstElement)) (rest interpreted-args)))])]
    ['error (error (string-append "User Error DXUQ: " (serialize (interp (first args) env))))])) ; need to put serialization of parameter into %s


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) boolV))
(define (interp-leq args)
  (cond
    [(and (numV? (first args)) (numV? (second args)))
     (boolV (<= (numV-val (cast (first args) numV)) (numV-val (cast (second args) numV)))) ]
    [else (error "Invalid operands for DXUQ <=")]))


; interprets addition primitive
(: interp-add (-> (Listof Value) numV))
(define (interp-add args)
  (cond
    [(empty? args) (numV 0)]
    [else (match (first args)
            [(numV n) (numV (+ n (numV-val (interp-add (rest args)))) )]
            [other (error "Invalid operands for DXUQ +")] )] ))


; interprets subtraction primitive
(: interp-sub (-> (Listof Value) numV))
(define (interp-sub args)
  (cond
    [(andmap numV? args)
     (define newArgs
       (cons (first args) (map (lambda ([arg : numV]) (numV (* -1 (numV-val arg)))) (rest args)) ) )
     (interp-add newArgs)]
    [else (error "Invalid operands for DXUQ -")]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) numV))
(define (interp-mult args)
  (cond
    [(empty? args) (numV 1)]
    [else (match (first args)
            [(numV n) (numV (* n (numV-val (interp-mult (rest args)))) )]
            [other (error "Invalid operands for DXUQ *")] )] ))


; interprets division primitive
(: interp-div (-> (Listof Value) numV))
(define (interp-div args)
  (cond
    [(and (andmap numV? args) (andmap (lambda ([n : numV]) (not (eq? (numV-val n) 0))) (rest args)))
     (define newArgs (cons (first args) (map (lambda ([arg : numV]) (numV (/ 1 (numV-val arg)))) (rest args)) ) )
     (interp-mult newArgs)]
    [else (error "Invalid operands for DXUQ /")]))


; interprets a DXUQ if statement and returns a Value
(: interp-cond (-> ExprC ExprC ExprC Env Value))
(define (interp-cond if then else env)
  (match (interp if env)
    [(boolV val) (cond
                   [val (interp then env)]
                   [else (interp else env)])]
    [other (error "Invalid operands for DXUQ if")]))



; make a binding for each symbol in appC-lamC-ids with each of the arguments in appC-args
; add the list of bindings to the env
; return a cloV with appC target, args, and new env
(: gen-cloV (-> appC Env cloV))
(define (gen-cloV app env)
  (define new-env (extend-env (lamC-ids (appC-target app)) (appC-args app) env))
  (cloV (appC-target app) (appC-args app) new-env))


; helper function for gen-cloV; returns extended environment
(: extend-env (-> (Listof Symbol) (Listof ExprC) Env Env))
(define (extend-env symbols args env)
  (cond
    [(xor (empty? symbols) (empty? args)) (error "Different numbers of symbols and args DXUQ")]
    [(empty? symbols) env]
    [else (cons (Bind (first symbols) (interp (first args) env)) (extend-env (rest symbols) (rest args) env))]))


; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Value))
(define (lookup-env sym env)
  (cond
    [(empty? env) (error "Environment binding not found DXUQ")]
    [(equal? (Bind-name (first env)) sym) (Bind-val (first env))]
    [else (lookup-env sym (rest env))]))
           

; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? n) (numV n)]
    [(? string? s) (strV s)]
    [(list (? symbol? s) args ...) #:when (member s primitives) (primV s (map (lambda (arg) (parse arg)) args))]
    [(list 'fn (list ids ...) expr) (lamC (cast ids (Listof Symbol)) (parse expr))]
    [(list 'if exprIf exprThen exprElse) (condC (parse exprIf) (parse exprThen) (parse exprElse))]
    [(list 'let mappings ... 'in body) (parse-let mappings body)]
    [(list expr args ...) (define parsedExpr (parse expr))
                          (cond
                            [(lamC? parsedExpr)
                             (appC (cast parsedExpr lamC) (map (lambda (arg) (parse arg)) args))]
                            ; change parse args to parse each arg and get a list of ExprC
                            [else (error "Invalid format DXUQ")])]
    [(? symbol? s) (idC s)]
    [other (error "Invalid format DXUQ")]))


; desugars a let statement into a function application (appC)
(: parse-let (-> (Listof Any) Sexp ExprC))
(define (parse-let mappings body)
  (define ids (map (lambda (mapping) (match mapping
                                       [(list (? symbol? s) '= expr) s]
                                       [other (error "Invalid formatting for let statement DXUQ")])) mappings))
  (define args (map (lambda (mapping) (match mapping
                                        [(list (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ids body) args) Sexp)))


; serializes a value into a string
(: serialize (-> Value String))
(define (serialize val)
  (match val
    [(numV val) (~v (~a val))]
    [(strV val) (~v val)]
    [(boolV val) (cond
                   [val (~v "true")]
                   [else (~v "false")])]))
    ;[(primV sym args) (~v "#<primop>")]
    ;[(lamC fun args) (~v "#<procedure>")]))

(define top-env (list (Bind 'true (boolV #t)) (Bind 'false (boolV #f))))

; interprets a DXUQ program into a string
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


(check-equal? (interp-prim '+ (list (numV 2) (numV 3) (numV 4)) '()) (numV 9))
(check-equal? (interp-prim '- (list (numV 2) (numV 3) (numV 4)) '()) (numV -5))
(check-equal? (interp-prim '- (list (numV 2) (numV 3)) '()) (numV -1))
(check-equal? (interp-prim '* (list (numV 2) (numV 3)) '()) (numV 6))
(check-equal? (interp-prim '/ (list (numV 8) (numV 2)) '()) (numV 4))
(check-equal? (interp-prim '/ (list (numV 8) (numV 2) (numV 2)) '()) (numV 2))


(check-equal? (interp-cond (boolV #t) (numV 1) (numV 2) '()) (numV 1))
(check-equal? (interp-cond (boolV #f) (numV 1) (numV 2) '()) (numV 2))


(check-equal? (extend-env '(a b c) (list (numV 1) (numV 2) (numV 3)) '())
              (list (Bind 'a (numV 1)) (Bind 'b (numV 2)) (Bind 'c (numV 3))))
(check-equal? (gen-cloV (appC (lamC '(a) (idC 'a)) (list (numV 3))) '())
              (cloV (lamC '(a) (idC 'a)) (list (numV 3)) (list (Bind 'a (numV 3)))))


(check-equal? (interp (idC 's) (list (Bind 's (numV 3)))) (numV 3))
(check-equal? (interp (idC 's) (list (Bind 's (strV "hi")))) (strV "hi"))
(check-equal? (interp (idC 's) (list (Bind 's (boolV #t)))) (boolV #t))


(check-equal? (parse '{{fn {a b} {+ a b}} 3 4})
              (appC (lamC (list 'a 'b) (primV '+ (list (idC 'a) (idC 'b)))) (list (numV 3) (numV 4))))
(check-equal? (parse '{let {a = 3} {b = 4} in {+ a b}})
              (appC (lamC (list 'a 'b) (primV '+ (list (idC 'a) (idC 'b)))) (list (numV 3) (numV 4))))


; {{fn {a b} {+ a b}} 3 4}
(check-equal? (interp
               (appC (lamC (list 'a 'b) (primV '+ (list (idC 'a) (idC 'b)))) (list (numV 3) (numV 4))) '())
              (numV 7))


(check-equal? (top-interp '{if {<= 3 5} 3 5}) "\"3\"")
(check-equal? (top-interp '{if true 3 5}) "\"3\"")
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
(check-equal? (top-interp '{if {equal? 2 2 2} 3 5}) "\"3\"")

(check-exn (regexp (regexp-quote "Invalid operands for DXUQ +"))
           (lambda () {top-interp '{+ true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ -"))
           (lambda () {top-interp '{- true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ *"))
           (lambda () {top-interp '{* true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ /"))
           (lambda () {top-interp '{/ true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ <="))
           (lambda () {top-interp '{<= true 5}}))
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ if"))
           (lambda () {top-interp '{if 5 true 5}}))

(check-exn (regexp (regexp-quote "Different numbers of symbols and args DXUQ"))
           (lambda () {top-interp '{{fn {a} {+ a 1}} 1 2}}))
(check-exn (regexp (regexp-quote "Environment binding not found DXUQ"))
           (lambda () {top-interp '{{fn {a} {+ b 1}} 1}}))

(check-equal? (top-interp '{equal? "hello" "hello"}) "\"true\"")
(check-equal? (top-interp '{equal? "hello" "bye" "hello"}) "\"false\"")
;{check-equal? (top-interp '{let {b = 5} {a = b} in {+ a b}}) "\"10\""}
;(check-equal? (top-interp '{let {b = 5 4} {a = b} in {+ a b}}) "\"10\"")
(check-exn (regexp (regexp-quote "Invalid formatting for let statement DXUQ"))
           (lambda () {top-interp '{let {a = 12 1} in {+ a 2}}}))
(check-exn (regexp (regexp-quote "Invalid formatting for let statement DXUQ"))
           (lambda () {top-interp '{let {a =} in {+ a 2}}}))

(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () {top-interp '{5 4 3}}))
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () {top-interp '{}}))
(check-exn (regexp (regexp-quote "User Error DXUQ: \"hi\""))
           (lambda () {top-interp '{error "hi"}}))

; (parse '{{g} 15})


;(check-equal? (top-interp '{fn {a b} {+ a b}}) "\"#<procedure>\"")