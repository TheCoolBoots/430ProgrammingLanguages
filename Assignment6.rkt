#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC Value))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([body : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC] [argTypes : (Listof Type)]) #:transparent)           ;  [argTypes : (Listof Type)]

(define-type Value (U numV strV primV boolV cloV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([body : ExprC] [args : (Listof Symbol)] [clo-env : Env]) #:transparent)

(define-type Type (U numT strT boolT fnT))
(struct numT () #:transparent)
(struct strT () #:transparent)
(struct boolT () #:transparent)
(struct fnT ([paramTs : (Listof Type)] [returnT : Type]) #:transparent)

(define-type Env (Mutable-HashTable Symbol Value))
(define-type TEnv (Mutable-HashTable Symbol Type))

(define reserved '(if : = let in rec fn ->))

#|
; interprets a DXUQ expression into a Value
(: interp (-> ExprC Env Value))
(define (interp exp env)
  (match exp
    [(idC sym) (lookup-env sym env)]
    [(condC if then else) (interp-cond if then else env)]
    [(appC body args)
     (define interpretedBody (interp body env))
     (match interpretedBody
       [(cloV clo-body ids clo-env)
        (define interpretedArgs (map (lambda ([arg : ExprC]) (interp arg env)) args))
        (define new-env (extend-env ids interpretedArgs clo-env))
        (interp clo-body new-env)]
       [(primV function) (function (map (lambda ([arg : ExprC]) (interp arg env)) args))]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(lamC ids body) (cloV body ids env)]
    [(boolV val) exp]
    [(numV n) exp]
    [(strV str) exp]))



; returns extended environment including given symbols/ExprC's
(: extend-env (-> (Listof Symbol) (Listof Value) Env Env))
(define (extend-env symbols args env)
  (cond
    [(xor (empty? symbols) (empty? args)) (error "Different numbers of ids and args DXUQ")]
    [(empty? symbols) env]
    [else (cons (Bind (first symbols) (first args)) (extend-env (rest symbols) (rest args) env))]))


; interprets user-defined error
(: interp-error (-> (Listof Value) numV))
(define (interp-error args)
  (cond
    [(not (equal? (length args) 1)) (error "Invalid number of arguments for error DXUQ")]
    [else (error (string-append "User Error DXUQ: " (serialize (first args))))]))


; interprets equal?
(: interp-equal (-> (Listof Value) boolV))
(define (interp-equal args)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for equal? DXUQ")]
    [(or (cloV? (first args)) (cloV? (second args))
         (primV? (first args)) (primV? (second args))) (boolV #f)]
    [else (boolV (equal? (first args) (second args)))]))


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
(: interp-div (-> (Listof Value) numV))
(define (interp-div args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for / DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV (? natural? m)) (numV (/ n m))]
                        [other (error "Invalid operands for / DXUQ")])]
            [other (error "Invalid operands for / DXUQ")])]))


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) boolV))
(define (interp-leq args)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for <= DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (boolV (<= n m))]
                        [other (error "Invalid operands for <= DXUQ")])]
            [other (error "Invalid operands for <= DXUQ")])]))


; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Value))
(define (lookup-env sym env)
  (cond
    [(empty? env) (error "Environment binding not found DXUQ")]
    [(equal? (Bind-name (first env)) sym) (Bind-val (first env))]
    [else (lookup-env sym (rest env))]))         


; interprets a DXUQ if statement and returns a Value
(: interp-cond (-> ExprC ExprC ExprC Env Value))
(define (interp-cond if then else env)
  (match (interp if env)
    [(boolV val) (cond
                   [val (interp then env)]
                   [else (interp else env)])]
    [other (error "Invalid operands for DXUQ if")]))

; serializes a value into a string
(: serialize (-> Value String))
(define (serialize val)
  (match val
    [(numV val) (~v val)]
    [(strV val) val]
    [(boolV val) (cond
                   [val "true"]
                   [else "false"])]
    [(primV sym) "#<primop>"]
    [(cloV body ids env) "#<procedure>"]))


; interprets a DXUQ program into a string
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

(define top-env (list (Bind 'true (boolV #t))
                      (Bind 'false (boolV #f))
                      (Bind '+ (primV interp-add))
                      (Bind '- (primV interp-sub))
                      (Bind '* (primV interp-mult))
                      (Bind '/ (primV interp-div))
                      (Bind '<= (primV interp-leq))
                      (Bind 'equal? (primV interp-equal))
                      (Bind 'error (primV interp-error))))|#

(define top-t-env
  (ann (make-hash
        (list (cons '+ (fnT (list (numT) (numT)) (numT)))
              (cons '- (fnT (list (numT) (numT)) (numT)))
              (cons '* (fnT (list (numT) (numT)) (numT)))
              (cons '/ (fnT (list (numT) (numT)) (numT)))
              (cons '<= (fnT (list (numT) (numT)) (boolT)))
              (cons 'num-eq? (fnT (list (numT) (numT)) (boolT)))
              (cons 'str-eq? (fnT (list (strT) (strT)) (boolT)))
              (cons 'substring (fnT (list (strT) (numT)) (numT)))
              (cons 'true (boolT))
              (cons 'false (boolT))))
       TEnv))


; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? n) (numV n)]
    [(? string? s) (strV s)]
    [(? symbol? s) #:when (not (member s reserved)) (idC s)]
    [(list 'if exprIf exprThen exprElse) (condC (parse exprIf) (parse exprThen) (parse exprElse))]
    [(list 'let mappings ... 'in body) (parse-let mappings body)]
    [(list 'fn (list ty_ids ...) expr) (define types (map (lambda (ty_id) (match ty_id
                                                                            [(list type (? symbol? id)) (parse-type (cast type Sexp))]
                                                                            [other (error 'ERROR "cannot parse ~e" ty_id)])) ty_ids))
                                       (define ids (map (lambda (ty_id) (match ty_id
                                                                            [(list type (? symbol? id)) id])) ty_ids))
                                       (lamC ids (parse expr) types)]
    [(list 'rec (list (list fnName ty_ids ...) ': returnTy recursiveBody) body) (numV 0)]  ; desugars into something like a let 
    [(list expr args ...) (appC (parse expr) (map (lambda (arg) (parse arg)) args))]
    [other (error "Invalid format DXUQ")]))


; parses s-expr into type
(: parse-type (-> Sexp Type))
(define (parse-type exp)
  (match exp
    ['num (numT)]
    ['bool (boolT)]
    ['str (strT)]
    [(list ty ... '-> rety) (fnT (map (lambda (type) (parse-type type)) (cast ty (Listof Sexp))) (parse-type rety))]
    [other (error 'ERROR "Cannot parse ~e into a type DXUQ6" exp)])) 


; desugars a let statement into a function application (appC)
(: parse-let (-> (Listof Any) Sexp ExprC))
(define (parse-let mappings body)
  (define ty_ids (map (lambda (mapping) (match mapping
                                       [(list type (? symbol? s) '= expr) (list type s)]
                                       [other (error "Invalid formatting for let statement DXUQ")])) mappings))
  (define args (map (lambda (mapping) (match mapping
                                        [(list type (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ty_ids body) args) Sexp)))


; proves that the given expression is valid. Throws error if invalid
(: type-check (-> ExprC TEnv Type))
(define (type-check exp tenv)
  (match exp
    [(idC sym) (hash-ref tenv sym)] ; return type in type-env
    [(condC if then else) (match (type-check if tenv)
                            [(boolT) (define thenType (type-check then tenv))
                                     (define elseType (type-check else tenv))
                                     (cond
                                       [(equal? thenType elseType) thenType]
                                       [else (error 'ERROR "~e and ~e types don't match in if statement DXUQ" then else)])]
                            [other (error 'ERROR "~e not of type boolean DXUQ" if)])]; make sure type of if is boolean, then & else have same type, return type of then
    [(appC body args) (numT)] ; arg types must match param types of body, extend type-env, return type of body return
    [(lamC ids body types) (fnT types (type-check body tenv))] ; return type of body
    [(boolV val) (boolT)]
    [(numV n) (numT)]
    [(strV str) (strT)]))


(: type-check-app (-> ExprC (Listof ExprC) TEnv Type))
(define (type-check-app lamc args tenv)
  (match lamc
    ; prove lamc is fn (t1 ...) -> t2
    ; prove arg types match (t1 ...)
    ; extend type env to include mappings of symbols
    ; prove lamc->body is of t2
    [(lamC ids body types) (define newTEnv (argTypesValid? ids types args tenv))
                           (type-check body newTEnv)]
    [other (error 'ERROR "Arguments applied to non-function ~e DXUQ" lamc)]))


; helper function for checking appC param and arg types
(: argTypesValid? (-> (Listof Symbol) (Listof Type) (Listof ExprC) TEnv TEnv))
(define (argTypesValid? ids targetTypes args tenv)
  (cond
    [(xor (empty? targetTypes) (empty? args)) (error 'ERROR "unequal numbers of params and args DXUQ")]
    [(empty? targetTypes) tenv]
    [(equal? (first targetTypes) (type-check (first args) tenv)) (hash-set! tenv (first ids) (first targetTypes))
                                                                 (argTypesValid? (rest ids) (rest targetTypes) (rest args) tenv)]
    [else (error 'ERROR "type mismatch. Expected ~e but got ~e" (first targetTypes) (type-check (first args) tenv))]))

; tests for parse
(check-equal? (parse '{let {num s = 5} in {+ s s}})
              (appC (lamC (list 's) (appC (idC '+) (list (idC 's) (idC 's))) (list (numT))) (list (numV 5))))
(check-equal? (parse '{if true "hi" "bye"}) (condC (idC 'true) (strV "hi") (strV "bye")))


