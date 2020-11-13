#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC Value recC))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([body : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC] [argTypes : (Listof Type)]) #:transparent)
(struct recC ([fnName : Symbol] [params : (Listof Symbol)] [paramTs : (Listof Type)]
                              [retT : Type] [body : ExprC] [use : ExprC]) #:transparent)

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

(define-type TEnv (Mutable-HashTable Symbol Type))

(define-type Store (Mutable-HashTable Integer Value))

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)

(define reserved '(if : = let in rec fn ->))
(define primitives '(+ - * / <= num-eq? str-eq? substring))


; interprets a DXUQ expression into a Value
(: interp (-> ExprC Env Store Value))
(define (interp exp env sto)
  (match exp
    [(idC sym) (lookup-env sym env sto)]
    [(condC if then else) (interp-cond if then else env sto)]
    [(appC body args)
     (define interpretedBody (interp body env sto))
     (match interpretedBody
       [(cloV clo-body ids clo-env)
        (define interpretedArgs (map (lambda ([arg : ExprC]) (interp arg env sto)) args))
        (define new-env (extend-env ids interpretedArgs clo-env sto))
        (interp clo-body new-env sto)]
       [(primV func) (func (map (lambda ([arg : ExprC]) (interp arg env sto)) args))])]
    [(lamC ids body types) (cloV body ids env)]
    [(recC fnName params paramTs retT body use)
     (define nextIndex (hash-count sto))
     (define newEnv (cons (Bind fnName nextIndex) env))
     (hash-set! sto nextIndex (cloV body params newEnv))
     (interp use newEnv sto)]
    [(numV val) exp]
    [(strV val) exp]
    [(boolV val) exp]))


; returns extended environment including given symbols/ExprC's
(: extend-env (-> (Listof Symbol) (Listof Value) Env Store Env))
(define (extend-env symbols args env sto)
  (cond
    [(empty? symbols) env]
    [else (define nextIndex (hash-count sto))
          (hash-set! sto nextIndex (first args))
          (cons (Bind (first symbols) nextIndex) (extend-env (rest symbols) (rest args) env sto))]))


; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Store Value))
(define (lookup-env sym env sto)
  (define loc (lookup sym env))
  (hash-ref sto loc))


; gets the location associated with given id in Env
(: lookup (-> Symbol Env Integer))
(define (lookup sym env)
 (cond
    [(equal? (Bind-name (first env)) sym) (Bind-loc (first env))]
    [else (lookup sym (rest env))]))


; interprets addition primitive
(: interp-add (-> (Listof Value) numV))
(define (interp-add args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV m) (numV (+ n m))])]))


; interprets subtraction primitive
(: interp-sub (-> (Listof Value) numV))
(define (interp-sub args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV m) (numV (- n m))])]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) numV))
(define (interp-mult args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV m) (numV (* n m))])]))


; interprets division primitive
(: interp-div (-> (Listof Value) numV))
(define (interp-div args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV (? natural? m)) (numV (/ n m))])]))


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) boolV))
(define (interp-leq args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV m) (boolV (<= n m))])]))


; interprets a num-eq? primitive
(: interp-num-eq (-> (Listof Value) boolV))
(define (interp-num-eq args)
  (match (first args)
    [(numV n) (match (second args)
                [(numV m) (boolV (equal? n m))])]))


; interprets a str-eq? primitive
(: interp-str-eq (-> (Listof Value) boolV))
(define (interp-str-eq args)
  (match (first args)
    [(strV s1) (match (second args)
                 [(strV s2) (boolV (equal? s1 s2))])]))


; returns str[begin:end] excluding char @ index end
(: interp-substring (-> (Listof Value) Value))
(define (interp-substring args)
  (match args
    [(list (strV str) (numV (? natural? n)) (numV (? natural? m)))
           #:when (and (>= n 0) (< n m) (<= m (string-length str))) (strV (substring str n m))]
    [other (error "Invalid operands for substring DXUQ")]))


; interprets a DXUQ if statement and returns a Value
(: interp-cond (-> ExprC ExprC ExprC Env Store Value))
(define (interp-cond if then else env sto)
  (match (interp if env sto)
    [(boolV val) (cond
                   [val (interp then env sto)]
                   [else (interp else env sto)])]))


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
                                                                            [other (error 'DXUQ "cannot parse ~e" ty_id)])) ty_ids))
                                       (define ids (map (lambda (ty_id) (match ty_id
                                                                            [(list type (? symbol? id)) id])) ty_ids))
                                       (lamC ids (parse expr) types)]
    [(list 'rec (list (list (? symbol? s) (list t (? symbol? s2)) ...) ': t2 expr) expr2)
     (recC  s (cast s2 (Listof Symbol))
            (map (lambda ([x : Sexp]) (parse-type x)) (cast t (Listof Sexp))) (parse-type t2) (parse expr) (parse expr2))]
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
    [other (error 'DXUQ "Cannot parse ~e into a type" exp)])) 


; desugars a let statement into a function application (appC)
(: parse-let (-> (Listof Any) Sexp ExprC))
(define (parse-let mappings body)
  (define ty_ids (map (lambda (mapping) (match mapping
                                       [(list type (? symbol? s) '= expr) (list type s)]
                                       [other (error 'DXUQ "Invalid formatting for let statement ~e" mapping)])) mappings))
  (define args (map (lambda (mapping) (match mapping
                                        [(list type (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ty_ids body) args) Sexp)))


; proves that the given expression is valid. Throws error if invalid
(: type-check (-> ExprC TEnv Type))
(define (type-check exp tenv)
  (match exp
    [(idC sym) (cond
                 [(member sym (hash-keys tenv)) (hash-ref tenv sym)]
                 [else (error 'DXUQ "Unbound variable ~e" sym)])]
    [(condC if then else) (match (type-check if tenv)
                            [(boolT) (define thenType (type-check then tenv))
                                     (define elseType (type-check else tenv))
                                     (cond
                                       [(equal? thenType elseType) thenType]
                                       [else (error 'DXUQ "~e and ~e types don't match in if statement" thenType elseType)])]
                            [other (error 'DXUQ "If statement condition not of type boolean")])]
    [(appC body args) (match (type-check body tenv)
                        [(fnT paramTs returnT) (check-param-types paramTs args tenv) returnT]
                        [other (error 'ERROR "Arguments applied to non-function ~e" body)])]
    [(lamC ids body types) (extend-tenv ids types tenv) (fnT types (type-check body tenv))]
    [(recC fnName params paramTs returnT body use)
     (begin
       (define fnType (fnT paramTs returnT))
       (extend-tenv (cons fnName params) (cons fnType paramTs) tenv)
       (cond
         [(equal? (type-check body tenv) returnT) (type-check use tenv)]
         [else (error 'DXUQ "Invalid recursive function ~e" fnName)]))]
    [(numV n) (numT)]
    [(strV str) (strT)]))


; extends tenv to include mappings of lamc symbols to types
(: extend-tenv (-> (Listof Symbol) (Listof Type) TEnv TEnv))
(define (extend-tenv ids types tenv)
  (cond
    [(empty? ids) tenv]
    [else (hash-set! tenv (first ids) (first types)) (extend-tenv (rest ids) (rest types) tenv)]))


; verifies appC's have correct arg types
(: check-param-types (-> (Listof Type) (Listof ExprC) TEnv Any))
(define (check-param-types targetTypes args tenv)
  (cond
    [(xor (empty? targetTypes) (empty? args)) (error 'DXUQ "unequal numbers of params and args")]
    [(empty? targetTypes) 0]
    [(equal? (first targetTypes) (type-check (first args) tenv)) (check-param-types (rest targetTypes) (rest args) tenv)]
    [else (error 'DXUQ "type mismatch. Expected ~e but got ~e" (first targetTypes) (type-check (first args) tenv))]))


(define top-env (list (Bind '+ 0)
                      (Bind '- 1)
                      (Bind '* 2)
                      (Bind '/ 3)
                      (Bind '<= 4)
                      (Bind 'num-eq? 5)
                      (Bind 'str-eq? 6)
                      (Bind 'substring 7)
                      (Bind 'true 8)
                      (Bind 'false 9)))


(define top-store
  (ann (make-hash
        (list (cons 0 (primV interp-add))
              (cons 1 (primV interp-sub))
              (cons 2 (primV interp-mult))
              (cons 3 (primV interp-div))
              (cons 4 (primV interp-leq))
              (cons 5 (primV interp-num-eq))
              (cons 6 (primV interp-str-eq))
              (cons 7 (primV interp-substring))
              (cons 8 (boolV #t))
              (cons 9 (boolV #f))))
       Store))


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



; tests for parse
(check-equal? (parse '{let {num s = 5} in {+ s s}})
              (appC (lamC (list 's) (appC (idC '+) (list (idC 's) (idC 's))) (list (numT))) (list (numV 5))))
(check-equal? (parse '{if true "hi" "bye"}) (condC (idC 'true) (strV "hi") (strV "bye")))
(check-exn (regexp (regexp-quote "Invalid formatting for let statement '(bool n = 5 3)"))
           (lambda () (parse '{let {bool n = 5 3} in {+ n n}})))
(check-exn (regexp (regexp-quote "cannot parse '(bool m k)"))
           (lambda () (parse '{fn {[bool n] [bool m k]} {+ 1 2}})))
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () (parse '{fn k m s})))


;tests for parse-type
(check-equal? (parse-type '{str bool -> bool}) (fnT (list (strT) (boolT)) (boolT)))
(check-exn (regexp (regexp-quote "Cannot parse '(sa efo we a) into a type"))
           (lambda () (parse-type '{sa efo we a})))


; tests for type-check
(check-equal? (type-check (parse '{let {num s = 5} in {+ s s}}) top-t-env)
              (numT))
(check-equal? (type-check (parse '{+ 1 4}) top-t-env) (numT))
(check-equal? (type-check (parse '{num-eq? 1 4}) top-t-env) (boolT))
(check-equal? (type-check (parse '{if true true false}) top-t-env) (boolT))
(check-equal? (type-check (parse '{if true "hi" "bye"}) top-t-env) (strT))
(check-exn (regexp (regexp-quote "unequal numbers of params and args"))
           (lambda () {type-check (parse '{+ 1 2 3}) top-t-env}))
(check-exn (regexp (regexp-quote "type mismatch. Expected (numT) but got (boolT)"))
           (lambda () {type-check (parse '{+ true 3}) top-t-env}))
(check-exn (regexp (regexp-quote "type mismatch. Expected (numT) but got (boolT)"))
           (lambda () {type-check (parse '{{fn {[num x]} {+ x x}} true}) top-t-env}))
(check-exn (regexp (regexp-quote "If statement condition not of type boolean"))
           (lambda () {type-check (parse '{if 1 2 3}) top-t-env}))
(check-exn (regexp (regexp-quote "(boolT) and (fnT (list (numT)) (numT)) types don't match in if statement"))
           (lambda () {type-check (parse '{if true false {fn {[num n]} n}}) top-t-env}))
(check-exn (regexp (regexp-quote "Unbound variable 'hello"))
           (lambda () {type-check (parse '{hello 1 2}) top-t-env}))
(check-exn (regexp (regexp-quote "Arguments applied to non-function (numV 1432)"))
           (lambda () {type-check (parse '{1432 1 2}) top-t-env}))
(check-equal? (type-check (parse '{rec
                                      {{factorial [num n]} : num {if {<= n 0} 1 {* n {factorial {- n 1}}}}}
                                    {factorial 3}}) top-t-env) (numT))
(check-exn (regexp (regexp-quote "Invalid recursive function 'factorial"))
           (lambda () (type-check (parse '{rec
                                              {{factorial [num n]} : num {if {<= n 0} true {num-eq? n {factorial {- n 1}}}}}
                                            {factorial 3}}) top-t-env)))
(check-equal? (interp (parse '{rec
                                  {{factorial [num n]} : num {if {<= n 0} 1 {* n {factorial {- n 1}}}}}
                                {factorial 3}}) top-env top-store) (numV 6))


; tests for primitives
(check-equal? (interp (appC (idC '+) (list (numV 1) (numV 2))) top-env top-store) (numV 3))
(check-equal? (interp (appC (idC '-) (list (numV 1) (numV 2))) top-env top-store) (numV -1))
(check-equal? (interp (appC (idC '*) (list (numV 1) (numV 2))) top-env top-store) (numV 2))
(check-equal? (interp (appC (idC '/) (list (numV 2) (numV 2))) top-env top-store) (numV 1))
(check-equal? (interp (appC (idC '<=) (list (numV 1) (numV 2))) top-env top-store) (boolV #t))
(check-equal? (interp (appC (idC 'substring) (list (strV "hello") (numV 0) (numV 2))) top-env top-store) (strV "he"))
(check-exn (regexp (regexp-quote "Invalid operands for substring DXUQ"))
           (lambda () {interp (parse '{substring "hello there" 3 0}) top-env top-store}))
(check-equal? (interp (appC (idC 'num-eq?) (list (numV 1) (numV 1))) top-env top-store) (boolV #t))
(check-equal? (interp (appC (idC 'num-eq?) (list (numV 1) (numV 2))) top-env top-store) (boolV #f))
(check-equal? (interp (appC (idC 'str-eq?) (list (strV "magic") (strV "magic"))) top-env top-store) (boolV #t))
(check-equal? (interp (appC (idC 'str-eq?) (list (strV "magic") (strV "pen"))) top-env top-store) (boolV #f))

