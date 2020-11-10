#lang typed/racket

(require typed/rackunit)


; not done yet
; on Extend your type checker to handle variables and if expressions.

; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC Value))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([body : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)

; definitions for Types
(define-type Type (U numT strT boolT funT))
(struct numT () #:transparent)
(struct strT () #:transparent)
(struct boolT () #:transparent)
(struct funT ([args : (Listof Type)] [ret : Type]) #:transparent)

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value]) #:transparent)

(define-type TEnv (Listof TBind))
(struct TBind ([name : Symbol] [t : Type]))

(define-type Value (U numV strV primV boolV cloV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([body : ExprC] [args : (Listof Symbol)] [clo-env : Env]) #:transparent)

(define reserved '(fn let if =))

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

; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? n) (numV n)]
    [(? string? s) (strV s)]
    [(? symbol? s) #:when (validID? s) (idC s)]
    [(list 'fn (list ids ...) expr) #:when
                                    (and (equal? (remove-duplicates ids) ids) (andmap (lambda (a) (symbol? a)) ids))
                                    (lamC (cast ids (Listof Symbol)) (parse expr))]
    [(list 'if exprIf exprThen exprElse) (condC (parse exprIf) (parse exprThen) (parse exprElse))]
    [(list 'let mappings ... 'in body) (parse-let mappings body)]
    [(list expr args ...) (appC (parse expr) (map (lambda (arg) (parse arg)) args))]
    [other (error "Invalid format DXUQ")]))

; parses the language of types
(: parse-type (-> Sexp Type))
(define (parse-type s)
  (match s
    ['num (numT)]
    ['str (strT)]
    ['bool (boolT)]
    [(list types ... '-> ret) (funT (map (lambda (a) (parse-type a)) (cast types (Listof Sexp))) (parse-type ret))]
    [other (error "Invalid type DXUQ")]))

; type checks a given exprC
(: type-check (-> ExprC TEnv Type))
(define (type-check e env)
  (match e
    [(numV n) (numT)]
    [(strV n) (strT)]
    [(boolV n) (boolT)]
    [other (error "Type-check failure DXUQ")]))

; checks to see if an ID is not a reserved DXUQ4 keyword
(: validID? (-> Symbol Boolean))
(define (validID? sym)
  (cond
    [(member sym reserved) #f]
    [else #t]))

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
                      (Bind 'error (primV interp-error))))

;test cases

(check-equal? (interp-cond (boolV #t) (numV 1) (numV 2) '()) (numV 1))
(check-equal? (interp-cond (boolV #f) (numV 1) (numV 2) '()) (numV 2))

(check-equal? (extend-env '(a b c) (list (numV 1) (numV 2) (numV 3)) '())
              (list (Bind 'a (numV 1)) (Bind 'b (numV 2)) (Bind 'c (numV 3))))

(check-equal? (interp (idC 's) (list (Bind 's (numV 3)))) (numV 3))
(check-equal? (interp (idC 's) (list (Bind 's (strV "hi")))) (strV "hi"))
(check-equal? (interp (idC 's) (list (Bind 's (boolV #t)))) (boolV #t))

(check-equal? (top-interp '{if {<= 3 5} 3 5}) "3")
(check-equal? (top-interp '{if true 3 5}) "3")
(check-equal? (top-interp '{if false "hi" "bye"}) "bye")
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
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
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
(check-exn (regexp (regexp-quote "Invalid operands for DXUQ if"))
           (lambda () {top-interp '{if 5 true 5}}))

(check-exn (regexp (regexp-quote "Different numbers of ids and args DXUQ"))
           (lambda () {top-interp '{{fn {a} {+ a 1}} 1 2}}))
(check-exn (regexp (regexp-quote "Environment binding not found DXUQ"))
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
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () {top-interp '{}}))
(check-exn (regexp (regexp-quote "User Error DXUQ: hi"))
           (lambda () {top-interp '{error "hi"}}))
(check-equal? (top-interp '(fn () 9)) "#<procedure>")
(check-equal? (top-interp '+) "#<primop>")
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () {top-interp '(fn (x x) 3)}))
(check-exn (regexp (regexp-quote "Invalid format DXUQ"))
           (lambda () {top-interp '(fn (3 4 5) 6)}))
(check-equal? (top-interp '((fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b))))) "3")
(check-equal? (interp (parse '((fn (minus) (minus 2 1)) (fn (x y) (- x y)))) top-env) (numV 1))

; test cases for parse-type
(check-equal? (parse-type 'num) (numT))
(check-equal? (parse-type 'str) (strT))
(check-equal? (parse-type 'bool) (boolT))
(check-equal? (parse-type '{bool num -> str}) (funT (list (boolT) (numT)) (strT)))
(check-equal? (parse-type '{str {num -> num} -> bool})
              (funT (list (strT) (funT (list (numT)) (numT))) (boolT)))
(check-exn (regexp (regexp-quote "Invalid type DXUQ"))
           (lambda () {parse-type '{+ 1}}))

(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {top-interp '{+ 1 +}}))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {top-interp '{- 1 +}}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {top-interp '{* 1 +}}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {top-interp '{/ 1 +}}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {top-interp '{<= 1 +}}))
(check-equal? (top-interp '{/ 1 1}) "1")
(check-equal? (top-interp '{equal? + +}) "false")

; test cases for check-type
(check-equal? (type-check (numV 10) '()) (numT))
(check-equal? (type-check (strV "david") '()) (strT))
(check-equal? (type-check (boolV #f) '()) (boolT))