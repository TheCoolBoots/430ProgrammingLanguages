#lang typed/racket

(require typed/rackunit)

;; Finished project, passes all test cases created by our group
;; as well as the handin server.

(define-type ExprC (U numC stringC ifC idC fnC appC lamC assignC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct fnC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [l : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct assignC ([id : Symbol] [body : ExprC]) #:transparent)

(define-type Value (U numV funV strV primV boolV closV arrayV nullV))
(struct numV ([n : Real]) #:transparent)
(struct funV ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct strV ([s : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct closV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct arrayV ([loc : Integer] [length : Integer]) #:transparent)
(struct nullV ())

(struct fundef ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)
(define-type Env (Listof Bind))


(define-type Store (Mutable-HashTable Integer Value))





(define keywords (list 'let 'in 'if 'fn ':=))


(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env top-store)))



; interprets a DXUQ2 expression and returns a real
(: interp (-> ExprC Env Store Value))
(define (interp exp env st)
  (match exp
    [(numC n) (numV n)]
    [(stringC s) (strV s)]
    [(lamC arg body) (closV arg body env)]
    [(assignC id body) (interp-assignC exp env st)]
    [(ifC test then else) (define b (interp test env st))
                          (match b
                            [(boolV bool) (cond
                                            [(equal? b (boolV #t)) (interp then env st)]
                                            [else (interp else env st)])]
                            [other (error "invalid format DXUQ")])]          
    [(appC fun args) (define interpretedBody (interp fun env st))
     (match interpretedBody
       [(closV ids clo-body clo-env)
        (define interpretedParams (map (lambda ([param : ExprC]) (interp param env st)) args))
        (define new-env (extend-env ids interpretedParams clo-env st))
        (interp clo-body new-env st)]
       [(primV symbol) (symbol (map (lambda ([param : ExprC]) (interp param env st)) args))]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(idC sym) 
                 (lookup-env sym env st)]))
    ;[other (error "unimplemented")]))



   
; returns extended environment including given symbols/ExprC's
(: extend-env (-> (Listof Symbol) (Listof Value) Env Store Env))
(define (extend-env symbols args env sto)
  (cond
    [(xor (empty? symbols) (empty? args)) (error "Different numbers of ids and args DXUQ")]
    [(empty? symbols) env]
    [else
     (define index (get-next-index sto))
     (allocate sto args)
     (cons (Bind (first symbols) index) (extend-env (rest symbols) (rest args) env sto))]))


; gets the next index in the hash table
(: allocate (-> Store (Listof Value) Integer))
(define (allocate sto args)
  (cond
    [(empty? args) (get-next-index sto)]
    [else (hash-set! sto (get-next-index sto) (first args))
          (get-next-index sto)]))

; returns the next available index in the given store
(: get-next-index (-> Store Integer))
(define (get-next-index sto)
  (+ 1 (hash-count sto)))

;(check-equal? (extend-env2 '() (list 'a 'b) (list (numV 1) (numV 2)))
;              (list (Bind 'a (numV 1)) (Bind 'b (numV 2))))
;(check-exn (regexp (regexp-quote "invalid format DXUQ"))
;           (lambda () (extend-env2 '() '() (list (numV 1)))))

(define (lookup-env [sym : Symbol] [env : Env] [st : Store]) : Value
  (define loc (lookup sym env))
  (hash-ref st loc))



;;helper for appC interp-args
;(define (interp-args [args : (Listof ExprC)] [env : Env]) : (Listof Value)
;  (cond
;    [(empty? args) '()]
;    [else (cons (interp (first args) env) (interp-args (rest args) env))]))

;(check-equal? (interp-args (list (numC 1) (stringC "hello")) '())
;              (list (numV 1) (strV "hello")))
 



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

(: interp-equal (-> (Listof Value) boolV))
(define (interp-equal args)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for equal? DXUQ")]
    [(or (closV? (first args)) (closV? (second args))
         (primV? (first args)) (primV? (second args))) (boolV #f)]
    [else (boolV (equal? (first args) (second args)))]))

(: interp-error (-> (Listof Value) numV))
(define (interp-error args)
  (cond
    [(not (equal? (length args) 1)) (error "Invalid number of arguments for error DXUQ")]
    [else (error (string-append "User Error DXUQ: " (serialize (first args))))]))

; interprets a mutation operator
(: interp-assignC (-> assignC Env Store Value))
(define (interp-assignC expr env sto)
  (define index (cond
                  [(in-env? (assignC-id expr) env) (lookup (assignC-id expr) env)]
                  [else (get-next-index sto)]))
  (define new-env (cons (Bind (assignC-id expr) index) env))
  (define interp-body (interp (assignC-body expr) new-env sto))
  (hash-set! sto index interp-body)
  (nullV))

; returns t/f if an id exists in the env
(: in-env? (-> Symbol Env Boolean))
(define (in-env? sym env)
  (cond
    [(empty? env) #f]
    [(equal? sym (Bind-name (first env))) #t]
    [else (in-env? sym (rest env))]))

; interprets a DXUQ if statement and returns a Value
;(: interp-cond (-> ExprC ExprC ExprC Env Value))
;(define (interp-cond if then else env)
;  (match (interp if env)
;    [(boolV val) (cond
;                   [val (interp then env)]
;                   [else (interp else env)])]
;    [other (error "Invalid operands for DXUQ if")]))

;(check-equal? interp



;;take a symbol and environment, return value
(define (lookup [for : Symbol] [env : Env]) : Integer
  (cond
    [(empty? env) (error "Name not found DXUQ")]
    [else (cond
            [(equal? for (Bind-name (first env))) (Bind-loc (first env))]
            [else (lookup for (rest env))])]))

;(check-equal? (lookup 'x (list (Bind 'a (numV 1)) (Bind 'x (numV 2)))) (numV 2))
;(check-exn (regexp (regexp-quote "Name not found DXUQ"))
;           (lambda () (lookup 'a (list))))


;;takes a value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (~v n)]
    [(strV s) (~v s)]
    [(primV op) "#<primop>"]
    [(closV arg body env) "#<procedure>"]
    [(arrayV first body) "#<array>"]
    [(boolV val) (cond
                   [val "true"]
                   [else "false"])]))
(check-equal? (serialize (numV 1)) "1")
(check-equal? (serialize (strV "hello")) "\"hello\"")
(check-equal? (serialize (boolV true)) "true")
(check-equal? (serialize (boolV false)) "false")
;(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (closV '(a) (appC (numC 0) (list (numC 1))) '())) "#<procedure>")


;;takes an s-expr, returns a ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) (numC r)]
    [(list (? symbol? id) ':= body) (cond
                          [(ExprC-keyword? id keywords) (error "invalid format DXUQ")]
                          [else (assignC id (parse body))])]
    [(? symbol? sym) (cond
                       [(ExprC-keyword? sym keywords) (error "invalid format DXUQ")]
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

;(define (get-begin [exp : ExprC]) : Value

; interprets a new-array statement, returns the array
(: interp-new-array (-> (Listof Value) Value))
(define (interp-new-array args)
  (cond
    [(equal? (length args) 2) (match args
                           [(list (numV (? natural? n)) val) (define index (get-next-index top-store))
                                                           (make-new-array n val)
                                                           (arrayV index n)]
                           [other (error "Invalid operands new-array DXUQ")])]
    [else (error "Invalid new-array DXUQ")]))

          
; returns the first location of an array in memory
(: make-new-array (-> Integer Value Integer))
(define (make-new-array length val)
  (cond
    [(= 0 length) 1]
    [else (hash-set! top-store (get-next-index top-store) val)
          (make-new-array (- length 1) val)]))

; interprets an array statement, returns the array
(: interp-array (-> (Listof Value) Value))
(define (interp-array args)
  (define index (get-next-index top-store))
  (map (lambda ([a : Value]) (hash-set! top-store (get-next-index top-store) a)) args)
  (arrayV index (length args)))

; returns the value in the array at a given index
(: interp-aref (-> (Listof Value) Value))
(define (interp-aref args)
  (cond
    [(equal? (length args) 2) (match args
                                  [(list (arrayV loc len) (numV (? natural? n))) (cond
                                                                                   [(< n len) (hash-ref top-store (+ n loc))]
                                                                                   [else (error "Index out of bounds DXUQ")])]
                                  [other (error "Invalid operands for aref DXUQ")])]
    [else (error "Invalid number of operands for aref DXUQ")]))

; sets a value in a given array to a new value
(: interp-aset! (-> (Listof Value) Value))
(define (interp-aset! args)
  (cond
    [(equal? (length args) 3) (match args
                                [(list (arrayV loc len) (numV (? natural? n)) new) (cond
                                                                                     [(< n len) (hash-set! top-store (+ n loc) new)
                                                                                      (nullV)]
                                                                                     [else (error "Index out of bounds DXUQ")])]
                                [other (error "Invalid operands for aset! DXUQ")])]
    [else (error "Invalid number of operands for aset! DXUQ")]))
                                                                                   
; interprets the begin operation
(: interp-begin (-> (Listof Value) Value))
(define (interp-begin args)
  (cond
    [(equal? (length args) 1) (first args)]
    [else (first args) (interp-begin (rest args))]))
  
(: interp-substring (-> (Listof Value) Value))
(define (interp-substring args)
  (match args
    [(list (strV str) (numV (? natural? n1)) (numV (? natural? n2)))
     (cond
       [(and (< n1 n2) (>= n1 0) (< n2 (string-length str))) (strV (substring str n1 n2))]
       [else (error "Invalid substring operation DXUQ")])]
    [other (error "Invalid substring operation DXUQ")]))


(define top-env (list (Bind '+ 1)
                        (Bind '- 2)
                        (Bind '* 3)
                        (Bind '/ 4)
                        (Bind '<= 5)
                        (Bind 'equal? 6)
                        (Bind 'true 7)
                        (Bind 'false 8)
                        (Bind 'error 9)
                        (Bind 'new-array 10)
                        (Bind 'array 11)
                        (Bind 'aref 12)
                        (Bind 'aset! 13)
                        (Bind 'begin 14)
                        (Bind 'substring 15)))

(define top-store (ann (make-hash
                        (list (cons 1 (primV interp-add))
                              (cons 2 (primV interp-sub))
                              (cons 3 (primV interp-mult))
                              (cons 4 (primV interp-div))
                              (cons 5 (primV interp-leq))
                              (cons 6 (primV interp-equal))
                              (cons 7 (boolV #t))
                              (cons 8 (boolV #f))
                              (cons 9 (primV interp-error))
                              (cons 10 (primV interp-new-array))
                              (cons 11 (primV interp-array))
                              (cons 12 (primV interp-aref))
                              (cons 13 (primV interp-aset!))
                              (cons 14 (primV interp-begin))
                              (cons 15 (primV interp-substring))))
                       Store))


#|(check-equal? (parse 1) (numC 1))
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
|#
