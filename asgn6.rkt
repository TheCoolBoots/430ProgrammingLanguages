#lang typed/racket

(require typed/rackunit)

;;not passing while/in-order test case

(define-type ExprC (U numC stringC ifC idC fnC appC lamC assignC))
(struct numC ([n : Real]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])  #:transparent)
(struct idC ([s : Symbol])                                      #:transparent)
(struct fnC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [l : (Listof ExprC)]) #:transparent)
(struct lamC ([arg : (Listof Symbol)] [body : ExprC] [t : (Listof Type)]) #:transparent)
(struct assignC ([id : Symbol] [body : ExprC]) #:transparent)

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
(define-type Env (Listof Bind))


(define-type Store (Mutable-HashTable Integer Value))
(define keywords (list 'let 'in 'if 'fn ':=))
#|; while
(define while '{let {while = "bogus"}
                 in {begin
                      {while := {fn {guard body} {if {guard} {begin {body} {while guard body}} null}}}
                 while}})


; in-order
(define in-order '{let {in-order = "bogus"} in
                    {begin
                      {in-order :=
                                {fn {arr len}
                                    {let {i = 0} {valid = true} in
                                      {begin {while {fn {} {<= i (- len 2)}} {fn {} {if
                                                                                     (<= {aref arr (+ i 1)}
                                                                                         {aref arr i})
                                                                                     (begin {i := (+ i 1)}
                                                                                            {valid := false})
                                                                                     (i := (+ i 1))}}}
                                             valid}}}}
                    in-order}})





;; interperets sexp into String
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


;; returns value of lookup-env
(define (lookup-env [sym : Symbol] [env : Env] [st : Store]) : Value
  (define loc (lookup sym env))
  (hash-ref st loc))

;;take a symbol and environment, return value
(define (lookup [for : Symbol] [env : Env]) : Integer
  (cond
    [(empty? env) (error "Name not found DXUQ")]
    [else (cond
            [(equal? for (Bind-name (first env))) (Bind-loc (first env))]
            [else (lookup for (rest env))])]))

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

;;interprets equal and returns boolean
(: interp-equal (-> (Listof Value) boolV))
(define (interp-equal args)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for equal? DXUQ")]
    [(or (closV? (first args)) (closV? (second args))
         (primV? (first args)) (primV? (second args))) (boolV #f)]
    [else (boolV (equal? (first args) (second args)))]))


;;interprets errors
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


;;takes a value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (~v n)]
    [(strV s) (~v s)]
    [(nullV) ""]
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
(check-equal? (serialize (closV '(a) (appC (numC 0) (list (numC 1))) '())) "#<procedure>")


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
                                  [(list (arrayV loc len) (numV (? natural? n)))
                                   (cond
                                       [(< n len) (hash-ref top-store (+ n loc))]
                                       [else (error "Index out of bounds DXUQ")])]
                                  [other (error "Invalid operands for aref DXUQ")])]
    [else (error "Invalid number of operands for aref DXUQ")]))

; sets a value in a given array to a new value
(: interp-aset! (-> (Listof Value) Value))
(define (interp-aset! args)
  (cond
    [(equal? (length args) 3) (match args
                                [(list (arrayV loc len) (numV (? natural? n)) new)
                                 (cond
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
       [(and (< n1 n2) (>= n1 0) (<= n2 (string-length str))) (strV (substring str n1 n2))]
       [else (error "Invalid substring operation DXUQ")])]
    [other (error "Invalid substring operation DXUQ")]))
|#

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
    [(list 'let (list ty sym '= sym2) ... 'in body) (parse-let2 ty sym sym2 body)]
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))]
    [(list 'fn (list (list (? symbol? syms) (? symbol? syms2)) ...) body) (cond
                                                   [(equal? (check-duplicates syms2) #f)
                                                    (lamC (cast syms2 (Listof Symbol))
                                                          (parse body)
                                                          (map (lambda (x) (parse-type x))
                                                               (cast syms (Listof Sexp))))]
                                                   [else (error "invalid format DXUQ")])]
    
    [(list expr expr2 ...) (cond
                       [(empty? expr) (error "invalid format DXUQ")]
                       [else (appC (parse expr) (map (lambda (a) (parse a)) expr2))])]
    [else (error "invalid format DXUQ test")]))


(define (parse-let2 [t : (Listof Any)] [l : (Listof Any)] [l2 : (Listof Any)] [body : Sexp]) : ExprC
  (define ids (cast l (Listof Symbol)))
  ;(define types (map (lambda ([x : Symbol]) (parse-type x)) (cast types (Listof Symbol))))
  (define types (cast t (Listof Symbol)))
  ;(define vals (cast l2 (Listof Symbol)))
  (parse (cast (cons (list 'fn (let-helper types ids) body) l2) Sexp)))

(define (let-helper [t : (Listof Symbol)] [var : (Listof Symbol)]) : Sexp
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

;;define the top environment 
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
                        (Bind 'substring 15)
                        (Bind 'null 16)))

;;define the top store
#|(define top-store (ann (make-hash
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
                              (cons 15 (primV interp-substring))
                              (cons 16 (nullV))))
                       Store))


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