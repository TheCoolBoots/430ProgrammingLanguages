#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC assignC Value))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([body : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct assignC ([id : Symbol] [body : ExprC]))

(define-type Store (Mutable-HashTable Integer Value))

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)

(define-type Value (U numV strV primV boolV cloV arrayV nullV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Store Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([body : ExprC] [args : (Listof Symbol)] [clo-env : Env]) #:transparent)
(struct arrayV ([startIndex : Integer] [length : Integer]) #:transparent)
(struct nullV ([val : Integer]))

(define reserved '(:= if let = in fn))

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
       [(primV func) (func (map (lambda ([arg : ExprC]) (interp arg env sto)) args) sto)]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(lamC ids body) (cloV body ids env)]
    [(assignC id body) (interp-assignC exp env sto)]
    [(numV val) exp]
    [(strV val) exp]
    [(boolV val) exp]))


; returns extended environment including given symbols/ExprC's
(: extend-env (-> (Listof Symbol) (Listof Value) Env Store Env))
(define (extend-env symbols args env sto)
  (cond
    [(xor (empty? symbols) (empty? args)) (error "Different numbers of ids and args DXUQ")]
    [(empty? symbols) env]
    [else (define nextIndex (get-next-index sto))
          (hash-set sto nextIndex (first args))
          (cons (Bind (first symbols) nextIndex) (extend-env (rest symbols) (rest args) env sto))]))


; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Store Value))
(define (lookup-env sym env sto)
  (define loc (lookup sym env))
  (hash-ref sto loc))


; interprets a assignC into a value
(: interp-assignC (-> assignC Env Store Value))
(define (interp-assignC expr env sto)
  (define nextOpenLocation (cond
                              [(in-env? (assignC-id expr) env) (lookup (assignC-id expr) env)]
                              [else (get-next-index sto)]))
  (define newEnv (cons (Bind (assignC-id expr) nextOpenLocation) env))
  (define interpretedBody (interp (assignC-body expr) newEnv sto))
  (hash-set sto nextOpenLocation interpretedBody)
  (nullV 0))


; gets the location associated with given id in Env
(: lookup (-> Symbol Env Integer))
(define (lookup sym env)
  (cond
    [(empty? env) (error "Symbol not found in Environment DXUQ")]
    [(equal? (Bind-name (first env)) sym) (Bind-loc (first env))]
    [else (lookup sym (rest env))]))
    

; returns #t if symbol exists in Env, #f otherwise
(: in-env? (-> Symbol Env Boolean))
(define (in-env? sym env)
  (cond
    [(empty? env) #f]
    [(equal? (Bind-name (first env)) sym) #t]
    [else (in-env? sym (rest env))]))


; interprets a DXUQ if statement and returns a Value
(: interp-cond (-> ExprC ExprC ExprC Env Store Value))
(define (interp-cond if then else env sto)
  (match (interp if env sto)
    [(boolV val) (cond
                   [val (interp then env sto)]
                   [else (interp else env sto)])]
    [other (error "Invalid operands for DXUQ if")]))


; interprets user-defined error
(: interp-error (-> (Listof Value) Store numV))
(define (interp-error args sto)
  (cond
    [(not (equal? (length args) 1)) (error "Invalid number of arguments for error DXUQ")]
    [else (error (string-append "User Error DXUQ: " (serialize (first args))))]))


; interprets equal?
(: interp-equal (-> (Listof Value) Store boolV))
(define (interp-equal args sto)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for equal? DXUQ")]
    [(or (cloV? (first args)) (cloV? (second args))
         (primV? (first args)) (primV? (second args))) (boolV #f)]
    [else (boolV (equal? (first args) (second args)))]))


; interprets addition primitive
(: interp-add (-> (Listof Value) Store numV))
(define (interp-add args sto)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for + DXUQ")]
    [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (numV (+ n m))]
                        [other (error "Invalid operands for + DXUQ")])]
            [other (error "Invalid operands for + DXUQ")])]))


; interprets subtraction primitive
(: interp-sub (-> (Listof Value) Store numV))
(define (interp-sub args sto)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for - DXUQ")]
      [else (match (first args)
              [(numV n) (match (second args)
                          [(numV m) (numV (- n m))]
                          [other (error "Invalid operands for - DXUQ")])]
              [other (error "Invalid operands for - DXUQ")])]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) Store numV))
(define (interp-mult args sto)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for * DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (numV (* n m))]
                        [other (error "Invalid operands for * DXUQ")])]
            [other (error "Invalid operands for * DXUQ")])]))


; interprets division primitive
(: interp-div (-> (Listof Value) Store numV))
(define (interp-div args sto)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for / DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV (? natural? m)) #:when (> m 0) (numV (/ n m))]
                        [other (error "Invalid operands for / DXUQ")])]
            [other (error "Invalid operands for / DXUQ")])]))


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) Store boolV))
(define (interp-leq args sto)
  (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for <= DXUQ")]
      [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (boolV (<= n m))]
                        [other (error "Invalid operands for <= DXUQ")])]
            [other (error "Invalid operands for <= DXUQ")])]))


; interprets an array into a Value
(: interp-array (-> (Listof Value) Store arrayV))
(define (interp-array values sto)
  (begin
    (define arrayStartIndex (get-next-index sto))
    (map (lambda ([v : Value]) (hash-set! sto (get-next-index sto) v)) values)
    (arrayV arrayStartIndex (length values))))


; creates and returns a new array with given parameters
(: interp-new-array (-> (Listof Value) Store arrayV))
(define (interp-new-array args sto)
  (cond
    [(equal? (length args) 2) (match args
                                [(list (numV (? natural? n)) val) (begin (define arrayStartIndex (get-next-index sto))
                                                                         (init-array n val sto)
                                                                         (arrayV arrayStartIndex n))]
                                [other (error "Invalid operands for new-array DXUQ")])]
    [else (error "Invalid number of arguments for new-array DXUQ")]))


; allocates given number of store locations and assigns given value to them
(: init-array (-> Natural Value Store 1))
(define (init-array num val sto)
  (cond
    [(equal? num 0) 1]
    [else (hash-set! sto (get-next-index sto) val) (init-array (- num 1) val sto)]))


; returns the value of the given array at index i
(: interp-aref (-> (Listof Value) Store Value))
(define (interp-aref args sto)
  (cond
    [(equal? (length args) 2) (error "Invalid number of arguments for aref DXUQ4")]
    [else (match args
            [(list (arrayV loc length) (numV (? natural? n))) #:when (< n length) (hash-ref sto (+ loc n))]
            [other (error "Invalid operands for aref DXUQ")])]))


; returns the value of the given array at index i
(: interp-aset! (-> (Listof Value) Store Value))
(define (interp-aset! args sto)
  (cond
    [(equal? (length args) 3) (error "Invalid number of arguments for aref DXUQ4")]
    [else (match args
            [(list (arrayV loc length) (numV (? natural? n))) #:when (< n length)
                                                          (hash-set! sto (+ loc n) (third args))
                                                          (nullV 0)]
            [other (error "Invalid operands for aref DXUQ")])]))


; returns the value of the last interpreted argument
(: interp-begin (-> (Listof Value) Store Value))
(define (interp-begin args sto)
  (first (reverse args)))


; returns str[begin:end] excluding char @ index end
(: interp-substring (-> (Listof Value) Store Value))
(define (interp-substring args sto)
  (match args
    [(list (strV str) (numV (? natural? n)) (numV (? natural? m)))
           #:when (and (>= n 0) (< n m) (< m (string-length str))) (strV (substring str n m))]
    [other (error "Invalid operands for aref DXUQ")]))


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
    [(cloV body ids env) "#<procedure>"]
    [(arrayV first rest) "#<array>"]))


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
    [(list 'when cond sec) (condC (parse cond) (strV "null") (parse sec))]
    [(list expr args ...) (appC (parse expr) (map (lambda (arg) (parse arg)) args))]
    [other (error "Invalid format DXUQ")]))


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


; returns the next available index in the given store
(: get-next-index (-> Store Integer))
(define (get-next-index sto)
  (hash-count sto))


(define top-env (list (Bind '+ 0)
                      (Bind '- 1)
                      (Bind '* 2)
                      (Bind '/ 3)
                      (Bind '<= 4)
                      (Bind 'equal? 5)
                      (Bind 'true 6)
                      (Bind 'false 7)
                      (Bind 'error 8)
                      (Bind 'array 9)
                      (Bind 'new-array 10)
                      (Bind 'aref 11)
                      (Bind 'aset! 12)
                      (Bind 'begin 13)
                      (Bind 'substring 14)))
(define top-store
  (ann (make-hash
        (list (cons 0 (primV interp-add))
              (cons 1 (primV interp-sub))
              (cons 2 (primV interp-mult))
              (cons 3 (primV interp-div))
              (cons 4 (primV interp-leq))
              (cons 5 (primV interp-equal))
              (cons 6 (boolV #t))
              (cons 7 (boolV #f))
              (cons 8 (primV interp-error))
              (cons 9 (primV interp-array))
              (cons 10 (primV interp-new-array))
              (cons 11 (primV interp-aref))
              (cons 12 (primV interp-aset!))
              (cons 13 (primV interp-begin))
              (cons 14 (primV interp-substring))))
       Store))

; tests for +
(check-equal? (interp (appC (idC '+) (list (numV 1) (numV 2))) top-env top-store) (numV 3))
(check-exn (regexp (regexp-quote "Invalid number of arguments for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (numV 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (boolV #t) (numV 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for + DXUQ"))
           (lambda () {interp (appC (idC '+) (list (numV 1) (boolV #t))) top-env top-store}))

; tests for -
(check-equal? (interp (appC (idC '-) (list (numV 1) (numV 2))) top-env top-store) (numV -1))
(check-exn (regexp (regexp-quote "Invalid number of arguments for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (numV 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (boolV #t) (numV 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for - DXUQ"))
           (lambda () {interp (appC (idC '-) (list (numV 1) (boolV #t))) top-env top-store}))

; tests for *
(check-equal? (interp (appC (idC '*) (list (numV 1) (numV 2))) top-env top-store) (numV 2))
(check-exn (regexp (regexp-quote "Invalid number of arguments for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (numV 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (boolV #t) (numV 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for * DXUQ"))
           (lambda () {interp (appC (idC '*) (list (numV 1) (boolV #t))) top-env top-store}))

; tests for /
(check-equal? (interp (appC (idC '/) (list (numV 2) (numV 2))) top-env top-store) (numV 1))
(check-exn (regexp (regexp-quote "Invalid number of arguments for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (numV 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (boolV #t) (numV 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (numV 1) (boolV #t))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for / DXUQ"))
           (lambda () {interp (appC (idC '/) (list (numV 1) (numV 0))) top-env top-store}))

; tests for <=
(check-equal? (interp (appC (idC '<=) (list (numV 1) (numV 2))) top-env top-store) (boolV #t))
(check-exn (regexp (regexp-quote "Invalid number of arguments for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (numV 1))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (boolV #t) (numV 2))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid operands for <= DXUQ"))
           (lambda () {interp (appC (idC '<=) (list (numV 1) (boolV #t))) top-env top-store}))

; tests for equal?
(check-equal? (interp (appC (idC 'equal?) (list (numV 1) (numV 2))) top-env top-store) (boolV #f))
(check-exn (regexp (regexp-quote "Invalid number of arguments for equal? DXUQ"))
           (lambda () {interp (appC (idC 'equal?) (list (numV 1))) top-env top-store}))

; tests for error
(check-exn (regexp (regexp-quote "User Error DXUQ: hi"))
           (lambda () {interp (appC (idC 'error) (list (strV "hi"))) top-env top-store}))
(check-exn (regexp (regexp-quote "Invalid number of arguments for error DXUQ"))
           (lambda () {interp (appC (idC 'error) (list (strV "hi") (strV "bye"))) top-env top-store}))

; '{begin {c := 5} {c := {+ c 5}}}

