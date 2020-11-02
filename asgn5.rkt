#lang typed/racket

(require typed/rackunit)

; need to finish mutation test cases

; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC asgnC Value))
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([body : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([ids : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct asgnC ([id : Symbol] [body : ExprC]) #:transparent)

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)

(define-type Value (U numV strV primV boolV cloV arrayV nullV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : (-> (Listof Value) Value)]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([body : ExprC] [args : (Listof Symbol)] [clo-env : Env]) #:transparent)
(struct arrayV ([loc : Integer] [length : Integer]) #:transparent)
(struct nullV ())

(define reserved '(:= if let = in fn))

; interprets a DXUQ expression into a Value
(: interp (-> ExprC Env Store Value))
(define (interp exp env sto)
  (match exp
    [(idC sym) (lookup-env sym env sto)]
    [(condC if then else) (interp-cond if then else env sto)]
    [(appC body params)
     (define interpretedBody (interp body env sto))
     (match interpretedBody
       [(cloV clo-body ids clo-env)
        (define interpretedParams (map (lambda ([param : ExprC]) (interp param env sto)) params))
        (define new-env (extend-env ids interpretedParams clo-env sto))
        (interp clo-body new-env sto)]
       [(primV symbol) (symbol (map (lambda ([param : ExprC]) (interp param env sto)) params))]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(lamC ids body) (cloV body ids env)]
    [(asgnC id body) (interp-asgnC exp env sto)]
    [(numV val) exp]
    [(strV val) exp]
    [(boolV val) exp]
    ))

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
  (hash-set! sto (get-next-index sto) (first args))
  (get-next-index sto))

; returns the next available index in the given store
(: get-next-index (-> Store Integer))
(define (get-next-index sto)
  (hash-count sto))

; interprets a mutation operator
(: interp-asgnC (-> asgnC Env Store Value))
(define (interp-asgnC expr env sto)
  (define index (cond
                  [(in-env? (asgnC-id expr) env) (lookup (asgnC-id expr) env)]
                  [else (get-next-index sto)]))
  (define new-env (cons (Bind (asgnC-id expr) index) env))
  (define interp-body (interp (asgnC-body expr) new-env sto))
  (hash-set! sto index interp-body)
  (nullV))

; returns t/f if an id exists in the env
(: in-env? (-> Symbol Env Boolean))
(define (in-env? sym env)
  (cond
    [(empty? env) #f]
    [(equal? sym (Bind-name (first env))) #t]
    [else (in-env? sym (rest env))]))

; returns the index of an element in the env
(: lookup (-> Symbol Env Integer))
(define (lookup sym env)
  (cond
    [(empty? env) (error "Symbol not in Env DXUQ")]
    [(equal? sym (Bind-name (first env))) (Bind-loc (first env))]
    [else (lookup sym (rest env))]))

; interprets addition primitive
(: interp-add (-> (Listof Value) Value))
(define (interp-add args)
  (cond
    [(not (equal? (length args) 2)) (error "Invalid number of arguments for + DXUQ")]
    [else (match (first args)
            [(numV n) (match (second args)
                        [(numV m) (numV (+ n m))]
                        [other (error "Invalid operands for + DXUQ")])]
            [other (error "Invalid operands for + DXUQ")])]))


; interprets subtraction primitive
(: interp-sub (-> (Listof Value) Value))
(define (interp-sub args)
    (cond
      [(not (equal? (length args) 2)) (error "Invalid number of arguments for - DXUQ")]
      [else (match (first args)
              [(numV n) (match (second args)
                          [(numV m) (numV (- n m))]
                          [other (error "Invalid operands for - DXUQ")])]
              [other (error "Invalid operands for - DXUQ")])]))


; interprets multiplication primitive
(: interp-mult (-> (Listof Value) Value))
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

; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Store Value))
(define (lookup-env sym env sto)
  (cond
    [(empty? env) (error "Environment binding not found DXUQ")]
    [(equal? (Bind-name (first env)) sym) (hash-ref sto (Bind-loc (first env)))]
    [else (lookup-env sym (rest env) sto)]))         

; interprets a DXUQ if statement and returns a Value
(: interp-cond (-> ExprC ExprC ExprC Env Store Value))
(define (interp-cond if then else env sto)
  (match (interp if env sto)
    [(boolV val) (cond
                   [val (interp then env sto)]
                   [else (interp else env sto)])]
    [other (error "Invalid operands for DXUQ if")]))

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
                                  [(< n len) (hash-set! top-store (+ n loc) new) (nullV)]
                                  [else (error "Index out of bounds DXUQ")])]
                                [other (error "Invalid operands for aset! DXUQ")])]
    [else (error "Invalid number of operands for aset! DXUQ")]))
                                                                                   
; interprets the begin operation
(: interp-begin (-> (Listof Value) Value))
(define (interp-begin args)
  (cond
    [(equal? (length args) 1) (first args)]
    [else (first args) (interp-begin (rest args))]))

; interprets the substring operation
(: interp-substring (-> (Listof Value) Value))
(define (interp-substring args)
  (match args
    [(list (strV str) (numV (? natural? n1)) (numV (? natural? n2)))
     (cond
       [(and (< n1 n2) (>= n1 0) (< n2 (string-length str))) (strV (substring str n1 n2))]
       [else (error "Invalid substring operation DXUQ")])]
    [other (error "Invalid substring operation DXUQ")]))


(check-equal? (interp-substring (list (strV "david") (numV 1) (numV 3))) (strV "av"))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () {interp-substring (list (strV "d") (strV "d"))}))
(check-exn (regexp (regexp-quote "Invalid substring operation DXUQ"))
           (lambda () {interp-substring (list (strV "d") (numV 4) (numV 3))}))

; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? n) (numV n)]
    [(? string? s) (strV s)]
    [(? symbol? s) #:when (validID? s) (idC s)]
    [(list (? symbol? sym) ':= body) (asgnC sym (parse body))]
    [(list 'fn (list ids ...) expr) #:when
                                    (and (equal? (remove-duplicates ids) ids) (andmap (lambda (a) (symbol? a)) ids))
                                    (lamC (cast ids (Listof Symbol)) (parse expr))]
    [(list 'if exprIf exprThen exprElse) (condC (parse exprIf) (parse exprThen) (parse exprElse))]
    [(list 'let mappings ... 'in body) (parse-let mappings body)]
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
    [(arrayV first rest) "#<array>"]
    [(nullV) ""]))


; interprets a DXUQ program into a string
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env top-store)))


(define top-env (list (Bind 'true 1)
                      (Bind 'false 2)
                      (Bind '+ 3)
                      (Bind '- 4)
                      (Bind '* 5)
                      (Bind '/ 6)
                      (Bind '<= 7)
                      (Bind 'equal? 8)
                      (Bind 'error 9)
                      (Bind 'new-array 10)
                      (Bind 'array 11)
                      (Bind 'aref 12)
                      (Bind 'aset! 13)
                      (Bind 'begin 14)
                      (Bind 'substring 15)))

(define-type Store (Mutable-HashTable Integer Value))
(define top-store
  (ann (make-hash
        (list (cons 0 (numV 9))
              (cons 1 (boolV #t))
              (cons 2 (boolV #f))
              (cons 3 (primV interp-add))
              (cons 4 (primV interp-sub))
              (cons 5 (primV interp-mult))
              (cons 6 (primV interp-div))
              (cons 7 (primV interp-leq))
              (cons 8 (primV interp-equal))
              (cons 9 (primV interp-error))
              (cons 10 (primV interp-new-array))
              (cons 11 (primV interp-array))
              (cons 12 (primV interp-aref))
              (cons 13 (primV interp-aset!))
              (cons 14 (primV interp-begin))
              (cons 15 (primV interp-substring))))
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

(check-equal? (lookup 's (list (Bind 'a 1) (Bind 'b 2) (Bind 's 3))) 3)
(check-exn (regexp (regexp-quote "Symbol not in Env DXUQ"))
           (lambda () {lookup 's (list)}))
(check-equal? (interp-equal (list (cloV (idC 's) '() '()) (cloV (idC 'a) '() '()))) (boolV #f))
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
