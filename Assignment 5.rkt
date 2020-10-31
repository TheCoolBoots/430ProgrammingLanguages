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

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)

(define-type Value (U numV strV primV boolV cloV))
(struct numV  ([val : Real]) #:transparent)
(struct strV  ([val : String]) #:transparent)
(struct primV ([op : Symbol]) #:transparent)
(struct boolV ([val : Boolean]) #:transparent)
(struct cloV  ([body : ExprC] [args : (Listof Symbol)] [clo-env : Env]) #:transparent)

(define top-env (list (Bind '+ 1)
                      (Bind '- 2)
                      (Bind '* 3)
                      (Bind '/ 4)
                      (Bind '<= 5)
                      (Bind 'equal? 6)
                      (Bind 'true 7)
                      (Bind 'false 8)
                      (Bind 'error 9)))

(define-type Store (Mutable-HashTable Real Value))
(define top-store
  (ann (make-hash
        (list (cons 1 (primV '+))
              (cons 2 (primV '-))
              (cons 3 (primV '*))
              (cons 4 (primV '/))
              (cons 5 (primV '<=))
              (cons 6 (primV 'equal?))
              (cons 7 (boolV #t))
              (cons 8 (boolV #f))
              (cons 9 (primV 'error))))
       Store))

(define reserved '(fn let if =))


; interprets a DXUQ expression into a Value
(: interp (-> ExprC Env Store Value))
(define (interp exp env sto)
  (match exp
    [(idC sym) (lookup-env sym env sto)]
    ;[(condC if then else) (interp-cond if then else env)]
    [(appC body params)
     (define interpretedBody (interp body env sto))
     (match interpretedBody
       ;[(cloV clo-body ids clo-env)
       ; (define interpretedParams (map (lambda ([param : ExprC]) (interp param env)) params))
       ; (define new-env (extend-env ids interpretedParams clo-env))
       ;     for each pair of id & param
                       ; if env[id] != null
                       ;      update store location to param
                       ; else
                       ;      get next open store location
                       ;      add location to env
                       ;      add param @ loc in store
                       ; if id not found
                       ;      get next open store location loc
                       ;      add id with loc to Env
                       ;      add param @ loc in store
       ; (interp clo-body new-env)]
       [(primV symbol) (interp-primV symbol (map (lambda ([param : ExprC]) (interp param env sto)) params))]
       [other (error "Applied arguments to non-function DXUQ")])]
    [(lamC ids body) (cloV body ids env)]
    [(assignC id body) (numV 0)
                       ; if env[id] != null
                       ;      update store location to body
                       ; else
                       ;      get next open store location
                       ;      add location to env
                       ;      add enterpreted body @ loc in store
                       ; if id not found
                       ;      get next open store location loc
                       ;      add id with loc to Env
                       ;      add interpreted body @ loc in store
    ]
    [(numV val) exp]
    [(strV val) exp]
    [(boolV val) exp]))


; interprets a primV into a value
(: interp-primV (-> Symbol (Listof Value) Value))
(define (interp-primV primSymbol params)
  (match primSymbol
    ['+ (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for + DXUQ")]
          [else (interp-add params)])]
    ['- (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for - DXUQ")]
          [else (interp-sub params)])]
    ['* (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for * DXUQ")]
          [else (interp-mult params)])]
    ['/ (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for / DXUQ")]
          [else (interp-div params)])]
    ['<= (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for <= DXUQ")]
          [else (interp-leq params)])]
    ['equal? (cond
          [(not (equal? (length params) 2)) (error "Invalid number of arguments for equal? DXUQ")]
          [(or (cloV? (first params)) (cloV? (second params))
               (primV? (first params)) (primV? (second params))) (boolV #f)]
          [else (boolV (equal? (first params) (second params)))])] ))
    ;['error (cond
    ;      [(not (equal? (length params) 1)) (error "Invalid number of arguments for equal? DXUQ")]
    ;      [else (error (string-append "User Error DXUQ: " (serialize (first params))))])]))


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


; interprets <= exprC exprC to a boolean
(: interp-leq (-> (Listof Value) boolV))
(define (interp-leq args)
  (cond
    [(and (numV? (first args)) (numV? (second args)))
     (boolV (<= (numV-val (cast (first args) numV)) (numV-val (cast (second args) numV)))) ]
    [else (error "Invalid operands for DXUQ <=")]))


; looks up a symbol in an environment then returns the value associated with the symbol
(: lookup-env (-> Symbol Env Store Value))
(define (lookup-env sym env sto)
  (cond
    [(empty? env) (error "Environment binding not found DXUQ")]
    [(equal? (Bind-name (first env)) sym) (hash-ref sto (Bind-loc (first env)))]
    [else (lookup-env sym (rest env) sto)]))


; returns the next available index in the given store
(: get-next-index (-> Store Integer))
(define (get-next-index sto)
  (length (hash-keys sto)))


(check-equal? (interp-primV '+ (list (numV 2) (numV 3))) (numV 5))
(check-equal? (interp-primV '- (list (numV 2) (numV 3))) (numV -1))
(check-equal? (interp-primV '* (list (numV 2) (numV 3))) (numV 6))
(check-equal? (interp-primV '/ (list (numV 8) (numV 2))) (numV 4))
(check-equal? (interp (appC (idC '+) (list (numV 1) (numV 2))) top-env top-store) (numV 3))

; '{begin {c := 5} {c := {+ c 5}}}

