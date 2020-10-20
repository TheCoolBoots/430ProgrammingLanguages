#lang typed/racket

(require typed/rackunit)


; NEED TO DO COMPLETION STATEMENT

; definitions for ExprC types
(define-type ExprC (U IdC AppC CondC LamC Value))
(struct IdC ([s : Symbol]) #:transparent)
(struct AppC ([target : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct CondC ([if : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([ids : (Listof IdC)] [body : ExprC]) #:transparent)

(define-type Value (U NumV StrV PrimV BoolV CloV))
(struct NumV  ([val : Real]) #:transparent)
(struct StrV  ([val : String]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct BoolV ([val : Boolean]) #:transparent)
(struct CloV  ([target : ExprC] [args : (Listof ExprC)] [clo-env : Env]) #:transparent)

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value]) #:transparent)
(define top-env (list (Bind 'true (BoolV #t))
                      (Bind 'false (BoolV #f))
                      (Bind '+ (PrimV '+))
                      (Bind '- (PrimV '-))
                      (Bind '* (PrimV '*))
                      (Bind '/ (PrimV '/))
                      (Bind '<= (PrimV '<=))
                      (Bind 'equal? (PrimV 'equal?))
                      (Bind 'error (PrimV 'error))))

(define primitives (list '+ '- '* '/ '<= 'equal? 'true 'false))
(define keywords (list 'let 'in 'if 'fn))


;; returns a string that is the result of a DXUQ4 statement
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))

;; interprets a DXUQ4 statement and returns a Value
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumV n) exp]
    [(StrV n) exp]
    [(BoolV n) exp]
    [(IdC n) (interp (lookup n env) env)]
    ; need to do clov
    ; need to do appc
    ; need to do condc
    [(LamC ids expr) (CloV expr ids env)]
    [other (error "Invalid DXUQ4 statement")]
    ))

;; searches for a binding between a symbol and value in the env
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error "DXUQ4 env binding not found")]
    [(equal? for (Bind-name (first env))) (Bind-val (first env))]
    [else (lookup for (rest env))]))

;; takes in an expression and returns an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumV n)]
    [(? string? n) (StrV n)]
    [(? symbol? n) (DXUQ-keyword? n keywords)]
    [(list 'fn (list ids ...) expr) (LamC (map (lambda ([a : Symbol]) (DXUQ-keyword? a keywords)) (cast ids (Listof Symbol))) (parse expr))]
    [(list 'if cond then else) (CondC (parse cond) (parse then) (parse else))]
    [(list 'let mappings ... 'in expr) (parse-let mappings expr)]
    [(list expr exprs ...) (AppC (parse expr) (map (lambda (a) (parse a)) exprs))]
    [other (error "Inavlid DXUQ4 statement")]
    ))

;; helps to parse a let statement
(define (parse-let [mappings : (Listof Any)] [expr : Sexp]) : ExprC
  (define ids (map (lambda (id) (match id
                                  [(list (? symbol? s) '= expr) s]
                                  [else (error "DXUQ4 Invalid let statement")])) mappings))
  (define args (map (lambda (arg) (match arg
                                  [(list (? symbol? s) '= expr) expr])) mappings))
  (parse (cast (cons (list 'fn ids expr) args) Sexp)))

; checks to see if a symbol is part of a list of symbols
; returns true if symbol exists in list, false otherwise
(define (DXUQ-keyword? [target : Symbol] [keywords : (Listof Symbol)]) : IdC
  (cond
    [(empty? keywords) (IdC target)]
    [(eq? (first keywords) target) (error "Invalid Id DXUQ4")]
    [else (DXUQ-keyword? target (rest keywords))]))

;; accepts a DXUQ4 value and returns a string
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (~v n)]
    [(StrV n) (~v n)]
    [(BoolV n)
     (cond
       [n "true"]
       [else "false"])]
    [(CloV target args env) "#<procedure>"]
    [(PrimV op) "#<primop>"]))

; top-interp test cases
(check-equal? (top-interp 11) "11")
(check-equal? (top-interp "david") "\"david\"")

; interp test cases
(check-equal? (interp (NumV 12) (list (Bind 's (NumV 11)))) (NumV 12))
(check-equal? (interp (StrV "david") (list (Bind 's (NumV 12)))) (StrV "david"))
(check-equal? (interp (BoolV #f) '()) (BoolV #f))
(check-equal? (interp (IdC 's) (list (Bind 's (NumV 123)))) (NumV 123))

; lookup test cases
(check-equal? (lookup 's (list (Bind 's (NumV 13)))) (NumV 13))
(check-equal? (lookup 'd (list (Bind 's (BoolV #t)) (Bind 'd (BoolV #f)))) (BoolV #f))

; parse test cases
(check-equal? (parse 12) (NumV 12))
(check-equal? (parse "dave") (StrV "dave"))
(check-equal? (parse 's) (IdC 's))
(check-equal? (parse '{fn {a b} 23}) (LamC (list (IdC 'a) (IdC 'b)) (NumV 23)))
(check-equal? (parse '{if 1 2 3}) (CondC (NumV 1) (NumV 2) (NumV 3)))
(check-equal? (parse '{let {z = {+ 9 14}} {y = 98} in {+ z y}}) (parse '{{fn {z y} {+ z y}} {+ 9 14} 98}))

; serialize test cases
(check-equal? (serialize (NumV 23)) "23")
(check-equal? (serialize (StrV "David")) "\"David\"")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (CloV (LamC '() (NumV 12)) '() '())) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")