#lang typed/racket

(require typed/rackunit)


; passes all handin test cases


; definitions for ExprC types
(define-type ExprC (U idC appC condC lamC Value))
(struct idC ([s : Symbol]))
(struct appC ([target : ExprC] [args : (Listof ExprC)]))
(struct condC ([if : ExprC] [then : ExprC] [else : ExprC]))
(struct lamC ([ids : (Listof Symbol)] [body : ExprC]))

(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Real]))

(define-type Value (U numV strV primV boolV cloV))
(struct numV ([val : Real]))
(struct strV ([val : String]))
(struct primV ([op : Symbol] [args : (Listof Any)]))
(struct boolV ([val : Boolean]))
(struct cloV ([target : ExprC] [args : (Listof ExprC)] [clo-env : Env]))


; interprets a DXUQ4 expression into a series of Values
(: interp (-> ExprC Env Real))
(define (interp exp env)
  (match exp
    [(idC sym) 0]                ; look up symbol in environment, return binding if symbol exists in environment
    [(appC function params) 0]   ; return a cloV with env extended to include mappings from lamC ids to params
    [(condC if then else) 0]     ; if(if), then return then, else return else
    [(lamC ids body) 0]          ; not sure what to do here
    ;[(Value contents ...) 0]   ; pass onto helper function to interpret primitives, closures, and data types
    [other (error "invalid DXUQ4")]))

; takes in s-expr and parses to create ExprC
(: parse (-> Sexp ExprC))
(define (parse s)
  (match s
    [(? real? n) (numV n)]
    [(? string? s) (strV s)]
    [(? boolean? b) (boolV b)]
    [(list (? symbol? s) args ...) #:when (equal? 1 1) (primV s args)]  ; change when equal to when symbol is a primitive
    ; let: desugar into function
    [(list 'fn (list ids ...) expr) (lamC (cast ids (Listof Symbol)) (parse expr))]
    [(list 'if exprIf exprThen exprElse) (condC (parse exprIf) (parse exprThen) (parse exprElse))]
    ;[(list expr args ...) (appC (parse expr) (parse args))] ; change parse args to parse each arg and get a list of ExprC
    [(? symbol? s) (idC s)]
    [other (error "invalid format DXUQ")]))

#|

;;top-interp test cases
(check-equal? (top-interp '{{fundef {f x} {+ x 14}}
                     {fundef {main} {f 2}}}) 16)
(check-equal? (top-interp '{{fundef {f x} {+ 5 14}}
                     {fundef {main} {f 2}}}) 19)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* z 2}}
                            {fundef {main} {f2 3}}}) 6)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* {- z 1} 2}}
                            {fundef {main} {f2 3}}}) 4)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f2 z} {* {- z 1} {f 1}}}
                            {fundef {main} {f2 3}}}) 12)
(check-equal? (top-interp '{{fundef {f x} {+ x 5}}
                            {fundef {f1 l} {/ 2 2}}
                            {fundef {f0 l} 5}
                            {fundef {f2 z} {* {- z 1} {f 1}}}
                            {fundef {main} {f0 3}}}) 5)



;;interp test cases
(check-equal? (interp (numC 1) (parse-prog '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (binopC '* (numC 2) (numC 3)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 6)
(check-equal? (interp (ifleq0C (numC 0) (numC 1) (numC 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 1)
(check-equal? (interp (ifleq0C (numC 1) (numC 1) (numC 2)) (parse-prog  '{{fundef {f x} {+ x 14}}})) 2)
(check-equal? (interp (appC 'f (list (numC 3))) (parse-prog  '{{fundef {f x} {+ x 14}}})) 17)
(check-equal? (interp (appC 'f (list (binopC '+ (numC 1) (numC 1)))) (parse-prog  '{{fundef {f x} {/ x 1}}})) 2)
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (interp (idC 'a) (parse-prog '{{fundef {f x} {+ x 14}}}))))



;;interp-fns test cases
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main} {f 2}}})) 16)
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main} {+ 1 2}}})) 3)
(check-equal? (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}
                     {fundef {main} {+ 1 2}}})) 3)
(check-exn (regexp (regexp-quote "main function not found DXUQ"))
           (lambda () (interp-fns (parse-prog '{{fundef {f x} {+ x 14}}}))))

(check-equal? (interp-fns
       (parse-prog '{{fundef {f x y} {+ x y}}
                     {fundef {main} {f 1 2}}}))
      3)
(check-equal? (interp-fns
        (parse-prog '{{fundef {f} 5}
                      {fundef {main} {+ {f} {f}}}}))
       10)
(check-exn (regexp (regexp-quote "Number of parameters does not match number of arguments DXUQ"))
           (lambda () (top-interp '{{fundef {f x y} {+ x y}}
                             {fundef {main} {f 1}}} )))


;;parse test cases
(check-equal? (parse 1) (numC 1))
(check-equal? (parse '(+ 1 2)) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parse '(- 1 2)) (binopC '- (numC 1) (numC 2)))
(check-equal? (parse '(+ (+ 1 2) 3)) (binopC '+ (binopC '+ (numC 1) (numC 2)) (numC 3)))
(check-equal? (parse '(* (* 1 2) 3)) (binopC '* (binopC '* (numC 1) (numC 2)) (numC 3)))
(check-equal? (parse '(ifleq0 1 2 3)) (ifleq0C (numC 1) (numC 2) (numC 3)))
(check-equal? (parse '(ifleq0 (+ 1 2) 2 3)) (ifleq0C (binopC '+ (numC 1) (numC 2)) (numC 2) (numC 3)))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse '(addone 1)) (appC 'addone (list (numC 1))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(1 2 3 4 5))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ / 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ + 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ - 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ undef 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ ifleq0 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ * 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(+ fundef 3))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse '(/ 1 2 3))))


;;parse-fundef test cases
(check-equal? (parse-fundef '{fundef {addone x} {+ x 1}}) (fundefC 'addone (list 'x) (binopC '+ (idC 'x) (numC 1))))
(check-equal? (parse-fundef '{fundef {main} {f 2}}) (fundefC 'main '() (appC 'f (list (numC 2)))))
(check-equal? (parse-fundef '{fundef {mult x} {* x 2}}) (fundefC 'mult (list 'x) (binopC '* (idC 'x) (numC 2))))
(check-equal? (parse-fundef '{fundef {main} {ifleq0 0 1 2}})
              (fundefC 'main '() (ifleq0C (numC 0) (numC 1) (numC 2))))
(check-equal? (parse-fundef '{fundef {sub z} {- z 1}}) (fundefC 'sub (list 'z) (binopC '- (idC 'z) (numC 1))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-fundef '(1 x (+ x 1)))))



;;parse-prog test cases
(check-equal? (parse-prog '((fundef (f x) (+ x 14)))) 
              (list (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3))))
              (list (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 (list 'z) (binopC '* (numC 2) (numC 3)))))
(check-equal? (parse-prog '((fundef (f x) (+ x 14)) (fundef (f2 z) (* 2 3)) (fundef (f3 y) (/ y 1))))
              (list (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 (list 'z) (binopC '* (numC 2) (numC 3)))
                    (fundefC 'f3 (list 'y) (binopC '/ (idC 'y) (numC 1)))))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog '())))
(check-exn (regexp (regexp-quote "invalid format DXUQ"))
           (lambda () (parse-prog 'a)))


;;interp-binopC test cases
(check-equal? (interp-binop '- (numC 1) (numC 0) (list
 (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14))))) 1)
(check-equal? (interp-binop '/ (numC 0) (numC 1) (list
 (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14))))) 0)
(check-exn (regexp (regexp-quote "Dividing by Zero DXUQ"))
           (lambda () (interp-binop '/ (numC 1) (numC 0) '())))


;;get-fundef test cases
(check-equal? (get-fundef 'f2 (list (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 (list 'z) (binopC '* (numC 2) (numC 3)))))
              (fundefC 'f2 (list 'z) (binopC '* (numC 2) (numC 3))))
(check-equal? (get-fundef 'f (list (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14)))
                    (fundefC 'f2 (list 'z) (binopC '* (numC 2) (numC 3)))))
              (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 14))))
(check-exn (regexp (regexp-quote "called DXUQ function not found"))
           (lambda () (get-fundef 'x '())))



;;subst test cases
(check-equal? (subst (numC 5) 'f (binopC '+ (idC 'f) (numC 5))) (binopC '+ (numC 5) (numC 5)))
(check-equal? (subst (numC 2) 'f (numC 5)) (numC 5))
(check-equal? (subst (numC 0) 'x (ifleq0C (idC 'x) (numC 1) (numC 2))) (ifleq0C (numC 0) (numC 1) (numC 2)))
(check-equal? (subst (idC 'z) 'x (idC 'x)) (idC 'z))
(check-equal? (subst (idC 'z) 'x (idC 'f)) (idC 'f))
(check-equal? (subst (numC 3) 'x (appC 'f (list (idC 'x)))) (appC 'f (list (numC 3))))


;;is_main? test cases
(check-equal? (is_main? (fundefC 'main '() (binopC '+ (numC 1) (numC 2)))) true)
(check-equal? (is_main? (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 1)))) false)



;;not_main? test cases
(check-equal? (not_main? (fundefC 'main '() (binopC '+ (numC 1) (numC 2)))) false)
(check-equal? (not_main? (fundefC 'f (list 'x) (binopC '+ (idC 'x) (numC 1)))) true)


|#