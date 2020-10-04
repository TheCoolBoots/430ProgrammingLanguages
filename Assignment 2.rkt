#lang typed/racket

(require typed/rackunit)

#|
  DXUQ2	 = num
 	 | {+ DXUQ2 DXUQ2}
 	 | {- DXUQ2 DXUQ2}
 	 | {* DXUQ2 DXUQ2}
 	 | {/ DXUQ2 DXUQ2}
 	 | {id DXUQ2}
 	 | {ifleq0 DXUQ2 DXUQ2 DXUQ2}
 	 | id

  DEFN	 = {fundef {id id} DXUQ2}

(: parse (-> Sexp ExprC))
(define (parse s)
  '())

(: parse-fundef (-> Sexp FundefC))
(define (parse-fundef s)
  '())

(: parse-prog (-> Sexp (listof FundefC)))
(define (parse-prog s)
  '())

(: interp-fns (-> (listof FundefC) Real))
(define (interp-fns funs)
  0)

(: interp (-> ExprC (listof FundefC) Real))
(define (interp exp funs)
  0)

(: top-interp (-> Sexp Real))
(define (top-interp fun-sexps)
  0)

|#

(define-type ExprC (U numC binopC ifleq0C idC appC))
(struct numC ([n : Real]) #:transparent)
(struct ifleq0C ([expIf : ExprC] [expThen : ExprC] [expElse : ExprC]) #:transparent)
(struct binopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct idC ([s : Symbol]))
(struct appC ([fun : Symbol] [arg : ExprC]))


(define-type FunDefC fdC)
(struct fdC ([name : Symbol] [arg : Symbol] [body : ExprC]))

; interprets an ExprC into a real number
(: interp (-> (Listof FunDefC) ExprC Real))
(define (interp fds expr)
  (match expr
    [(numC n) n]
    [(binopC op l r) (interpBinop fds expr)]
    [(ifleq0C expIf expThen expElse) (ifleq0 fds expIf expThen expElse)]))


; interprets a binary operator
(: interpBinop (-> (Listof FunDefC) binopC Real))
(define (interpBinop fds op)
  (match op
    [(binopC op l r)
     (match op
       ['+ (+ (interp fds l) (interp fds r))]
       ['- (- (interp fds l) (interp fds r))]
       ['/ (/ (interp fds l) (interp fds r))]
       ['* (* (interp fds l) (interp fds r))]
       [other (error 'ERROR "Binary Operator unimplemented")])]))
       


; handles the logic for if less than or equal to zero
; if expIf is less than or equal to zero, return expThen
; else, return expElse
(: ifleq0 (-> (Listof FunDefC) ExprC ExprC ExprC Real))
(define (ifleq0 fds expIf expThen expElse)
 (cond
   [(<= (interp fds expIf) 0) (interp fds expThen)]
   [else (interp fds expElse)]))


; converts an S-expression into an ExprC
(: parse (-> Sexp ExprC))
(define (parse exp)
  (match exp
    [(? real? num) (numC exp)]
    [(list 'ifleq0 e1 e2 e3) (ifleq0C (parse e1) (parse e2) (parse e3))]
    [(list op l r) #:when (opSymbol? op) (parseBinary exp)]
    [other (error 'ERROR "Unrecognizable expression")]))


; checks to see if a symbol is a binary operator
(: opSymbol? (-> Any Boolean))
(define (opSymbol? in)
  (match in
    ['* #t]
    ['/ #t]
    ['+ #t]
    ['- #t]
    [other #f]))


; parses a binary operator expression into a binopC
(: parseBinary (-> Sexp binopC))
(define (parseBinary exp)
  (match exp
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [other (error 'ERROR "Binary Operator unimplemented")]))

(check-equal? (parseBinary '(+ 1 2)) (binopC '+ (numC 1) (numC 2)))
(check-equal? (parseBinary '(+ (- 1 2) 3))
              (binopC '+ (binopC '- (numC 1) (numC 2)) (numC 3)))
(check-equal? (parseBinary '(/ 3 (* 1 3)))
              (binopC '/ (numC 3) (binopC '* (numC 1) (numC 3))))

; parses then interprets an S-expression
(: top-interp (-> (Listof FunDefC) Sexp Number))
(define (top-interp fds exp)
  (interp fds (parse exp)))

;(check-equal? (top-interp '(ifleq0 (+ 1 2) 0 2)) 2)

; interpreting binary operators
;(check-equal? (interpBinop (binopC '+ (numC 1) (numC 2))) 3)
;(check-equal? (interpBinop (binopC '- (numC 1) (numC 2))) -1)
;(check-equal? (interpBinop (binopC '* (numC 1) (numC 2))) 2)
;(check-equal? (interpBinop (binopC '/ (numC 1) (numC 2))) 1/2)
;(check-equal? (interpBinop (binopC '+ (binopC '- (numC 3) (numC 4)) (numC 2))) 1)

; interpreting ExprC's
;(check-equal? (interp (binopC '+ (numC 3) (numC 2))) 5)
;(check-equal? (interp (numC 5)) 5)

; top-interp tests
;(check-equal? (top-interp '(ifleq0 (+ 1 2) 0 2)) 2)
;(check-equal? (top-interp '(ifleq0 (+ 0 0) 0 2)) 0)

; parsing a binary operator
;(check-equal? (opSymbol? '+) #t)
;(check-equal? (opSymbol? '-) #t)
;(check-equal? (opSymbol? '*) #t)
;(check-equal? (opSymbol? '/) #t)
;(check-equal? (opSymbol? 'k) #f)