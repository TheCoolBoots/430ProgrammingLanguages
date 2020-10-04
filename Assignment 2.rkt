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

(define-type ExprC (U numC binopC ifleq0C))
(struct numC ([n : Real]) #:transparent)
(struct ifleq0C ([expIf : ExprC] [expThen : ExprC] [expElse : ExprC]) #:transparent)
(struct binopC ([op : OperatorC]) #:transparent)


(define-type OperatorC (U addC subC multC divC))
(struct addC ([l : ExprC] [r : ExprC]) #:transparent)
(struct subC ([l : ExprC] [r : ExprC]) #:transparent)
(struct multC ([l : ExprC] [r : ExprC]) #:transparent)
(struct divC ([l : ExprC] [r : ExprC]) #:transparent)


; interprets an ExprC into a real number
(: interp (-> ExprC Real))
(define (interp expr)
  (match expr
    [(numC n) n]
    [(binopC op) (interpBinop op)]
    [(ifleq0C expIf expThen expElse) (ifleq0 expIf expThen expElse)]))


; interprets a binary operator
(: interpBinop (-> OperatorC Real))
(define (interpBinop op)
  (match op
    [(addC l r) (+ (interp l) (interp r))]
    [(subC l r) (- (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]
    [(divC l r) (/ (interp l) (interp r))]))


; handles the logic for if less than or equal to zero
; if expIf is less than or equal to zero, return expThen
; else, return expElse
(: ifleq0 (-> ExprC ExprC ExprC Real))
(define (ifleq0 expIf expThen expElse)
 (cond
   [(<= (interp expIf) 0) (interp expThen)]
   [else (interp expElse)]))


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
    [(list '+ l r) (binopC (addC (parse l) (parse r)))]
    [(list '- l r) (binopC (subC (parse l) (parse r)))]
    [(list '* l r) (binopC (multC (parse l) (parse r)))]
    [(list '/ l r) (binopC (divC (parse l) (parse r)))]))

(check-equal? (parseBinary '(+ 1 2)) (binopC (addC (numC 1) (numC 2))))
(check-equal? (parseBinary '(+ (- 1 2) 3))
              (binopC (addC (binopC (subC (numC 1) (numC 2))) (numC 3))))
(check-equal? (parseBinary '(/ 3 (* 1 3)))
              (binopC (divC (numC 3) (binopC (multC (numC 1) (numC 3))))))

; parses then interprets an S-expression
(: top-interp (-> Sexp Number))
(define (top-interp exp)
  (interp (parse exp)))

(check-equal? (top-interp '(ifleq0 (+ 1 2) 0 2)) 2)

; interpreting binary operators
(check-equal? (interpBinop (addC (numC 1) (numC 2))) 3)
(check-equal? (interpBinop (subC (numC 1) (numC 2))) -1)
(check-equal? (interpBinop (multC (numC 1) (numC 2))) 2)
(check-equal? (interpBinop (divC (numC 1) (numC 2))) 1/2)
(check-equal? (interpBinop (addC (binopC (subC (numC 3) (numC 4))) (numC 2))) 1)

; interpreting ExprC's
(check-equal? (interp (binopC (addC (numC 3) (numC 2)))) 5)
(check-equal? (interp (numC 5)) 5)

; top-interp tests
(check-equal? (top-interp '(ifleq0 (+ 1 2) 0 2)) 2)
(check-equal? (top-interp '(ifleq0 (+ 0 0) 0 2)) 0)

; parsing a binary operator
(check-equal? (opSymbol? '+) #t)
(check-equal? (opSymbol? '-) #t)
(check-equal? (opSymbol? '*) #t)
(check-equal? (opSymbol? '/) #t)
(check-equal? (opSymbol? 'k) #f)