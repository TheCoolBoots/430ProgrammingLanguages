#lang typed/racket

(require typed/rackunit)

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
(struct nullV () #:transparent)

(struct Bind ([name : Symbol] [loc : Integer]) #:transparent)
(define-type Env (Listof Bind))


(define-type Store (Mutable-HashTable Integer Value))


;;takes a value and returns a list of mem locations
(define (val-locs [val : Value]) : (Listof Integer)
  (list 1))


;;takes an Env and returns a list of mem locations
(define (mem-locs [env : Env]) : (Listof Integer)
  (cond
    [(empty? env) '()]
    [else (cons (Bind-loc (first env)) (mem-locs (rest env)))]))

(check-equal? (mem-locs (list)) '())
(check-equal? (mem-locs (list (Bind 'a 1) (Bind 'b 2))) '(1 2))

;;takes list of seen mem locations and list of unexamined mem locations,
;;returns list of mem locations referred to
(define (mark [seen : (Listof Integer)] [unseen : (Listof Integer)] [st : Store]) : (Listof Integer)
  (list 1))

;;takes a store and list of still-used locations, returns list of non used locations
(define (sweep [st : Store] [used : (Listof Integer)]) : (Listof Integer)
  (list 1))

;;takes a list of env and list of store, returns list of unreachable locations
(define (collect [envs : (Listof Env)] [st : Store]) : (Listof Integer)
  (list 1))



  
