#lang typed/racket

;; represents an expression
(define-type ExprC (U Real Symbol String LamC AppC IfC SetC))
(struct LamC ([params : (Listof Symbol)]
              [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC]
              [args : (Listof ExprC)]) #:transparent)
(struct IfC ([test : ExprC]
             [thn : ExprC]
             [els : ExprC]) #:transparent)
(struct SetC ([var : Symbol]
              [newval : ExprC]))
(define-type PrimV (U '+ '- '* '/))
 
;; represents a value
(define-type Value (U Real Boolean String CloV PrimV
                      ArrayV))
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Env]))
(struct ArrayV ([addr : Address] [length : Natural])
  #:transparent)
 
;; represents an Environment
(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [loc : Natural]) #:transparent)
 
;; represents an address
(define-type Address Natural)
 
;; represents a store
(define-type Store (Mutable-HashTable Address Value))


; returns a list of all free memory locations
(: mark-and-sweep (-> Env Store (Listof Address)))
(define (sweep env sto)
  (define markedAddresses (remove-duplicates (mark env sto)))
  (define memAddresses (hash-keys sto))
  (set-subtract memAddresses markedAddresses))


; returns a list of all marked memory locations starting from top-env
(: mark (-> Env Store (Listof Address)))
(define (mark env sto)
  (cond
    [(empty? env) '()]
    [else (define currentMemAddr (Bind-loc (first env)))
          (define currentValReferences (val-mem-loc (hash-ref sto currentMemAddr) sto))
          (append (list currentMemAddr) currentValReferences (mark (rest env) sto))]))


; returns a list of memory locations that the given value refers to
(: val-mem-loc (-> Value Store (Listof Natural)))
(define (val-mem-loc val sto)
  (match val
    [(CloV params body env) (mark env sto)]
    [(ArrayV addr len) (get-array-references addr len 0 sto)]
    [other '()]))


; returns a list of memory locations referenced in the given array
(: get-array-references (-> Address Natural Natural Store (Listof Natural)))
(define (get-array-references memStart length currentIndex sto)
  (cond
    [(equal? currentIndex length) '()]
    [else (append (list (+ memStart currentIndex))
                  (val-mem-loc (hash-ref sto (cast (+ memStart currentIndex) Address)) sto)
                  (get-array-references memStart length (+ currentIndex 1) sto))]))





#|; returns a list of memory locations that the given value refers to
(: val-mem-loc (-> Value Env Store (Listof Integer)))
(define (val-mem-loc val env sto)
  (match val
    [(cloV body args clo-env) (check-clo-env clo-env sto)]
    [(arrayV memStart length) (check-array memStart 0 length env sto)]
    [other '()]))


; returns a list of memory locations referenced in the given array
(: check-array (-> Integer Integer Integer Env Store (Listof Integer)))
(define (check-array memStart currentIndex length env sto)
  (cond
    [(equal? currentIndex length) '()]
    [else (append (list (+ memStart currentIndex))
                  (val-mem-loc (hash-ref sto (+ memStart currentIndex)) env sto)
                  (check-array memStart (+ currentIndex 1) length env sto))]))


; returns a list of memory locations referenced in the given environment
(: check-clo-env (-> Env Store (Listof Integer)))
(define (check-clo-env env sto)
  (cond
    [(empty? env) '()]
    [else
     (define currentLoc (Bind-loc (first env)))
     (append (list currentLoc) (val-mem-loc (hash-ref sto currentLoc) env sto) (check-clo-env (rest env) sto))]))|#
