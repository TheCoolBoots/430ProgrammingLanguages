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
(define-type Env (HashTable Symbol Address))

;; represents an address
(define-type Address Natural)
 
;; represents a store
(define-type Store (Mutable-HashTable Address Value))


; returns a list of all free memory locations
(: mark-and-sweep (-> Env Store Store (Listof Address)))
(define (mark-and-sweep env sto seen)
  (define markedAddresses (remove-duplicates (mark (hash-keys env) (hash-values env) sto seen)))
  (define memAddresses (hash-keys sto))
  (set-subtract memAddresses markedAddresses))


; returns a list of all marked memory locations starting from top-env
(: mark (-> (Listof Symbol) (Listof Address) Store Store (Listof Address)))
(define (mark hashKeys hashValues sto seen)
  (cond
    [(empty? hashKeys) '()]
    [else 
     (cond
       [(hash-ref seen (first hashValues) #f)
        (mark (rest hashKeys) (rest hashValues) sto seen)]
       [else
        (hash-set! seen (first hashValues) #t)
        (define currentValReferences (val-mem-loc (hash-ref sto (first hashValues)) sto seen))    
        (append (list (first hashValues))
                currentValReferences
                (mark (rest hashKeys) (rest hashValues) sto seen))])]))


; returns a list of memory locations that the given value refers to
(: val-mem-loc (-> Value Store Store (Listof Natural)))
(define (val-mem-loc val sto seen)
  (match val
    [(CloV params body env) (mark (hash-keys env) (hash-values env) sto seen)]
    [(ArrayV addr len) (get-array-references addr len 0 sto seen)]
    [other '()]))


; returns a list of memory locations referenced in the given array
(: get-array-references (-> Address Natural Natural Store Store (Listof Natural)))
(define (get-array-references memStart length currentIndex sto seen)
  (cond
    [(equal? currentIndex length) '()]
    [(hash-ref seen (+ memStart currentIndex) #f)
     (get-array-references memStart length (+ currentIndex 1) sto seen)]
    [else
     (hash-set! seen (+ memStart currentIndex) #t)
     (append (list (+ memStart currentIndex))
             (val-mem-loc (hash-ref sto (+ memStart currentIndex)) sto seen)
             (get-array-references memStart length (+ currentIndex 1) sto seen))]))


(define top-env
  (ann (make-hash
        (list (cons 'hi 0)
              (cons 'bye 1)
              (cons 'arr1 4)
              (cons 'clo1 8)
              (cons 'temp1 0)))
       Env))


(define clo-env
  (ann (make-hash
        (list (cons 'hi 4)
              (cons 'bye 5)
              (cons 'test 8)
              ))
       Env))


(define top-store
  (ann (make-hash
        (list (cons 0 9)
              (cons 1 "magic")
              (cons 2 (ArrayV 3 1))
              (cons 3 (ArrayV 2 1))
              (cons 4 (ArrayV 5 3))
              (cons 5 1)
              (cons 6 2)
              (cons 7 (ArrayV 4 1))
              (cons 8 (CloV '(a b) 5 clo-env))))
       Store))

(define top-seen
  (ann (make-hash)
       Store))

(mark-and-sweep top-env top-store top-seen)
