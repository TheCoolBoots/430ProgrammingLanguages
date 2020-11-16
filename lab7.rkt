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

; accepts a value and returns a list of memory locations that it refers to
(: value-to-memlocs (-> Value (Listof Address)))
(define (value-to-memlocs val)
  ;; NEED TO DO
  (list 1))

; accepts an environment, returns a list of memory locations it refers to
(: env-to-memlocs (-> Env (Listof Address)))
(define (env-to-memlocs env)
  ;; NEED TO DO
  (list 1))

; accepts a list of seen and unexamined mem locs and a store, returns a list of referred mem locs
(: mark (-> (Listof Address) (Listof Address) Store (Listof Address)))
(define (mark seen unexamined sto)
  ;; NEED TO DO
  (list 1))

; accepts a store and a list of still-used mem locs, returns a list of no-longer-used mem locs
(: sweep (-> Store (Listof Address) (Listof Address)))
(define (sweep sto used-locs)
  ;; NEED TO DO
  (list 1))

; accepts a list of envs and a store, returns a list of unreachable mem locs
(: collect (-> (Listof Env) Store (Listof Address)))
(define (collect envs sto)
  ;; NEED TO DO
  (list 1))