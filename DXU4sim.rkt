#lang racket
 
(module sim-DXUQ4 racket
    (provide
     (rename-out [lambda fn])
     #%module-begin
     #%datum
     #%app
     + - * / = <=
     if))
 
(module my-mod1 (submod ".." sim-DXUQ4)
 
#|
 ExprC = Num
       | id
       | String
       | {if Expr Expr Expr}
       | {let {id = Expr} ... in Expr}
       | {fn {id ...} Expr}
       | {Expr Expr ...}
|#

  (+ 1 2)

  
 
  )
 
(require 'my-mod1)