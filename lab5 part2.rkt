#lang racket
 
(module sim-DXUQ4 racket
    (provide
     (rename-out [lambda fn]
                 [my-let let])
     #%module-begin
     #%datum
     #%app
     + - * / = equal? <=
     if)

  '{let {one = {fn {function arg} {function arg}}}
    {two = {fn {function arg} {{function arg} arg}}}
    {add = {fn {fn1 fn2} {fn1 fn2}}} in {add one two}}
  
 
  (define-syntax my-let
    (syntax-rules (in =)
      [(my-let [v = e] ... in eb)
       ((lambda (v ...) eb) e ...)])))