#lang typed/racket

(require typed/rackunit)

;; Asgn 1

;; Was able to finish all assigned problems. Had slight trouble
;; getting the binary tree functions to work, but after taking
;; a break was able to figure them out.

;; 2.1 Some Textbook Problems
;; 2.3.3
;; returns profit based on number of attendees
(define (total-profit [attendees : Integer]) : Real
  (- (* attendees 5) (+ 20 (* attendees 0.5))))

;; 3.3.3
(define PI 3.14159265359) 

;; returns the surface area of a cylinder based on radius and height
(define (area-cylinder [radius : Real] [height : Real]) : Real
  (+ (* (* 2 PI) (* radius radius)) (* (* 2 PI) (* radius height))))

;; 2.2 Magic Tricks
(define-type magic-trick (U Card-Trick Guillotine))
(struct Card-Trick ([decks : Real] [volunteers : Integer]))
(struct Guillotine ([realism : Real] [has-tiger? : Boolean]))

;; returns the amount of time a given magic trick will take
(define (trick-minutes [m : magic-trick]) : Real
  (match m
    [(Card-Trick d v) (* d (expt 2 v))]
    [(Guillotine r ht) (guillotine-trick ht)]))

;; calculates amount of time for the guillotine trick
(define (guillotine-trick [tiger? : Boolean]) : Real
  (cond
    [(equal? tiger? #t) 20]
    [else 10]))

;; 2.3 Low-degree Polynomials
(define-type Polynomial (U Linear Quadratic))
; linear form = Ax + B
(struct Linear ([a : Real] [b : Real]) #:transparent)
; quadratic form = Ax^2 + Bx + C
(struct Quadratic ([a : Real] [b : Real] [c : Real]) #:transparent)

;; returns result of a polynomial and a value for x
(define (interp [p : Polynomial] [x : Real]) : Real
  (match p
    [(Linear a b) (+ (* a x) b)]
    [(Quadratic a b c) (+ (* a x x) (* b x) c)]))

;; 2.4 Derivative
;; returns the derivative of a given polynomial
(define (derivative [p : Polynomial]) : Polynomial
  (match p
    [(Linear a b) (Linear 0 a)]
    [(Quadratic a b c) (Linear (* 2 a) b)]))

;; 2.5 Binary Tree
(define-type BTree (U Leaf Node))
(struct Leaf ([s : Symbol]) #:transparent)
(struct Node ([left : BTree] [right : BTree]) #:transparent)

;; examples of leaf data
(define leaf1 (Leaf 'a))
(define leaf2 (Leaf 'b))
(define leaf3 (Leaf 'c))

;; examples of node data
(define node1 (Node leaf1 leaf2))
(define r-node1 (Node leaf2 leaf1))
(define node2 (Node node1 node1))
(define r-node2 (Node r-node1 r-node1))
(define node3 (Node node2 node1))
(define r-node3 (Node r-node1 r-node2))
(define node4 (Node leaf1 node3))
(define r-node4 (Node r-node3 leaf1))

;; Image of the above tree
;;                   node3
;;        node 2                 node1
;;    node1       node1       leaf1 leaf2
;; leaf1 leaf2 leaf1 leaf2

;; 2.6 Mirror
;; returns the mirror of a given binary tree
(define (mirror [b : BTree]) : BTree
  (match b
    ; case of tree being one leaf, returns a leaf with the same symbol
    [(Leaf s) (Leaf s)]
    [(Node l r)
     (cond
       ; both node children are leaves
       [(and (Leaf? l) (Leaf? r)) (Node r l)]
       ; left child a leaf
       [(Leaf? l) (Node (mirror r) l)]
       ; right child a leaf
       [(Leaf? r) (Node r (mirror l))]
       ; both children are nodes
       [else (Node (mirror r) (mirror l))])]))

;; 2.7 Min-Depth
;; returns the min length path to a leaf in a given binary tree
(define (min-depth [b : BTree]) : Integer
  (match b
    ; single leaf, path length is zero
    [(Leaf s) 0]
    [(Node l r)
     (cond
       ; left or right child a leaf
       [(or (Leaf? l) (Leaf? r)) 1]
       ; both children are nodes
       [else (+ 1 (min-depth l)) (+ 1 (min-depth r))])]))

;; 2.8 Substitution
(define (subst [b : BTree] [s : Symbol] [rep : BTree]) : BTree
  (match b
    ;; handles a single leaf
    [(Leaf sym)
     (cond
       ; symbol match
       [(equal? sym s) rep]
       ; non-match, return original leaf
       [else (Leaf sym)])]
    [(Node l r)
     (cond
       ; left child is a match
       [(and (Leaf? l) (equal? (Leaf-s l) s)) (Node rep (subst r s rep))]
       ; right child is a match
       [(and (Leaf? r) (equal? (Leaf-s r) s)) (Node (subst l s rep) rep)]
       ; neither child is a match
       [else (Node (subst l s rep) (subst r s rep))])]))

;; Test Cases
;; 2.1
;; 2.3.3
(check-= (total-profit 10) 25 0)
(check-= (total-profit 0) -20 0)
(check-= (total-profit 100) 430 0)

;; 3.3.3
(check-= (area-cylinder 10 20) 1884.96 0.01)
(check-= (area-cylinder 1 1) (* 4 PI) 0.01)
(check-= (area-cylinder 0 1) 0 0)

;; 2.2
(check-= (guillotine-trick #t) 20 0)
(check-= (guillotine-trick #f) 10 0)
(check-= (trick-minutes (Guillotine 2 #t)) 20 0)
(check-= (trick-minutes (Guillotine 2 #f)) 10 0)
(check-= (trick-minutes (Card-Trick 20 2)) 80 0)

;; 2.3
(check-= (interp (Linear 10 2) 5) 52 0)
(check-= (interp (Linear 0 10) 1000) 10 0)
(check-= (interp (Quadratic 1 4 12) 3) 33 0)
(check-= (interp (Quadratic 3 10 13) 2) 45 0)

;; 2.4
(check-equal? (derivative (Linear 10 3)) (Linear 0 10))
(check-equal? (derivative (Linear 100 3241)) (Linear 0 100))
(check-equal? (derivative (Quadratic 25 43 123)) (Linear 50 43))
(check-equal? (derivative (Quadratic 1000 1234 100)) (Linear 2000 1234))

;; 2.6
(check-equal? (mirror (Leaf 's)) (Leaf 's))
(check-equal? (mirror node1) (Node leaf2 leaf1))
(check-equal? (mirror node4) r-node4)
(check-equal? (mirror (Node node4 leaf2)) (Node leaf2 r-node4))
(check-equal? (mirror node1) r-node1)
(check-equal? (mirror node2) r-node2)
(check-equal? (mirror node3) r-node3)

;; 2.7
(check-= (min-depth leaf3) 0 0)
(check-= (min-depth node3) 2 0)
(check-= (min-depth node4) 1 0)
(check-= (min-depth node1) 1 0)

;; 2.8
(check-equal? (subst (Leaf 's) 's (Leaf 'a)) (Leaf 'a))
(check-equal? (subst (Node (Leaf 'aa) node1) 'aa (Leaf 'david)) (Node (Leaf 'david) node1))
(check-equal? (subst (Node node1 (Leaf 'd)) 'd (Leaf 'test)) (Node node1 (Leaf 'test)))
(check-equal? (subst node4 'aaa node2) node4)