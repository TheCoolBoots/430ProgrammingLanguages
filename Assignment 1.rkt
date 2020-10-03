#lang typed/racket

(require typed/rackunit)


; Title:   CSC 430 Assignment 1
; Date:    9/19/2020
; Author's note: As of 9/23/2020 I am passing all the handin test cases. this
;                was a fun assignment that returned my brain to thinking like
;                a computer scientist after a long break so it was also refreshing.
;                I didn't really have trouble with any of the questions once I
;                thought about it for a bit and then wrote out pseudocode in my
;                notebook. I also had fun becoming more familiar and comfortable
;                with the Racket language. The All Paths Lengths problem, however
;                is no easy task. I have been using depth first searches for all
;                of my tree traversals but I think I have to change my algorithm.



; definitions for magic tricks
(struct Card-Trick ([decks : Integer] [volunteers : Integer]))
(struct Guillotine ([realism : Number] [has-tiger? : Boolean]))
(define-type Magic-Trick (U Card-Trick Guillotine))

; definitions for linear and quadratic polynomials
(struct Linear ([A : Number] [B : Number]) #:transparent)
(struct Quadratic ([A : Number] [B : Number] [C : Number]) #:transparent)
(define-type polynomial (U Linear Quadratic))

; definitions for binary tree leaves and nodes
(struct Leaf ([val : Symbol]) #:transparent)
(struct Node ([left : (U Node Leaf)] [right : (U Node Leaf)]) #:transparent)
(define-type BTree (U Node Leaf))


; exercise 2.3.3
; returns the total profit for a theater given a number of attendees
(: total-profit (-> Integer Number))
(define (total-profit attendees)
  (- (* 5 attendees) (+ 20 (* .50 attendees))))      ; 5 * num_attendees - (20 + .5 * num_attendees)


; exercise 3.3.3
; returns the surface area of a cylinder given radius and height of cylinder
(: area-cylinder (-> Real Real Real))
(define (area-cylinder radius height)
  (+ (* height pi (+ radius radius)) (* pi radius radius 2)))   ; 2 * pi * radius * height + 2 * pi * r^2


; 2.2 -- Magic tricks
; returns  time required for the given magic trick

(define (trick-minutes mt)
  (match mt
    [(Card-Trick decks volunteers) (* decks (power 2 volunteers))]
    [(Guillotine realism has-tiger?)
     (cond
       [has-tiger? 20]
       [else 10])]                    
    ))

; returns n^x through recursive multiplication
(: power (-> Integer Integer Integer))
(define (power n x)
  (cond
      [(<= x 0) 1]
      [else (* n (power n (- x 1)))]))


; 2.3 Low Degree Polynomials
; returns evaluation of polynomial at x
(: interp (-> polynomial Number Number))
(define (interp poly x)
  (match poly
    [(Linear a b) (+ (* a x) b)]
    [(Quadratic a b c) (+ (* a x x) (* b x) c)]))


; 2.4 derivatiave
; returns the derivative of a quadratic or linear polynomial
(: derivative (-> polynomial (U polynomial Number)))
(define (derivative poly)
  (match poly
    [(Linear a b) (Linear 0 a)]
    [(Quadratic a b c) (Linear (* 2 a) b)]))


; 2.6.2 mirror
; returns a mirror image of the given binary tree
(: mirror (-> BTree BTree))
(define (mirror root)
  (match root
    [(Node l r) (Node (mirror r) (mirror l))]
    [(Leaf v) root]))


; 2.7 Min-Depth
; returns the length of the shortest path to a leaf
; returns zero if leaf
; for node, returns the shortest depth child plus 1 
(: min-depth (-> BTree Integer))
(define (min-depth root)
  (match root
    [(Leaf leaf) 0]
    [(Node left right) (min (+ 1 (min-depth left)) (+ 1 (min-depth right)))]))


; 2.8 Substitution
; replace all leaves of given tree with replacement tree
(: subst (-> BTree Symbol BTree BTree))
(define (subst source target replace)
  (match source
    [(Leaf val) (cond
                  [(equal? val target) replace]
                  [else source])]
    [(Node left right) (Node (subst left target replace) (subst right target replace))]))


; 2.9 All Path Lengths (unfinished)
; returns a list of all the path lengths from the root to each leaf
;(: all-path-lengths (-> BTree (Listof Integer)))
;(define (all-path-lengths root)
;  (apl-helper root 0)

; helper function for all-path-lengths -- has additional length parameter
;(: apl-helper (-> BTree Integer (Listof Integer)))
;(define (apl-helper root dist)
;  (match root



; book exercise tests
(check-equal? (total-profit 0) -20)
(check-equal? (total-profit 10) 25.0)
(check-equal? (area-cylinder 1 1) (+ (* 1 (* pi (+ 1 1))) (* pi 1 1 2)))
(check-equal? (power 2 3) 8)
(check-equal? (power 2 0) 1)
(check-equal? (power 3 2) 9)


; trick-minutes tests
(check-equal? (trick-minutes (Card-Trick 1 0)) 1)
(check-equal? (trick-minutes (Card-Trick 2 4)) 32)
(check-equal? (trick-minutes (Guillotine 5 #t)) 20)
(check-equal? (trick-minutes (Guillotine 5 #f)) 10)


; polynomial tests
(check-equal? (interp (Linear 1 1) 1) 2)
(check-equal? (interp (Linear 1 2) 1) 3)
(check-equal? (interp (Linear 1 1) 3) 4)
(check-equal? (interp (Quadratic 1 1 1) 1) 3)
(check-equal? (interp (Quadratic 2 1 1) 1) 4)
(check-equal? (interp (Quadratic 1 2 1) 3) 16)
(check-equal? (derivative (Linear 5 1)) (Linear 0 5))
(check-equal? (derivative (Linear 0 1)) (Linear 0 0))
(check-equal? (derivative (Quadratic 3 2 1)) (Linear 6 2))
(check-equal? (derivative (Quadratic 3 0 1)) (Linear 6 0))


; test trees for BTree unit tests
(define T1 (Node (Leaf 'a) (Leaf 'b)))
(define T2 (Node (Leaf 'b) (Leaf 'a)))
(define T3 (Node (Node (Leaf 'a) (Leaf 'b)) (Leaf 'c)))
(define T4 (Node (Leaf 'c) (Node (Leaf 'b) (Leaf 'a))))
(define T5 (Node (Node (Leaf 'b) (Leaf 'a)) (Node (Leaf 'b) (Leaf 'a))))
(define T6 (Node (Node (Leaf 'b) (Leaf 'a)) (Leaf 'b)))
(define T7 (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'a))))

; BTree tests
(check-equal? (mirror T1) T2)
(check-equal? (mirror T3) T4)
(check-equal? (mirror (Leaf 'a)) (Leaf 'a))
(check-equal? (min-depth (Leaf 'c)) 0)
(check-equal? (min-depth T1) 1)
(check-equal? (min-depth T3) 1)
(check-equal? (min-depth T4) 1)
(check-equal? (min-depth T5) 2)
(check-equal? (subst (Leaf 'a) 'a (Leaf 'b)) (Leaf 'b))
(check-equal? (subst T1 'a T2) T6)
(check-equal? (subst T1 'b T2) T7)


