#lang typed/racket

(require typed/rackunit)

;; Exercise 15
;; return true if sunny = false or friday = true
(define (==> [sunny : Boolean] [friday : Boolean]) : Boolean
  (if (or (equal? sunny false) (equal? friday true))
      true
      false)
  )

(check-equal? (==> true true) true)
(check-equal? (==> false true) true)
(check-equal? (==> true false) false)
(check-equal? (==> false false) true)

;; Exercise 19
;; inserts a "_" at a given location
(define (string-insert [str : String] [i : Integer]) : String
  (define new-str "")
  (define new-str1 (string-append new-str (substring str 0 i)))
  (define new-str2 (string-append new-str1 "_"))
  (string-append new-str2 (substring str i (string-length str)))
  )

(check-equal? (string-insert "abc" 1) "a_bc")
(check-equal? (string-insert "abc" 2) "ab_c")
(check-equal? (string-insert "abc" 3) "abc_")
(check-equal? (string-insert "" 0) "_")

;; Exercise 27
;; sample problem from book
;; constants for exercise 27
(define PEOPLE 120)
(define TICKETCHANGE 5.0)
(define PEOPLECHANGE 15)
(define CHANGERATIO 0.1)
(define FIXEDPRICE 180)
(define TICKETPRICERATIO 0.4)

(define (attendees [ticket-price : Real]) : Real
  (- PEOPLE (* (- ticket-price TICKETCHANGE) (/ PEOPLECHANGE CHANGERATIO))))

(define (revenue [ticket-price : Real]) : Real
  (* ticket-price (attendees ticket-price)))

(define (cost [ticket-price : Real]) : Real
  (+ FIXEDPRICE (* TICKETPRICERATIO (attendees ticket-price))))

(define (profit [ticket-price : Real]) : Real
  (- (revenue ticket-price)
     (cost ticket-price)))

(check-equal? (profit 5) 372.0)

;; 4.2
;; returns the amount of interest generated over one year
(define (interest [deposit : Real]) : Real
  (if (> deposit 5000)
      (* deposit 1.05)
      (if (<= deposit 1000)
          (* deposit 1.04)
          (* deposit 1.045))
      )
  )

(check-= (interest 100) 104.0 0)
(check-= (interest 1000) 1040.0 0)
(check-= (interest 5001) 5251.05 0)
(check-= (interest 5000) 5225.0 0)
(check-= (interest 4000) 4180.0 0)

;; 4.4
(define-type Office (U Desk Bookshelf))

;; Desk struct for 4.4
(struct Desk ([length : Real] [width : Real] [depth : Real]))
;; example desk
(define desk1 (Desk 5 3 3))

;; Bookshelf struct for 4.4
(struct Bookshelf ([depth : Real] [num-shelves : Real] [shelf-width : Real]))
;; example bookshelf
(define shelf1 (Bookshelf 2 10 1))

;; returns the footprint of the office furniture
(define (furniture-footprint [f : Office]) : Real
  (match f
    [(Desk length width depth) (* length width)]
    [(Bookshelf depth ns sw) (* depth sw)]))

(check-= (furniture-footprint desk1) 15.0 0)
(check-= (furniture-footprint shelf1) 2.0 0)