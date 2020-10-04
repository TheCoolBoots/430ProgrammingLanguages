#lang typed/racket

(require typed/rackunit)

;; 1
;; returns a string that is the elements of a list in reverse
(define (rev-str-app [l : (Listof String)]) : String
  (cond
    [(empty? l) ""]
    [else (string-append (rev-str-app (rest l)) (first l))]))

(check-equal? (rev-str-app '("apple" "banana" "orange")) "orangebananaapple")
(check-equal? (rev-str-app '()) "")
(check-equal? (rev-str-app '("apple")) "apple")
(check-equal? (rev-str-app '("1" "2" "3" "4" "5")) "54321")

;; 2
;; rev-str-app has the type "(-> (Listof String) String)"
;; This makes sense because the type of the function is one that
;; accepts a list of strings and returns a string.

;; + has a large list of different types. The list of types is
;; long because + can operate with many different types of input
;; whereas rev-str-app was only written to work with one type
;; of input.

;; 3
(define-type Bicycle (U Trek Bianchi Gunnar))
(struct Trek ([num-wheels? : Real]) #:transparent)
(struct Bianchi ([num-wheels? : Real]) #:transparent)
(struct Gunnar ([num-wheels? : Real]) #:transparent)

;; 4
;; filters out all bikes except for Trek bikes
(define (onlyTreks [l : (Listof Bicycle)]) : (Listof Trek)
  (cond
    [(empty? l) '()]
    [(Trek? (first l)) (cons (first l) (onlyTreks (rest l)))]
    [else (onlyTreks (rest l))]))

(define bikes (cons (Trek 3) (cons (Bianchi 3) (cons (Trek 2) '()))))
(define treks (cons (Trek 3) (cons (Trek 2) '())))
(define bianchis (cons (Bianchi 3) '()))
(define noTreks (cons (Bianchi 2) (cons (Gunnar 4) '())))

(check-equal? (onlyTreks bikes) treks)
(check-equal? (onlyTreks treks) treks)
(check-equal? (onlyTreks noTreks) '())

;; 5
;; filters out all bikes except for Bianchis
(define (onlyBianchis [l : (Listof Bicycle)]) : (Listof Bianchi)
  (cond
    [(empty? l) '()]
    [(Bianchi? (first l)) (cons (first l) (onlyBianchis (rest l)))]
    [else (onlyBianchis (rest l))]))

(check-equal? (onlyBianchis bikes) bianchis)
(check-equal? (onlyBianchis treks) '())
(check-equal? (onlyBianchis noTreks) (cons (Bianchi 2) '()))

;; 6
;; filters list of bikes based on given function
(define (onlyThese [l : (Listof Bicycle)] [f : (-> Any Boolean)]) : (Listof Bicycle)
  (cond
    [(empty? l) '()]
    [(f (first l)) (cons (first l) (onlyThese (rest l) f))]
    [else (onlyThese (rest l) f)]))

(check-equal? (onlyThese bikes Trek?) (onlyTreks bikes))
(check-equal? (onlyThese bikes Gunnar?) '())
(check-equal? (onlyThese bikes Bianchi?) (onlyBianchis bikes))

;; 7
;; appends two lists together to create one list
(define (my-append [l : (Listof Any)] [r : (Listof Any)]) : (Listof Any)
  (cond
    [(empty? l) r]
    [else (cons (first l) (my-append (rest l) r))]))

(check-equal? (my-append '(a b c) '(d e f)) '(a b c d e f))
(check-equal? (my-append '(a b) '(1 2 3 4)) '(a b 1 2 3 4))
(check-equal? (my-append '(a) '()) '(a))
(check-equal? (my-append '() '(b)) '(b))
(check-equal? (my-append '() '()) '())

;; 8
;; returns first n elements of a given list
(define (my-take [l : (Listof Any)] [n : Integer]) : (Listof Any)
  (cond
    [(or (empty? l) (<= n 0)) '()]
    [else (cons (first l) (my-take (rest l) (- n 1)))]))

(check-equal? (my-take '(a b c d e f) 0) '())
(check-equal? (my-take '(a b c d e f) 1) '(a))
(check-equal? (my-take '(a b c d e f) 2) '(a b))
(check-equal? (my-take '(a b c d e f) 3) '(a b c))
(check-equal? (my-take '(a b c d e f) 4) '(a b c d))
(check-equal? (my-take '(a b c d e f) 5) '(a b c d e))
(check-equal? (my-take '(a b c d e f) 6) '(a b c d e f))
(check-equal? (my-take '(a b c d e f) 111) '(a b c d e f))