#lang typed/racket

(require typed/rackunit)


(struct Trek ([n : Number]) #:transparent)
(struct Bianchi ([n : Number]) #:transparent)
(struct Gunnar ([n : Number]) #:transparent)
(define-type bicycle (U Trek Bianchi Gunnar))
(define-type bicycleCheck (U (-> Any Boolean : Gunnar) (-> Any Boolean : Trek) (-> Any Boolean : Bianchi)))


; 1: reverse the order of the strings in given list
; combine them into a string
(: rev-str-app (-> (Listof String) String))
(define (rev-str-app l)
  (cond
    [(empty? l) ""]
    [else (string-append (rev-str-app (rest l)) (first l))]))


; 2: rev-str-app is of type (-> (Listof String) String)
; this makes sense because that is the definition of the function
; + seems to have multiple overloads because there can be
; multiple definitions of addition


; 3: filters out all the non-Trek bicycles from a list
(: onlyTreks (-> (Listof bicycle) (Listof bicycle)))
(define (onlyTreks l)
  (cond
    [(empty? l) '()]
    [(Trek? (first l)) (cons (first l) (onlyTreks (rest l)))]
    [else (onlyTreks (rest l))]))


; 4: filters out all the non-Bianchi bicycles from a list
(: onlyBianchi (-> (Listof bicycle) (Listof bicycle)))
(define (onlyBianchi l)
  (cond
    [(empty? l) '()]
    [(Bianchi? (first l)) (cons (first l) (onlyBianchi (rest l)))]
    [else (onlyBianchi (rest l))]))


; 5: filters out all of the given bicycle from a list
(: onlyThese (-> (Listof bicycle) bicycleCheck (Listof bicycle)))
(define (onlyThese l b)
  (cond
    [(empty? l) '()]
    [(b (first l)) (cons (first l) (onlyThese (rest l) b))]
    [else (onlyThese (rest l) b)]))


; 6: appends one list with another list
(: my-append (-> (Listof Any) (Listof Any) (Listof Any)))
(define (my-append L1 L2)
  (cond
    [(empty? L1) L2]
    [else (cons (first L1) (my-append (rest L1) L2))]))


; 7: returns first n elements of given list
(: my-take (-> (Listof Any) Integer (Listof Any)))
(define (my-take l n)
  (cond
    [(or (equal? n 0) (empty? l)) '()]
    [else (cons (first l) (my-take (rest l) (- n 1)))]))

(check-equal? (my-take '() 0) '())
(check-equal? (my-take '(1 "hello" 3 'a) 2) '(1 "hello"))
(check-equal? (my-take '(1 2 3) 4) '(1 2 3))

; 8: returns true when second number is either twice or three times the first one
(: dub-or-trip (-> Real Real Boolean))
(define (dub-or-trip n m)
  (cond
    [(equal? (* 2 n) m) #t]
    [(equal? (* 3 n) m) #t]
    [else #f]))


(check-equal? (rev-str-app '("General" "Kenobi")) "KenobiGeneral")
(check-equal? (rev-str-app '()) "")
(check-equal? (onlyTreks '()) '())
(define t1 (Trek 1))
(define t2 (Trek 2))
(define b1 (Bianchi 1))
(define b2 (Bianchi 2))
(define g1 (Gunnar 1))
(check-equal? (onlyTreks (cons t1 '())) (cons t1 '()))
(check-equal? (onlyTreks (list t1 b1 b2 g1 t2)) (list t1 t2))
(check-equal? (onlyBianchi '()) '())
(check-equal? (onlyBianchi (cons b1 '())) (cons b1 '()))
(check-equal? (onlyBianchi (list t1 b1 b2 g1 t2)) (list b1 b2))
(check-equal? (onlyThese '() Gunnar?) '())
(check-equal? (onlyThese (cons b1 '()) Bianchi?) (cons b1 '()))
(check-equal? (onlyThese (list t1 b1 b2 g1 t2) Bianchi?) (list b1 b2))
(check-equal? (my-append '() '()) '())
(check-equal? (my-append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(check-equal? (dub-or-trip 1 2) #t)
(check-equal? (dub-or-trip 1 3) #t)
(check-equal? (dub-or-trip 1 1) #f)
(check-equal? (dub-or-trip 5.0 15.0) #t)