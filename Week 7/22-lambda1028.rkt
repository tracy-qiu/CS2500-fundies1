;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 22-lambda1028) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; short-msgs : [List-of Strings] -> [List-of Strings]
; Keeps just the strings that are < 14 characters

(define SHORT-LEN 14)

(check-expect
 (short-msgs (list))
 (list))

(check-expect
 (short-msgs (list "a" "b" "hi" "12345678901234567"))
 (list "a" "b" "hi"))

(define (short-msgs los)
  (filter (λ (msg) (< (string-length msg) SHORT-LEN))
            los))

#|
(define (short-msgs los)
  (filter short-msg? los))

; short-msg? : String -> Boolean
; Returns whether or not the message is < 14 chars

(check-expect (short-msg? "hi") #true)
(check-expect (short-msg? "123456789012345") #false)
 
(define (short-msg? msg)
  (< (string-length msg) SHORT-LEN))
|#

; double-squares : Nat -> [List-of Nat]
; The first n double squares

(check-expect (double-squares 0) '())
(check-expect (double-squares 1) (list 0))
(check-expect (double-squares 4) (list 0 2 8 18))

(define (double-squares n)
  (build-list n (λ (n) (* 2 (sqr n)))))

#|
(define (double-squares n)
  (local [; double-square : Nat -> Nat
          ; The nth double square
          (define (double-square n)
            (* 2 (sqr n)))]
    (build-list n double-square)))
|#

; add-3-to-all : [List-of Number] -> [List-of Number]
; Adds 3 to every number

(check-expect (add-3-to-all '()) '())
(check-expect (add-3-to-all (list 0)) (list 3))
(check-expect (add-3-to-all (list -7 2.3)) (list -4 5.3))

(define (add-3-to-all lon)
  (map (λ (n) (+ 3 n)) lon))
#|  (local [; add-3 : Number -> Number
          ; Adds 3 to a number
          ; Given 4, should return 7
          (define (add-3 n)
            (+ n 3))]
    (map add-3 lon)))
|#

; v: lambda λ
; s: (λ (arg1 arg2 ...) expr)
; sem: return a nameless function
; λ takes away purpose statements and signatures
; local takes away the way of calling them other places
; return of λ is a function
; (λ (input) (what we want it to do))


;(define (bar n)
;  (+ n 3))
(define bar (λ (n) (+ n 3)))
;(λ (n) (+ n 3))



(define (weird x)
  ((if (even? x)
       (λ (x) 10)
       (λ (x) (+ (sqr x) 1))) x))

; Design the function product-sqrts-plus-one that accepts a
; list of numbers and returns the product of (the sqrt of each number plus one).

; product-sqrts-plus-one : [List-of Number] -> Number
; returns the product of (the sqrt of each number plus one) 


(check-expect (product-sqrts-plus-one (list)) 1)
(check-expect (product-sqrts-plus-one (list 1 4 9)) 24)

(define (product-sqrts-plus-one/mapfold lon)
  (foldr
   *
   1
   (map (λ (n) (add1 (sqrt n))) lon)))

(define (product-sqrts-plus-one/fold lon)
  (foldr
   (λ (n p) (* p (add1 (sqrt n))))
  1
  lon))

(define (product-sqrts-plus-one/local lon)
  (local [; mult-transform : Number Number -> Number
          ; transforms the element from the list (first
          ; argument) and multiplies by the exisitng
          ; product (second arguement)
   (add1 (sqrt n))
  1
  lon]))




; Design a function num-perfect-squares that accepts a list of numbers
; and counts those numbers that are perfect squares.
; Try to come up with one approach that uses filter, the other that
; uses foldr.

; (Hint: a number is a perfect square if its square root is an integer.)

; num-perfect-squares : [List-of Number] -> NatNum
; Returns the count of numbers are a perfect square


(check-expect (num-perfect-squares '()) 0)
(check-expect (num-perfect-squares (list 3 4 18)) 1)
(check-expect (num-perfect-squares (list 3 10 -1 18)) 0)
(check-expect (num-perfect-squares (list 4 9 16)) 3)

(define (num-perfect-squares/filter lon)
  (length (filter (λ (n) (integer? (sqrt n))) lon)))

#|
(define (num-perfect-squares/fold lon)
  (foldr
   (λ (n ct)
     (if (integer? (sqrt n)
                   add1 ct
                   ct)
         0
         lon))))

|#



; Implement all of the following using only foldr


(check-expect (mymap add1 '()) '())
(check-expect (mymap add1 (list 1 2 3 4)) (list 2 3 4 5))
(check-expect (mymap sub1 (list 1 2 3 4)) (list 0 1 2 3))

(define (mymap f lox)
  (foldr
   (λ (x old-result-list) (cons (f x) old-result-list))
   '()
   lox))


(check-expect (myfilter even? '()) '())
(check-expect (myfilter even? (list 1 2 3 4)) (list 2 4))
(check-expect (myfilter odd? (list 1 2 3 4)) (list 1 3))

(define (myfilter p? lox)
  (foldr
   (λ (x old-filtered-list)
     (if (p? x)
         (cons x old-filtered-list)
         olf-filtered-list))
   '()
   lox))


(check-expect (myandmap even? '()) #true)
(check-expect (myandmap even? (list 1 2 3 4)) #false)
(check-expect (myandmap even? (list 2 4 6 8)) #true)

(define (myandmap p? lox)
  (foldr
   (λ (x old-boolean) (and (p? x) old-boolean))
   '()
   lox))


(check-expect (myormap even? '()) #false)
(check-expect (myormap even? (list 1 3 5 7)) #false)
(check-expect (myormap even? (list 1 3 50 7)) #true)

(define (myormap p? lox)
  (foldr
   (λ (x old-boolean) (or (p? x) old-boolean))
   #false
   lox))

