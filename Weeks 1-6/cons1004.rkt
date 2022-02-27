;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cons1004) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Flight is one of:
; - #flase
; - (make-flight Passenger Flight)
; Interpretation: the passengers on a flight
; - passenger is the current passenger
; - flight is the remainder of the passengers
; or a flight with no passengers (#false)

; A System is one of:
; - ":("
; - (make-system Moon System)
; Interpretation: the collection of moons we're trying
; to simulate (or ":(" for an empty system)

; A ListofNumbers (LoN) is one of:
; -'()
; - (cons Number LoN)
; Interpretation: a list of numbers


(define (lon-temp lon)
  (...
   (cond
     [(empty? lon) ...]
     [(cons? lon) 
      (first lon) ...
      (lon-temp (rest lon)) ...])))


; stopping point: '()
; are you the stopping point?: empty?
; thing we are creating (cons data rest-of-list)
; are you the thing to create things?: cons?
; reference current thing: (first (cons data rest-of-list)) -> data 
; reference to prior things: (rest (cons data rest-of-list)) -> rest-of-list 
; idea of a list?: list?

(define MYLIST (cons 5 '()))

(cons? MYLIST)
(empty? MYLIST)
(list? MYLIST)
(cons? (first MYLIST))
(empty? (first MYLIST))
(list? (first MYLIST))
(cons? (rest MYLIST))
(empty? (rest MYLIST))
(list? (rest MYLIST))


(check-expect (sum-list '()) 0)
(check-expect (sum-list (cons 5 '())) 5)
(check-expect (sum-list (cons -3 (cons 5 '()))) 2)


(define (sum-list lon)
   (cond
     [(empty? lon) 0]
     [(cons? lon) 
      (+ (first lon)
         (sum-list (rest lon)))]))
