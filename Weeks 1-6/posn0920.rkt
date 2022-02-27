;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname posn0920) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; v: make-posn
; s: (make-posn vlaue-x value-y)
; sem: (constructor) made a posn that has an x and y value

(define MYPOSN (make-posn 3 4))

; v: posn-x posn-y (selector) 
; s: (posn-x (make-posn value-x value-y))
; sem: -> value-x 

; v: posn? (predicate)
; s: (posn? ???) ->
; sem: returns #true if the supplied value is a make-posn

; A Position is a (make-posn Real Real)
; - x: is the x-coordinate 
; - y: is the y-coordinate
; Interpretation: a 2d position in space

(define POSITION-ORIGIN (make-posn 0 0))
(define POSITION-RANDOM (make-posn 50 100))
(define POSITION-NEG (make-posn -50 100))
(define POSITION-NEG2 (make-posn 0 -10))

; template
; position-temp : Position -> ???
(define (position-temp p)
  (...(posn-x p)...
      (posn-y p)...))


; make-posn : Real Real -> Position 
; posn-x : Position -> Real
; posn-y : Position -> Real
; posn? : Any -> Boolean



; Design the function x-greater-than-y? that accepts a Position
; and returns whether or not the x-coordinate is greater than
; the y-coordinate

; x-greater-than-y? : Position -> Boolean
; determines if the x-coordinate is greater than the y-coordinate

(check-expect (x-greater-than-y? POSITION-ORIGIN) #false)
(check-expect (x-greater-than-y? POSITION-RANDOM) #false)
(check-expect (x-greater-than-y? POSITION-NEG) #false)
(check-expect (x-greater-than-y? POSITION-NEG2) #true)

(define (x-greater-than-y? p)
   (> (posn-x p) (posn-y p)))



; Design the function add-10-to-x that accepts a Position and
; returns a Position with the same y-coordinate and an x-coordinate
; that is 10 greater

; add-10-to-x : Position -> Position
; creates a position adding 10 to the x-coordinate of the supplied position 

(check-expect (add-10-to-x POSITION-ORIGIN)
              (make-posn 10 0))
(check-expect (add-10-to-x POSITION-RANDOM)
              (make-posn 60 100))
(check-expect (add-10-to-x POSITION-NEG)
              (make-posn -40 100))
(check-expect (add-10-to-x POSITION-NEG2)
              (make-posn 10 -10))

(define (add-10-to-x p)
  (make-posn (+ (posn-x p)10)
      (posn-y p)))