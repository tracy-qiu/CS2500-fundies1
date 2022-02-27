;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; TODO: Design the function string-starts-with? which takes two
;; Strings and returns a Boolean indicating whether the first
;; string begins with the second. Be sure to follow all the steps
;; of the Design Recipe for functions.

;; When you are testing your function, make sure you test the case
;; where the first string is shorter than the second. For example
;; (string-starts-with? "fundies" "fun") should return #true but
;; (string-starts-with? "fun" "fundies") should return #false.


; string-starts-with? : String String -> Boolean
; checks if first string begins with second


(check-expect (string-starts-with? "fundies" "fun") #true)
(check-expect (string-starts-with? "fun" "fundies") #false)


(define (string-starts-with? string1 string2)
  (cond
   [(> (string-length string2) (string-length string1)) #false]
   [(= (substring string1 0 (string-length string2)) string2) #true]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; TODO: Design the function either-true? that takes two
;; Boolean parameters and returns true if either (or both)
;; of the parameters are true.

;; You must adhere to the following restrictions:
;;
;; - you are only allowed to use if, the names of the
;;   parameters, #true, and #false (though you may not
;;   need all of these);
;;
;; - you are not allowed to use an if that takes
;;   the following form (if parameter #true #false),
;;   since this is the same as the value of parameter;
;;
;; - the tests for your function should cover ALL possible
;;   input combinations for the parameters.
;;
;; And don't forget (for the rest of the class!), "designing" a function
;; means to produce all 4 parts of the Design Recipe for functions!

; either-true? : Boolean Boolean -> Boolean
; takes two Boolean parameters, returns true if either are true

(check-expect (either-true? (> 5 3) (= (string-length "hello") 5)) #true)  ; both true 
(check-expect (either-true? (< 14 9) (= 3 3)) #true)                       ; one true
(check-expect (either-true? (> 5 10) (= 8 3)) #false)                      ; both false

(define (either-true? bool1 bool2)
  (or bool1 bool2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; You are to design a small door-simulator program...
;;
;; - A door can either be open, closed, or locked. Your program
;;   will take in a representation of one of these states.
;;
;; - The user can open a closed door by pressing the "o"
;;   key on their keyboard. You cannot open a locked door,
;;   and attempting to open an already open door will do nothing.
;;
;; - The user can close an open door by pressing the "c" key
;;   on their keyboard. Attempting to close an already closed
;;   (or closed and locked) door will do nothing.
;;
;; - The user can lock a closed door by pressing the "l" key
;;   on their keyboard. Attempting to lock an open door or an
;;   already locked door will do nothing.
;;
;; - The user can unlock a locked door by pressing the "u" key
;;   on their keyboard. Attempting to unlock a closed door that
;;   is already unlocked, or an open door, will do nothing.


;; A DoorState is one of:
;; - "closed"
;; - "locked"
;; - "open"
;; Interpretation: state of a lockable door

;; TODO #1: finish the Design Recipe for data for DoorState
;; (so provide examples and a template)

(define DOORSTATE-CLOSED "closed")
(define DOORSTATE-LOCKED "locked")
(define DOORSTATE-OPEN "open")

(define (ds-state ds)
  (...
   (cond
     [(string=? ds DOORSTATE-CLOSED)...]
     [(string=? ds DOORSTATE-LOCKED)...]
     [(string=? ds DOORSTATE-OPEN)...])))

;; door-simulator : DoorState -> DoorState
;; Simulates a lockable door

;; TODO #2: write the door-simulator as a function that calls
;; big-bang; in addition to to-draw, what handler(s) will you
;; need for the description above?

(define (door-simulator start-state)
  (big-bang start-state
    [to-draw draw-door]
    [on-key change-door]))

;; TODO #3: design all the handlers in the "wish list" you
;; just generated (via your big-bang event handlers). To help
;; we've provided some examples of visualizations of the states
;; of the door (you can use these or make your own).

(define BG (rectangle 400 200 "solid" "blue"))

(define DOOR-W (/ (image-width BG) 5))
(define DOOR-H (- (image-height BG) 40))

(define KNOB-X (* .8 DOOR-W))
(define KNOB-Y (/ DOOR-H 2))

(define DOOR
  (place-image
   (circle (/ DOOR-W 10) "solid" "gray")
   KNOB-X KNOB-Y
   (rectangle DOOR-W DOOR-H "solid" "brown")))

(define DOOR-LOCK
  (place-image
   (text "x" 10 "black")
   KNOB-X KNOB-Y
   DOOR))

(define DOOR-X (* 0.6 (image-width BG)))
(define DOOR-Y (+ (/ DOOR-H 2) (- (image-height BG) DOOR-H)))

(define DOOR-CLOSED
  (place-image
   DOOR
   DOOR-X DOOR-Y
   BG))

(define DOOR-LOCKED
  (place-image
   DOOR-LOCK
   DOOR-X DOOR-Y
   BG))

(define DOOR-OPEN
  (place-image
   (beside (flip-horizontal DOOR)
           (rectangle DOOR-W DOOR-H "solid" "lightblue"))
   (- DOOR-X (/ DOOR-W 2)) DOOR-Y
   BG))

; draw-door : DoorState -> Image
; draws the light

(check-expect (draw-door DOORSTATE-CLOSED) DOOR-CLOSED)
(check-expect (draw-door DOORSTATE-LOCKED) DOOR-LOCKED)
(check-expect (draw-door DOORSTATE-OPEN) DOOR-OPEN)


(define (draw-door ds)
   (cond
     [(string=? ds DOORSTATE-CLOSED)DOOR-CLOSED]
     [(string=? ds DOORSTATE-LOCKED)DOOR-LOCKED]
     [(string=? ds DOORSTATE-OPEN)DOOR-OPEN]))

; change-door : DoorState Real KeyEvent -> DoorState
; changes the state of the door from on-key input

(check-expect (change-door DOORSTATE-CLOSED "o") DOOR-OPEN)
;(check-expect (change-door DOORSTATE-LOCKED "o") DOOR-LOCKED)
(check-expect (change-door DOORSTATE-OPEN "o") DOOR-OPEN)
(check-expect (change-door DOORSTATE-CLOSED "c") DOOR-CLOSED)
;(check-expect (change-door DOORSTATE-LOCKED "c") DOOR-LOCKED)
(check-expect (change-door DOORSTATE-OPEN "c") DOOR-CLOSED)
(check-expect (change-door DOORSTATE-CLOSED "l") DOOR-LOCKED)
(check-expect (change-door DOORSTATE-LOCKED "l") DOOR-LOCKED)
;(check-expect (change-door DOORSTATE-OPEN "l") DOOR-OPEN)

(define (change-door old-door-state ds-key)
   (cond
     [(key=? ds-key "o") DOOR-OPEN]
     [(key=? ds-key "c") DOOR-CLOSED]
     [(key=? ds-key "l") DOOR-LOCKED]))

