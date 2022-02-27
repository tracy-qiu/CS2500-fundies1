;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8-twocars0927) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct twocars [x1 vx1 x2 vx2])
 
; A TwoCars is a (make-twocars Number Number Number Number)
; Interpretation: Two cars racing towards each other
;  - x1 is the x-position of the first car in pixels from the left
;  - vx1 is the x-velocity of the first car in pixels/tick (driving to the right)
;  - x2 is the x-position of the second car in pixels from the left
;  - vx2 is the x-velocity of the second car in pixels/tick (driving to the left)
 
(define TWOCARS-1 (make-twocars 1 2 300 4))
(define TWOCARS-2 (make-twocars 8 2 12 4))
 
(define (twocars-temp tc)
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...))

; Design the program collision that simulates two cars
; accelerating towards each other and colliding.

(define ACCEL 0.1)

(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))
(define CAR-WRECK (rectangle 
(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))




; draw-twocars : TwoCars -> Image
; Draws each of the cars

(define (draw-cs cs)
  (cond
    [(twocars? cs 





; move-twocars : TwoCars -> TwoCars
; Moves each of the cars

(check-expect (move-twocars TWOCARS-1)
              (make-twocars 3 2.1
                            296 4.1))

(define (move-twocars tc)
  (make-twocars (+ (twocars-x1 tc) (twocars-vx1 tc))
                (+ (twocars-vx1 tc) ACCEL)
                (+ (twocars-x2 tc) (twocars-vx1 tc))
                (+ (twocars-vx2 tc) ACCEL)))
#|
; collision: CS -> CS
; simulate a car crash

(define (collision initial-cs)
  (big-bang intiial-cs
    [to-draw draw-cs]
    [on-tick move-cs]))
|#

; collision: CS -> Boolean
; simulate a car crash, return #true if a crash occurred 

(define (collision initial-cs)
  (wreck? (big-bang intiial-cs
            [to-draw draw-cs]
            [on-tick move-cs])))

; did-a-crash-occur? : CS -> Boolean

(define (did-a-crash-occur cs

; draw-cs : CS -> Image
l visualizes the CS



; A CollisionSim (CS) is one of:
; - TwoCars
; - Wreck
; Interpretation: either two cars racing towards each ohter,
; or the combined car after they've hit

(define CS-1 TWOCARS-1)
(define CS-2 WRECK-1)

(define (cs-temp cs)
  (...
   (cond
     [(twocars? cs) (twocars-temp cs)



                
              
