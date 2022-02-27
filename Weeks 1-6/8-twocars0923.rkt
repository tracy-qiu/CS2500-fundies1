;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8-twocars0923) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Design the program collision that simulates two cars
; accelerating towards each other and colliding.

(define ACCEL 0.1)

(define CAR-1 (rectangle 10 5 "solid" "blue"))
(define CAR-2 (rectangle 10 5 "solid" "red"))
(define Y-CAR 50)
(define BACKGROUND (empty-scene 600 400))


(define-struct twocars [x1 vx1 x2 vx2])
 
; A TwoCars is a (make-twocars Number Number Number Number)
; Interpretation: Two cars racing towards each other
;  - x1 is the x-position of the first car in pixels from the left
;  - vx1 is the x-velocity of the first car in pixels/tick (driving to the right)
;  - x2 is the x-position of the second car in pixels from the left
;  - vx2 is the x-velocity of the second car in pixels/tick (driving to the left)
 
(define TWOCARS-1 (make-twocars 1 2 300 4))
 
(define (twocars-temp tc)
  (... (twocars-x1 tc) ... (twocars-vx1 tc) ...
       (twocars-x2 tc) ... (twocars-vx2 tc) ...))

; collision : Real Real Real Real -> TwoCars
; simulates two cars driving at each other

(define (collision x1 vx1 x2 vx2)
  (big-bang (make-twocars x1 vx1 x2 vx2)
    [to-draw draw-twocars]
    [on-tick move-twocars]))

; draw-twocars : TwoCars -> Image

; move-twocars : TwoCars -> TwoCars

(check-expect (move-twocars TWOCARS-1)
              (make-twocars 3 2.1 296 4.1))
