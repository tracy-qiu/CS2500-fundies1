;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-twomoons0929) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Design a program that can simulate two moons moving across
; the pink Martian sky (with the sun in the middle).
; Each moon is the same size, but moves at a different velocity.


(define-struct moon [x vx])
 
; A Moon is a (make-moon Number Number)
; Representing the position and velocity of a moon.
;  - x is the x-position of the moon
;  - vx is the x-velocity of the moon
 
(define MOON-1 (make-moon 10 1))
(define MOON-2 (make-moon 300 -1))
 
(define (moon-temp m)
  (... (moon-x m) ...
       (moon-vx m) ...))
 
 
(define-struct twomoons [m1 m2])
 
; A TwoMoons is a (make-twomoons Moon Moon)
; Representing the two moons we're drawing.
;  - m1 is the first moon
;  - m2 is the second moon
 
(define TWOMOONS-1 (make-twomoons MOON-1 MOON-2))
 
(define (twomoons-temp tm)
  (... (moon-temp (twomoons-m1 tm))  ...
       (moon-temp (twomoons-m2 tm)) ...))

(define-struct system [moon others])

; A System is one of:
; - ":("
; - (make-system Moon System)
; Interpretation: a system of moons

(define SYSTEM-0 ":(")
(define SYSTEM-1 (make-system MOON-2 SYSTEM-0))
(define SYSTEM-2 (make-system MOON-1 SYSTEM-1))
(define SYSTEM-3 (make-system (make-moon 1 20) SYSTEM-2))

(define (system-temp s)
  (...
   (cond
     [(string? s) ...]
     [(system? s) ...
      (moon-temp (system-moon s)) ...
      (system-temp (system-others s)) ...])))

; TODO: design the data definition System that will
; support an arbitrary number of moons

; eclipse : System -> System
; Runs an eclipse of two moons
 
(define (eclipse initial-tm)
  (big-bang initial-tm
    [to-draw draw-eclipse]
    [on-tick move-eclipse]))


; draw-eclipse : TwoMoons -> Image
; Draws the moons

(define SIZE 400)
(define HALF (/ SIZE 2))
 
(define SUN (circle (/ SIZE 10) "solid" "yellow"))
(define MOON (circle (/ SIZE 10) "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))

(check-expect (draw-eclipse SYSTEM-0) SYSTEM-0)

(check-expect (draw-eclipse SYSTEM-2)
              (place-image
               MOON
               10 HALF
               (place-image
                MOON
                300 HALF
                (place-image
                 SUN
                 HALF HALF
                 SKY))))

(define (draw-eclipse s)
  (cond
    [(string? s)
     (place-image
      SUN
      HALF HALF
      SKY)]
    [(system? s) 
     (draw-moon (system-moon s) 
                (draw-eclipse (system-others s)))]))  
     

; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  (place-image MOON 
               (moon-x moon) HALF
               background))


; move-eclipse : System -> System
; Moves two moons for one tick

(check-expect (move-eclipse SYSTEM-0) SYSTEM-0)

(check-expect (move-eclipse SYSTEM-2)
              (make-system (make-moon 11 1)
                             (make-system( make-moon 299 -1)
                                         SYSTEM-0)))

(define (move-eclipse s)
   (cond
     [(string? s) s]
     [(system? s)
      (make-system
       (move-moon (system-moon s))
       (move-eclipse (system-others s)))]))


; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))

; add-moon : System KeyEvent -> System
; adds a moon to the system

(define NEW-MOON-X 1)
(define NEW-MOON-VX 20)

(check-expect (add-moon SYSTEM-0 "a")
              (make-system (make-moon NEW-MOON-X NEW-MOON-VX)
                           SYSTEM-0))

(check-expect (add-moon SYSTEM-1 "a")
              (make-system (make-moon NEW-MOON-X NEW-MOON-VX)
                           SYSTEM-0))


(define (add-moon s ke)
   (make-system (make-moon NEW-MOON-X NEW-MOON-VX) s))


#|

; eclipse : TwoMoons -> TwoMoons
; Runs an eclipse of two moons
 
(define (eclipse initial-tm)
  (big-bang initial-tm
    [to-draw draw-eclipse]
    [on-tick move-eclipse]))


; draw-eclipse : TwoMoons -> Image
; Draws the moons

(define SIZE 400)
(define HALF (/ SIZE 2))
 
(define SUN (circle (/ SIZE 10) "solid" "yellow"))
(define MOON (circle (/ SIZE 10) "solid" "gray"))
(define SKY (square SIZE "solid" "pink"))

(check-expect (draw-eclipse TWOMOONS-1)
              (place-image
               MOON
               10 HALF
               (place-image
                MOON
                300 HALF
                (place-image
                 SUN
                 HALF HALF
                 SKY))))

(define (draw-eclipse tm)
  (draw-moon (twomoons-m1 tm)
       (draw-moon (twomoons-m2 tm)
                  (place-image
                   SUN
                   HALF HALF
                   SKY))))

; draw-moon : Moon Image -> Image
; Draws a moon onto a background
 
(check-expect (draw-moon MOON-1 SKY)
              (place-image MOON (moon-x MOON-1) HALF SKY))
 
(define (draw-moon moon background)
  (place-image MOON
               (moon-x moon) HALF
               background))


; move-eclipse : TwoMoons -> TwoMoons
; Moves two moons for one tick

(check-expect (move-eclipse TWOMOONS-1)
              (make-twomoons (make-moon 11 1)
                             (make-moon 299 -1)))

(define (move-eclipse tm)
    (make-twomoons (move-moon (twomoons-m1 tm))
                   (move-moon (twomoons-m2 tm)))) 

; move-moon : Moon -> Moon
; Moves a single moon
 
(check-expect (move-moon MOON-1)
              (make-moon 11 1))
 
(check-expect (move-moon MOON-2)
              (make-moon 299 -1))
 
(define (move-moon m)
  (make-moon (+ (moon-x m) (moon-vx m))
             (moon-vx m)))

|# 
