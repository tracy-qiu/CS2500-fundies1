;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname eclipse0913) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SKY-WIDTH 300)
(define SKY-HEIGHT 200)
(define RADIUS 25)

(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))

#|
9/13/21

(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue")) 
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "dark blue")) 

; draw-eclipse : Number -> Image
; given a horizontal location of the moon
; draws it on top of the sun in the sky

(define (draw-eclipse x-moon)
  (place-image
   MOON
   x-moon (/ SKY-HEIGHT 2)
   (place-image
    SUN
    (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
    SKY)))

;(animate draw-eclipse)


(define (draw-eclipse-v2 x-moon)
  (place-image
   MOON
   x-moon (/ SKY-HEIGHT 2)
   (place-image
    SUN
    (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
    SKY)
   (if (= x-moon (/ SKY-HEIGHT 2))
     DARKSKY    
     SKY)))
;(animate draw-eclipse-v2)


(define (draw-eclipse-v3 x-moon)
  (place-image
   MOON
   x-moon (/ SKY-HEIGHT 2)
   (place-image
    SUN
    (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
    (cond
      [(< (abs (- x-moon (/ SKY-WIDTH 2))) (* 2 RADIUS)) DIMSKY]
      [else SKY]))))
(animate draw-eclipse-v3)
|#


; 9/16/21

; eclipse : Real -> Real
; simulates the moon moving in the sky

(define (eclipse initial-moon-x)
  (big-bang initial-moon-x
    [to-draw draw-eclipse]
    [on-tick move-moon]
    [on-key restart-moon]))

; draw-eclipse : Number -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun 

(check-expect
 (draw-eclipse 20)
 (place-image
  MOON
  20 (/ SKY-HEIGHT 2)
  (overlay SUN SKY)))

(check-expect
 (draw-eclipse 20)
 (place-image
  MOON
  20 (/ SKY-HEIGHT 2)
  (overlay SUN SKY)))

(define (draw-eclipse x-moon)
  (place-image
   MOON
   x-moon (/ SKY-HEIGHT 2)
   (overlay SUN SKY)))

; move-moon : Real -> Real
; moves the moon after one tick

(check-expect (move-moon 0) 5)
(check-expect (move-moon 10) 15)

(define (move-moon old-moon-x)
  ; (add1 old-moon-x) changed from add one to add five per tick
  (+ 5 old-moon-x))

; restart-moon : Real KeyEvent -> Real
; moves the moon to the left of the screen

(check-expect (restart-moon 55 "a") (- RADIUS))
(check-expect (restart-moon 120 " ") (- RADIUS))

(define (restart-moon old-moon-x ke)
  (- RADIUS))

#|
(define (restart-moon old-moon-x ke)
  (if (key=? ke "a")
  (- RADIUS)
  old-moon-x))
|#


; TODO #1: repalce with big-bang
; TODO #2: make the moon faster
; TODO #3: enable restart 



    
