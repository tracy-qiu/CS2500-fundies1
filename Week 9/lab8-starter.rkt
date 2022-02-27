;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab8-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 11/2/21
; Week 9 Design with File I/O, Multiple Complex Inputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 8

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

(define (f x y z) 
  (local [(define x (+ y z))]
    (local [(define y z)]
      (local [(define z (* x y))]
        (+ x y z)))))
 
(define (g a b) 
  (+ (local [(define b (string->number a))]
       (local [(define a (sqr b))]
         (- a b)))
     (string-length a)
     (if b 10 5))) 

;;; TODO #1: Uncomment and complete the following check-expects
;;; by replacing the ???'s. Run them to double-check your answers.
;;; The goal here is for you to predict how the nested local definitions
;;; work, and then confirm your understanding with these tests.

(check-expect (f 3 2 1) 7)
(check-expect (g "5" #true) 31)
(check-expect (g "-5" #false) 37)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;;; TODO #2: design the function adder that, given a Number, produces
;;; a function that adds that Number to a given Number. You must produce
;;; two versions: one that uses local, the other that uses lambda (λ).
;;; You are welcome to provide signatures/purpose for each version
;;; individually, or one for the pair (since none of this should change,
;;; aside from the actual function names). Some check-expects have been
;;; provided for you below:


(check-expect ((adder-local 3) 4) 7)
(check-expect ((adder-local 10) 2) 12)

(check-expect ((adder-lambda 3) 4) 7)
(check-expect ((adder-lambda 10) 2) 12)

; adder-local : Number -> Function
; given a number, produces a function that adds that number to a given number

(define (adder-local n1)
  (local [(define (f n2)
            (+ n1 n2))]
    f))

; adder-lambda : Number -> Function
; given a number, produces a function that adds that number to a given number

(define (adder-lambda n1)
  (λ (n2) (+ n1 n2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;;; Recall the following definition of a 2d position:

;;; A Position is a (make-posn Real Real)
;;; Interpretation: an (x, y) position

(define POSN-0 (make-posn 50 50))
(define POSN-1 (make-posn 5 10))

(define (posn-temp p)
  (... (posn-x p) ...
       (posn-y p) ...))


;;; TODO #3: design the function point-mover that accepts two
;;; real values (representing the change in x and y, respectively)
;;; and produces a function that will always move a supplied point
;;; by those values. For clarity, some tests have been supplied,
;;; which, because of the definitions, should be placed *after*
;;; your definition of point-mover.

; point-mover : Number Number -> Function
; given two numbers, produces a function that moves a supplied point by those x y values

(define (point-mover x y) 
  (λ (p) (make-posn (+ x (posn-x p)) (+ y (posn-y p)))))

(define move-right (point-mover 1 0))  
(define move-down (point-mover 0 1))

(check-expect (move-right POSN-0) (make-posn 51 50))
(check-expect (move-down POSN-0) (make-posn 50 51))
(check-expect ((point-mover 2.5 -7) POSN-1) (make-posn 7.5 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;;; TODO #4: design the function elbow-mover that accepts four
;;; real values (a first set of change in x/y, and a second set),
;;; and produces a function that takes in a time step (represented
;;; by a natural number) and a point, which then moves the point
;;; based upon time in a cyclic fashion: for the first two time
;;; steps (0, 1) the point is moved using the first set of x/y
;;; values, for the third time step (2) the point is moved using
;;; the second set of x/y values, and then repeat onwards.
;;; For clarity, some tests have been supplied, which, because of
;;; the definitions, should be placed *after* your definition of
;;; elbow-mover.
;;;
;;; Hint: if you recall from early in the semester, the remainder
;;; function is a great way to create repeated cycles over time...
;;; (remainder 0 3) -> 0
;;; (remainder 1 3) -> 1
;;; (remainder 2 3) -> 2
;;; (remainder 3 3) -> 0
;;; ...

; elbow-mover : Number Number Number Number -> Function
; given four numbers, produces a function that takes in time step and a point,
; which then moves the point based on time in a cyclic fashion

(define (elbow-mover x1 y1 x2 y2)
  (λ (timestep p)
    (if (= (remainder timestep 3) 2) 
        (make-posn (+ x2 (posn-x p)) (+ y2 (posn-y p))) 
        (make-posn (+ x1 (posn-x p)) (+ y1 (posn-y p))))))

(define right-up (elbow-mover 1 0 0 -1))
(define down-left (elbow-mover 0 1 -1 0))

(check-expect (right-up 0 POSN-0) (make-posn 51 50))
(check-expect (right-up 1 POSN-0) (make-posn 51 50))
(check-expect (right-up 2 POSN-0) (make-posn 50 49))
(check-expect (right-up 3 POSN-0) (make-posn 51 50))
(check-expect (right-up 4 POSN-0) (make-posn 51 50))
(check-expect (right-up 5 POSN-0) (make-posn 50 49))

(check-expect (down-left 0 POSN-0) (make-posn 50 51))
(check-expect (down-left 1 POSN-0) (make-posn 50 51))
(check-expect (down-left 2 POSN-0) (make-posn 49 50))
(check-expect (down-left 3 POSN-0) (make-posn 50 51))
(check-expect (down-left 4 POSN-0) (make-posn 50 51))
(check-expect (down-left 5 POSN-0) (make-posn 49 50))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For a change you don't actually have TODO anything below :)
;;;
;;; Instead, once you finish all the parts above, try uncommenting
;;; what you find below and running the following code...
;;;
;;; (go-points LOMP-1)
;;;
;;; The goal is to animate a list of points starting from various
;;; points moving according to various functions (like elbow-mover
;;; that you made above).
;;;
;;; Your only task is to make sure you understand all the code
;;; below. It makes heavy use of local/lambda, as well as list
;;; abstractions, and so is a great application of those concepts.
;;;
;;; If you have the time, try to implement your own version from
;;; scratch as a review problem :)
;;;
;;; (Note: in certain cases below we sacrificed readability for
;;; showcasing lambda - if you reimplement, you might make some
;;; better design choices as to local/lambda vs helper functions.)
;;;


(define-struct mp [p f-next])
;;; A MovingPoint (MP) a (make-mp Position [Nat Position -> Position])
;;; Interpretation: a position and a function to update that position
;;; given the current time

(define always-right (λ (t p) (move-right p)))
(define always-down (λ (t p) (move-down p)))

(define MP-CENTER-RIGHT (make-mp POSN-0 always-right))
(define MP-OTHER-DOWN (make-mp POSN-1 always-down))
(define MP-CENTER-DIAG-UP (make-mp POSN-0 right-up))
(define MP-CENTER-DIAG-DOWN (make-mp POSN-0 down-left))

(define (mp-temp mp)
  (... (posn-temp (mp-p mp)) ...
       (mp-f-next mp) ...))


(define-struct mpw [time points])

; A MovingPointWorld (MPW) is a (make-mpw Nat [List-of MovingPoint])
; Interpretation: the current time and a list of points,
; with their associated move functions

(define LOMP-1
  (list MP-CENTER-RIGHT
        MP-OTHER-DOWN
        MP-CENTER-DIAG-UP
        MP-CENTER-DIAG-DOWN))

(define MPW-START
  (make-mpw 0 LOMP-1))


(define LOMP-DONE
  (list (make-mp (make-posn 200 50) always-right)
        (make-mp (make-posn 90 -30) always-down)))

(define MPW-DONE
  (make-mpw 100 LOMP-DONE))


(define (mpw-temp mpw)
  (... (mpw-time mpw) ...
       (lomp-temp (mpw-points mpw)) ...))

;;; go-points : [List-of MovingPoint] -> [List-of Position]
;;; starts time at 0 and plots the points changing over
;;; time (based on their movement functions); stops when
;;; all points are off-screen and produces their final positions

(define (go-points lomp)
  (map mp-p
       (mpw-points
        (big-bang (make-mpw 0 lomp)
          [to-draw draw-mpw]
          [on-tick tick-mpw]
          [stop-when mpw-offscreen?]))))


; draw-mpw : MPW -> Image
; plots the current point locations on a common background

(define SIZE 100)
(define BG (square SIZE "solid" "white"))

(define RADIUS 2)
(define PT (circle RADIUS "solid" "black"))

(check-expect
 (draw-mpw MPW-START)
 (place-image
  PT
  50 50
  (place-image
   PT
   5 10
   (place-image
    PT
    50 50
    (place-image
     PT
     50 50
     BG)))))

(define (draw-mpw mpw)
  (foldr
   (λ (p scene)
     (place-image
      PT
      (posn-x p) (posn-y p)
      scene))
   BG
   (map mp-p
        (mpw-points mpw))))

;;; tick-mpw : MPW -> MPW
;;; increments time by one and moves each point according to
;;; its own movement function

(define (tick-mpw mpw)
  (local [(define NOW (mpw-time mpw))]
    (make-mpw
     (add1 NOW)
     (map
      (λ (mp)
        (make-mp
         ((mp-f-next mp) NOW (mp-p mp))
         (mp-f-next mp)))
      (mpw-points mpw)))))

(define MPW-NEXT (tick-mpw MPW-START))

(check-expect
 (mpw-time MPW-NEXT)
 1)

(check-expect
 (map mp-p (mpw-points MPW-NEXT))
 (list (make-posn 51 50)
       (make-posn 5 11)
       (make-posn 51 50)
       (make-posn 50 51)))
   

;;; mpw-offscreen? : MPW -> Boolean
;;; determines if all the points are off the screen in
;;; some dimension

(check-expect (mpw-offscreen? MPW-DONE) #true)
(check-expect (mpw-offscreen? MPW-START) #false)

(define (mpw-offscreen? mpw)
  (andmap
   (λ (p)
     (or
      (< (posn-x p) (- RADIUS)) (> (posn-x p) (+ SIZE RADIUS))
      (< (posn-y p) (- RADIUS)) (> (posn-y p) (+ SIZE RADIUS))))
   (map mp-p
        (mpw-points mpw))))


