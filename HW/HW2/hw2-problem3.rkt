;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; Part A

;; Write a data definition called CompassDirection that represents a single
;; cardinal or inter-cardinal direction that appears on a compass rose:
;;
;; https://en.wikipedia.org/wiki/Cardinal_direction
;;
;; Ensure you follow all steps of the data design recipe.

;; [TODO] Data design recipe

; A CompassDirection is one of:
; - "North"
; - "East"
; - "South"
; - "West"
; - "Northeast"
; - "Southeast"
; - "Southwest"
; - "Northwest"
; Interpretation: single cardinal or inter-cardinal directions

(define CD-NORTH "North")
(define CD-EAST "East")
(define CD-SOUTH "South")
(define CD-WEST "West")
(define CD-NORTHEAST "Northeast")
(define CD-SOUTHEAST "Southeast")
(define CD-SOUTHWEST "Southwest")
(define CD-NORTHWEST "Northwest")

(define (cd-temp cd)
  (cond
    [(string=? cd CD-NORTH) ...]
    [(string=? cd CD-EAST) ...]
    [(string=? cd CD-SOUTH) ...]
    [(string=? cd CD-WEST) ...]
    [(string=? cd CD-NORTHEAST) ...]
    [(string=? cd CD-SOUTHEAST) ...]
    [(string=? cd CD-SOUTHWEST) ...]
    [(string=? cd CD-NORTHWEST) ...]))
     
     

;; Part B

;; Write a predicate to determine if a CompassDirection is a cardinal
;; direction (and not an inter-cardinal direction).

;; [TODO] Function design recipe

; cardinal-direction? : CompassDirection -> Boolean
; determines if the given compass direction is a cardinal direction

(check-expect (cardinal-direction? CD-NORTH) #true)
(check-expect (cardinal-direction? CD-EAST) #true)
(check-expect (cardinal-direction? CD-SOUTH) #true)
(check-expect (cardinal-direction? CD-WEST) #true)
(check-expect (cardinal-direction? CD-NORTHEAST) #false)
(check-expect (cardinal-direction? CD-SOUTHEAST) #false)
(check-expect (cardinal-direction? CD-SOUTHWEST) #false)
(check-expect (cardinal-direction? CD-NORTHWEST) #false)

(define (cardinal-direction? cd)
  (cond
    [(string=? cd CD-NORTH) #true]
    [(string=? cd CD-EAST) #true]
    [(string=? cd CD-SOUTH) #true]
    [(string=? cd CD-WEST) #true]
    [(string=? cd CD-NORTHEAST) #false]
    [(string=? cd CD-SOUTHEAST) #false]
    [(string=? cd CD-SOUTHWEST) #false]
    [(string=? cd CD-NORTHWEST) #false])) 


;; Part C

;; Write a function that consumes a CompassDirection and produces the opposite
;; direction.

;; [TODO]

; opposite-direction : CompassDirection -> CompassDirection
; returns the opposite direction of the given compass direction

(check-expect (opposite-direction CD-NORTH) CD-SOUTH)
(check-expect (opposite-direction CD-EAST) CD-WEST)
(check-expect (opposite-direction CD-SOUTH) CD-NORTH)
(check-expect (opposite-direction CD-WEST) CD-EAST)
(check-expect (opposite-direction CD-NORTHEAST) CD-SOUTHWEST)
(check-expect (opposite-direction CD-SOUTHEAST) CD-NORTHWEST)
(check-expect (opposite-direction CD-SOUTHWEST) CD-NORTHEAST)
(check-expect (opposite-direction CD-NORTHWEST) CD-SOUTHEAST)

(define (opposite-direction cd)
  (cond
    [(string=? cd CD-NORTH) CD-SOUTH]
    [(string=? cd CD-EAST) CD-WEST]
    [(string=? cd CD-SOUTH) CD-NORTH]
    [(string=? cd CD-WEST) CD-EAST]
    [(string=? cd CD-NORTHEAST) CD-SOUTHWEST]
    [(string=? cd CD-SOUTHEAST) CD-NORTHWEST]
    [(string=? cd CD-SOUTHWEST) CD-NORTHEAST]
    [(string=? cd CD-NORTHWEST) CD-SOUTHEAST])) 






