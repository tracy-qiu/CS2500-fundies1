;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exam1-problem8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 8
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; Consider the following data definition and interpretation:

(define-struct star [name mass])
(define-struct planet [name mass closer])
;; A SolarSystem is one of:
;; - (make-star String Number)
;; - (make-planet String Number SolarSystem)
;; Interpretation: A solar system has a star, and some number of planets
;; orbiting that star.
;; A SolarSystem is either:
;; - The star of a solar system, with its name and mass in Earth masses.
;;   (The mass of the Earth is 1 Earth mass.)
;; - A planet with name and mass (in Earth masses) that orbits farther
;;   than the portion of the solar system that is closer.

;; Part A

;; Write four examples of a SolarSystem. You do *not* have to get names of
;; planets or their masses correct. Feel free to make them up.

;; [TODO] Four examples

(define SS-POLARIS (make-star "Polaris" 1))
(define SS-SIRIUS (make-star "Sirius" 1))
(define SS-EARTH (make-planet "Earth" 10 SS-POLARIS))
(define SS-MARS (make-planet "Mars" 5 SS-SIRIUS))
(define SS-JUPITER (make-planet "Jupiter" 2 SS-EARTH))
 

;; Part B

;; Write the template for a SolarSystem.

;; [TODO] Template

(define (solarsystem-temp ss)
  (...
   (cond
     [(star? ss) ... (star-temp ss) ...]
     [(planet? ss) ... (solar-system-temp (planet-temp ss)) ...])))

;; Part C

;; Design a function called solar-system-star-name
;; that produces the name of the star in a solar system.

;; [TODO] Function design

(check-expect (solar-system-star-name SS-POLARIS) (star-name SS-POLARIS)) 
(check-expect (solar-system-star-name SS-MARS) (star-name SS-SIRIUS))
(check-expect (solar-system-star-name SS-EARTH) (star-name SS-POLARIS))
(check-expect (solar-system-star-name SS-JUPITER) (star-name SS-POLARIS))

; solar-system-star-name : SolarSystem -> String
; produces the name of the star in the given solar system

(define (solar-system-star-name ss)
  (cond
    [(star? ss) (star-name ss)] 
    [(planet? ss) (solar-system-star-name (planet-closer ss))]))















