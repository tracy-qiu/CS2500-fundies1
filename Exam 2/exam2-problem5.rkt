;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam2-problem5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 5

;; You are running a car dealership, which seems to be a hot business these
;; days. Design data to represent a single vehicle in the dealership. You
;; need to track the vehicle's manufacturer (brand), the year it was made,
;; the price, whether or not it is new, and whether it is electric,
;; hybrid-electric, or runs on gasoline (petrol).


;; [TODO] Data design. Your examples should include new and used cars, all types 
;; of powertrains, and a variety of other reasonable values for other aspects of
;; a vehicle.

(define-struct vehicle [brand year price new? energy])

; A Vehicle is a (make-vehicle String Nat Nat Boolean Energy)
; - where brand is the vehicle's manufacturer
; - year is the year it was made
; - price is the price of the vehicle
; - new? is whether the car is new
; - energy is the type of energy it runs on
; Interpretation: description of a vehicle in a dealership

(define VEHICLE1 (make-vehicle "Toyota" 2020 30000 #true "electric"))
(define VEHICLE2 (make-vehicle "Honda" 2000 4000 #false "gasoline"))
(define VEHICLE3 (make-vehicle "Jeep" 2015 15000 #false "hybrid-electric"))
(define VEHICLE4 (make-vehicle "Tesla" 2021 40000 #true "electric"))

(define (vehicle-temp v)
  (... (vehicle-brand v) ...
       (vehicle-year v) ...
       (vehicle-price v) ...
       (vehicle-new? v) ...
       (energy-temp (vehicle-energy v)) ...))

; Energy is one of:
; - "electric"
; - "hybrid-electric"
; - "gasoline"
; Represents the energy that the car powered by

(define (energy-temp e)
  (...
   (cond
     [(string=? e "electric") ...]
     [(string=? e "hybrid-electric") ...]
     [(string=? e "gasoline") ...])))
