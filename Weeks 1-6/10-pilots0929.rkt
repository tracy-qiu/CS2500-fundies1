;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-pilots0929) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design the data for a flight that has a captain
; and first officer, both pilots with a name and age


(define-struct pilot [name age])
 
; A Pilot is a (make-pilot String PosInt)
; Representing the name and age of a pilot.
;  - name is the name of the pilot
;  - age is the age of the pilot in years
 
(define PILOT-1 (make-pilot "alice" 25))
(define PILOT-2 (make-pilot "bob" 35))
(define PILOT-3 (make-pilot "carol" 70))

(define (pilot-temp p)
  (... (pilot-name p ...) ...
       (pilot-age p) ...))

 
(define-struct flight [captain firstofficer])
 
; A Flight is a (make-flight Pilot Pilot)
; representing a flight with the two pilots.
;  - captain is the pilot who is the captain of the flight
;  - firstofficer is the pilot who is the first officer of the flight
 
(define FLIGHT-1 (make-flight PILOT-1 PILOT-2))
(define FLIGHT-2 (make-flight PILOT-1 PILOT-3))
(define FLIGHT-3 (make-flight PILOT-2 PILOT-3))

(define (flight-temp f)
    (... (pilot-temp (flight-captain f ...)) ...
         (pilot-temp (flight-firstofficer f)) ...))


; Design the function is-alice-flying? that accepts a Flight
; and returns whether or not the pilot "alice" is assigned to this flight.

; is-alice-flying? : Flight -> Boolean
; determines if alice is on the flight

(check-expect (is-alice-flying? FLIGHT-1) #true)
(check-expect (is-alice-flying? FLIGHT-2) #true)
(check-expect (is-alice-flying? FLIGHT-3) #false)

(define (is-alice-flying f)
    (or (is-alice? (flight-captain f ...)) ...
         (is-alice? (flight-firstofficer f)) ...))


; is-alice? : Pilot -> Boolean
; is this pilot Alice?

(check-expect (is-alice? PILOT-1) #true)
(check-expect (is-alice? PILOT-1) #false)
(check-expect (is-alice? PILOT-1) #false)

(define (is-alice? p)
  (string=? (pilot-name p) "alice"))













