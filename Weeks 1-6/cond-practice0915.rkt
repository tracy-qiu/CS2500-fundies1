;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond-practice0915) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design the data for temperature.

; A CelciusTemp is a real number greater than -273.15
; A FarTemp is a real number greater than -459.67
;

; Design the data for an MBTA line

; A MBTALine is one of the:
- "orange"
- "red"
- "green"
- "blue"

(define MBTALINE-GREEN "green")
(define MBTALINE-RED "red")
(define MBTALINE-ORANGE "orange")
(define MBTALINE-BLUE "blue")
(define MBTALINE-A "F")


; Design a function convert that converts a Celcius
; temperature to a Farenheit one.

; Design the function stops-atNU? that dtermines
; if a line stops at Northeastern. 