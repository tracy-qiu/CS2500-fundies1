;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lecture3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; create the funtion rectangle-area that computes
; the area of a rectangle, given the width and height
; (reminder: area = height x width). And dont forget a
; signature and purpose statement

; rectangle-area : Number Number -> Number
; Given the width and height of a rectangle, compute its area

(define (rectangle-area width height)
  (+ (* 100 width) height))

