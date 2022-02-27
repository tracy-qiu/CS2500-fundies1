;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Lecture1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;(Define (put-sun-in-sky-at-y-coordinate)
       ; (place-image SUN)

(define SUN (circle 25 "solid" "yellow"))
(define SKY (rectangle 300 200 "solid" "light blue"))

(place-image SUN
             220 50
             SKY)

