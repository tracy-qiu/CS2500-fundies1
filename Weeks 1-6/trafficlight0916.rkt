;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trafficlight0916) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Design a program that shows a traffic light,
; and just changes red->green->yellow...

(define TL-RADIUS 50)

; A TrafficColor is one of:
; - "green"
; - "yellow"
; - "red"
; Interpretation: the traffic light colors!

(define TC-GREEN "green")
(define TC-YELLOW "yellow")
(define TC-RED "red")

(define (tc-temp tc)
  (...
   (cond
     [(string=? tc TC-GREEN)...]
     [(string=? tc TC-YELLOW)...]
     [(string=? tc TC-RED)...])))

; traffic-light : TrafficColor -> TrafficColor
; simulates a traffic light

(define (traffic-light start-color)
  (big-bang start-color
    [to-draw draw-light]
    [on-tick change-light]))

; draw-light : TrafficColor -> Image
; draws the light

(check-expect (draw-light TC-GREEN) (circle TL-RADIUS "solid" "green"))
(check-expect (draw-light TC-YELLOW) (circle TL-RADIUS "solid" "yellow"))
(check-expect (draw-light TC-RED) (circle TL-RADIUS "solid" "red"))


(define (draw-light tc)
   (cond
     [(string=? tc TC-GREEN)(circle TL-RADIUS "solid" "green")]
     [(string=? tc TC-YELLOW)(circle TL-RADIUS "solid" "yellow")]
     [(string=? tc TC-RED)(circle TL-RADIUS "solid" "red")])))
