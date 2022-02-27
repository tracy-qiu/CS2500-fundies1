;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Design a function that consumes two strings and produces a single string
;; that concatenates them, with a space between them. However, in this result,
;; the first string should be the *longer* of the two. Ensure you follow
;; all steps of the design recipe, and include three distinct check-expects.

;; [TODO]

; concatenate-strings : String String -> String
; concatenates two strings with the longer of the two coming first

(define (concatenate-strings string1 string2)
  (cond
    [(>= (string-length string1) (string-length string2)) (string-append string1 " " string2)]
    [else (string-append string2 " " string1)]))
      
(check-expect (concatenate-strings "hello" "bye") "hello bye")
(check-expect (concatenate-strings "ball" "orange") "orange ball")
(check-expect (concatenate-strings "ball" "wall") "ball wall")