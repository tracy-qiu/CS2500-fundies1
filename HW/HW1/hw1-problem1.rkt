;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Part A

;; Examine the following function, and:
;; i.   Write down its signature,
;; ii.  Give it an informative purpose statement, and
;; iii. Give the function and its arguments more informative names.

;; [TODO] Signature
;; [TODO] Purpose

; detective : Number Number String -> String
; indicates if the rider is tall enough to get on the ride
; riders under 2ft tall are not able to ride 

; detective 
(define (detective height-ft height-in rider-name)
  (if (< (+ (* 12 height-ft) height-in) 48)
      (string-append "Welcome aboard, " rider-name "!")
      "Sorry, you may not board the ride. :("))

;; [TODO] Better names for detective and its arguments


;; Part B

;; Write the signature of the following function:

;; [TODO] Signature

; mystery : String String Number -> String
(define (mystery x y z)
  (string-append (substring x 0 z)
                 (substring y z)
                 (substring y 0 z)
                 (substring x z)))


;; Part C

;; Describe the values that mystery produces when you apply it to two identical
;; arguments for x and y.

;; [TODO] Prose description as a comment.

; That would return the argument twice. This first appends the from the index 0 to the zth element
; then taking the substring of the second argument just completes the word and that is repeated for
; the second word making two complete of the identical arguments.


