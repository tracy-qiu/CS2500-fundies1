;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; NOTE #3: You *may not* use the builtin function replicate to solve this
;; problem.

;; Design a function that receives a number N and a list of strings, and
;; produces a list of strings where each output string is the corresponding
;; input string repeated N times.

;; [TODO] Function design *without using replicate*

; list-multiplier : NatNum [List-of String] -> [List-of String]
; given a number and a list of strings, produces a list of strings
; where each output string is the corresponding input string repeated N times

(check-expect (list-multiplier 3 (list "apple" "banana"))
              (list "appleappleapple" "bananabananabanana"))
(check-expect (list-multiplier 2 (list "a" "z" "b"))
              (list "aa" "zz" "bb"))
(check-expect (list-multiplier 5 (list "anusha" "jaq" "v"))
              (list "anushaanushaanushaanushaanusha" "jaqjaqjaqjaqjaq" "vvvvv"))
(check-expect (list-multiplier 0 (list "apple" "banana"))
              (list "" ""))
              
(define (list-multiplier n los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons (multiplier n (first los))
           (list-multiplier n (rest los)))]))

;; multiplier : NatNum String -> String
;; given a number and string, produces a new string where the string is duplicated n number of times 

(check-expect (multiplier 3 "apple") "appleappleapple")
(check-expect (multiplier 2 "banana") "bananabanana")
(check-expect (multiplier 0 "cat") "")

(define (multiplier n s)
  (cond
    [(= 0 n) ""]
    [else (string-append s (multiplier (- n 1) s))]))

