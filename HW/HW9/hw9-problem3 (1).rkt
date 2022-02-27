;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists: a list of strings and a list of
;; numbers. The output should be a list of strings, where each string in the
;; output is the corresponding input string duplicated N times, where N
;; is the number in the corresponding list of numbers. However:
;;
;; 1. If there  are more strings than numbers, assume that the extra strings
;;    should be repeated twice each.
;; 1. If there are more numbers than strings, for each extra number N, 
;;    repeat the the string "Extra!" N times.

;; [TODO] Function design, and you *may* use replicate.

;; list-multiplier : [List-of Numbers] [List-of Strings] -> [List-of Strings]
;; Given a list of strings and list of numbers, returns a list of strings where
;; each corresponding string is duplicated the corresponding N times

(check-expect (list-multiplier (list 3 2) (list "apple" "banana"))
              (list "appleappleapple" "bananabanana"))
(check-expect (list-multiplier (list 1 2 3) (list "a" "z" "b"))
              (list "a" "zz" "bbb"))
(check-expect (list-multiplier (list 3 2 3)  (list "anusha" "jaq" "v"))
              (list "anushaanushaanusha" "jaqjaq" "vvv"))

(define (list-multiplier lon los)
  (cond
    [(or (empty? lon) (empty? los)) '()]
    [(and (cons? lon) (cons? los))
     (cons (multiplier (first lon) (first los))
           (list-multiplier (rest lon) (rest los)))]))


;; multiplier : NatNum String -> String
;; given a number and string, produces a new string where the string is duplicated n number of times 

(check-expect (multiplier 3 "apple") "appleappleapple")
(check-expect (multiplier 2 "banana") "bananabanana")
(check-expect (multiplier 0 "cat") "")

(define (multiplier n s)
  (cond
    [(= 0 n) ""]
    [else (string-append s (multiplier (- n 1) s))]))


