;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond0913) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; v: cond
; syntax:

#|
(cond
[test-1 answer-1]
[test-2 answer-2]
...
[else answer-n]
|#

; semantics: cond returns the first answer whose test is #true 


; TODO
; sign : Number -> String
; Returns "positive", "negative", or "zero" for the number

(check-expect (sign 5) "positive")
(check-expect (sign -5) "negative")
(check-expect (sign 0) "zero")

(define (sign num)
  (cond
    [(> num 0) "positive"]
    [(< num 0) "negative"]
    [(= num 0) "zero"]))
    ; can also do [else "zero"]))

; TODO
; num-grade : Number -> String
; Returns the letter grade corresponding to your numeric grade

(check-expect (num->grade 0) "F")
(check-expect (num->grade 75) "C")
(check-expect (num->grade 95) "A")

(define (num -> grade grade)
  (cond
    [(>= num 90) "A"]
    [(>= num 80) "B"]
    [(>= num 70) "C"]
    [(>= num 60) "D"]
    [else "F"]))

#|
(cond
  [test1 ans-1]
  [else else-ans])
|#


(define (num-> pf grade)
  (if (string=? (num-> grade grade "F"))))
                





; 09/15/21

; A NumericGrade is a real number in [0, 100]
; Interpretation: a numeric grade in the US system

(define NUMGRADE-0 0)
(define NUMGRADE-1 95)
(define NUMGRADE-2 87.5)
(define NUMGRADE-3 52)
(define NUMGRADE-100 100)

(define (numgrade-temp ng)
  (... ng ...))
  
; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "F"
; Interpretation: possible letter grades in the US system

(define LETTERGRADE-A "A")
(define LETTERGRADE-B "B")
(define LETTERGRADE-C "C")
(define LETTERGRADE-D "D")
(define LETTERGRADE-F "F")

(define (lettergrade-temp lg)
  (...
     (cond
       [(string=? lg LETTERGRADE-A) ...]
       [(string=? lg LETTERGRADE-B) ...]
       [(string=? lg LETTERGRADE-C) ...]
       [(string=? lg LETTERGRADE-D) ...]
       [(string=? lg LETTERGRADE-F) ...])))
       
; num->grade : NumericGrade -> LetterGrade creating types that we need to design 
; Returns the ltter grade corresponding to your numeric grade

; (check-expect (num->grade 0) "F")
 (check-expect (num->grade 75) "C")
; (check-expect (num->grade 95) "A")

(define (num->grade grade)
  (cond
    [(<= 90 grade) "A"]
    [(<= 90 grade) "B"]
    [(<= 90 grade) "C"]
    [(<= 90 grade) "D"]
    [else "F"]))

; A GPAPoint is a real number in [0, 4]
; Interpretation: grade point average in (most) US schools

(define GPA-A 4.0)
(define GPA-OK 3.4)
(define GPA-ADVISOR 2.0)

(define (gpa-temp gpa)
  (... gpa ...))

; Design the function grade->gpa that converts
; a letter grade to GPA points.

; grade->gpa : LetterGrade -> GPAPoint
; converts from letters to gpa points

(check-expect (grade->gpa LETTERGRADE-A) 4.0)
(check-expect (grade->gpa LETTERGRADE-B) 3.0)
(check-expect (grade->gpa LETTERGRADE-C) 2.0)
(check-expect (grade->gpa LETTERGRADE-D) 1.0)
(check-expect (grade->gpa LETTERGRADE-F) 0.0)
  
(define (grade->gpa lg)
     (cond
       [(string=? lg LETTERGRADE-A) 4.0]
       [(string=? lg LETTERGRADE-B) 3.0]
       [(string=? lg LETTERGRADE-C) 2.0]
       [(string=? lg LETTERGRADE-D) 1.0]
       [(string=? lg LETTERGRADE-F) 0.0]))
              