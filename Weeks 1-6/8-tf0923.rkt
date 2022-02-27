;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8-tf0923) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design a function passed? that determines if selecting
; the same response for all five questions on a True/False
; Test gets you a passing grade (at least 3-out-of-5).
; Each question has a prompt and a correct answer.

(define-struct tfq [prompt answer])
 
; A TFQuestion is a (make-tfq String Boolean)
; representing a prompt and correct response to a true/false question
 
(define TFQ-1 (make-tfq "Northeastern has a campus in Hawaii" #false))
(define TFQ-2 (make-tfq "Northeastern has a campus in London" #true))
(define TFQ-3 (make-tfq "Northeastern has a campus in Seattle" #true))
(define TFQ-4 (make-tfq "Northeastern has a campus in Florida" #false))
(define TFQ-5 (make-tfq "Northeastern has a campus in Egypt" #false))
 
(define (tfq-temp tfq)
  (... (tfq-prompt tfq) ...
       (tfq-answer tfq) ...))
 
(define-struct tft [q1 q2 q3 q4 q5])
 
; A TFTest is a (make-tft TFQuestion TFQuestion TFQuestion TFQuestion TFQuestion)
; representing five true-false questions on a test
 
(define TFT-1 (make-tft TFQ-1 TFQ-2 TFQ-3 TFQ-4 TFQ-5))
 
(define (tft-temp tft)
  (...
   (tfq-temp (tft-q1 tft)) ...
   (tfq-temp (tft-q2 tft)) ...
   (tfq-temp (tft-q3 tft)) ...
   (tfq-temp (tft-q4 tft)) ...
   (tfq-temp (tft-q5 tft)) ...))

; passed? : TFTest Boolean -> Boolean
; do I get 3-out-of-5 questions correct by always
; using the supplied answer

(check-expect (passed? TFT-1 #true) #false)
(check-expect (passed? TFT-1 #false) #true)

(define (passed? tft my-answer)
   (>= 
    (+
     (grade (tft-q1 tft) my-answer)  ; 0 or 1
     (grade (tft-q2 tft) my-answer)  ; 0 or 1
     (grade (tft-q3 tft) my-answer)  ; 0 or 1
     (grade (tft-q4 tft) my-answer)  ; 0 or 1
     (grade (tft-q5 tft) my-answer)) ; 0 or 1
    3))

; grade : TFQuestion Boolean -> {0, 1} 
; gives on point if the supplied answer is correct
; 0 otherwise

(check-expect (grade TFQ-1 #true) 0)
(check-expect (grade TFQ-1 #false) 1)
(check-expect (grade TFQ-2 #true) 1)
(check-expect (grade TFQ-2 #false) 0)

(define (grade tfq my-answer)
  (if (boolean=? (tfq-answer tfq) my-answer) 1 0))






































    