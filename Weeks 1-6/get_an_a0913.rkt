;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname get_an_a0913) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require test-engine/racket-tests)
; v; check-expect
; check-expect do-something expected-answer)
; sem: drracket will actually run these and tell you
; 
; gonna-get-an-A? : Number -> Boolean
; Returns whether or not youre going to get an A, given your grade
; Examples:

; (gonna-get-an-A? 10) should be #false 
(check-expect (gonna-get-an-A? 10 #false))
(check-expect (gonna-get-an-A? 10 #false))
(check-expect (gonna-get-an-A? 10 #true))
(check-expect (gonna-get-an-A? 10 #true))
        
(define (gonna-get-an-A? numeric-grade)
  (>= numeric-grade 90))