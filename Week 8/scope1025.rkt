;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname scope1025) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; f(x) = x + 7
; g(y) = x + y + 10

(define (f x)
  (+ x 7))

(f 3)

; V: local
; S:

#|
(local [definition1
         definition2
         ...]
  expr-possibly-using-local-definitions)
|#

(define INSIDE-JOKE "howdy")

(local [(define INSIDE-JOKE "knock knock")]
  (string-append INSIDE-JOKE "! who's there?"))

(define B
  (local [(define X 20)]
    (local [(define X 30)]
      X)))

(define C
  (local [(define other-thing 2)
          (define (helper x) x)
          (define (main z) (helper (* other-thing z)))]
    main))

; result of outer-function is inner-function
; outer-function : Number Number -> [Number -> Number]
  (local [(define (inner-function a)
            (+ x y a))]
    inner-function))

(define CONFUSED (outer-function 2 3))

(check-expect (CONFUSED 4) 9)







