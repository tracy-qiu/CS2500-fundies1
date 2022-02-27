;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: For all function designs, you must have 2+ tests and no halloween
;; colors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; The final objective in this problem is to design these three functions:
;;
;; 1. hello-goodbye: given a list of names, produces two strings for each
;;    string NAME: "Hello NAME!" and "Goodbye NAME!".
;;
;; 2. double-double: given a list of numbers, produces a list with two numbers
;;    for each number x: (* 2 x) and (* 4 x).
;;
;; 3. string-length-length : given a list of strings, produces a list with two 
;;    numbers for each string: the length, followed by half the length.
;;
;; However, you must not use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.
;;
;; To help you get started, here are the tests for the three functions.
;; However, you should also test your abstraction independently.

(check-expect (hello-goodbye '()) '())
(check-expect
 (hello-goodbye (cons "Alice" (cons "Bob" '())))
 (cons "Hello Alice!" (cons "Goodbye Alice!" (cons "Hello Bob!" (cons "Goodbye Bob!" '())))))


(check-expect (double-double '()) '())
(check-expect (double-double (cons 10 (cons 20 '())))
              (cons 20 (cons 40 (cons 40 (cons 80 '())))))


(check-expect (string-length-length '()) '())
(check-expect (string-length-length (cons "Hello" '()))
              (cons 5 (cons 2.5 '())))

;; hello-goodbye : [List-of String] -> [List-of String]
;; Greets and says goodbye to everyone in the list.
#|
(define (hello-goodbye los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello " (first los) "!")
                       (cons (string-append "Goodbye " (first los) "!")
                             (hello-goodbye (rest los))))])) 

(define (double-double lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon) (cons (* (first lon) 2)
                       (cons (* (first lon) 4)
                             (double-double (rest lon))))])) 

(define (string-length-length los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-length (first los))
                       (cons (* (string-length (first los)) 0.5) 
                             (string-length-length (rest los))))]))
|#

; two-items : (X, Y) [List of X] Y -> [List of X]
; abstracts functions hello-goodbye, double-double, and string-length-length

(define LOS-1 (cons "Tracy" (cons "Anusha" '())))
(define HG-LOS-1 (cons "Hello Tracy!" (cons "Goodbye Tracy!"
                                            (cons "Hello Anusha!"
                                                  (cons "Goodbye Anusha!" '())))))
(define LOS-2 (cons "Ferd" '()))
(define HG-LOS-2 (cons "Hello Ferd!" (cons "Goodbye Ferd!" '())))

(define LON-1 (cons 5 (cons 10 '())))
(define DD-LON-1 (cons 10 (cons 20 (cons 20 (cons 40 '())))))
(define LON-2 (cons 8 (cons 1 (cons 3 '()))))
(define DD-LON-2 (cons 16 (cons 32 (cons 2 (cons 4 (cons 6 (cons 12 '())))))))

(define SLL-LOS-1 (cons 5 (cons 2.5 (cons 6 (cons 3 '())))))
(define SLL-LOS-2 (cons 4 (cons 2 '())))
  
(check-expect (two-items LOS-1 h-g) HG-LOS-1)
(check-expect (two-items LOS-2 h-g) HG-LOS-2)

(check-expect (two-items LON-1 d-d) DD-LON-1)
(check-expect (two-items LON-2 d-d) DD-LON-2)

(check-expect (two-items LOS-1 s-l-l) SLL-LOS-1)
(check-expect (two-items LOS-2 s-l-l) SLL-LOS-2)

(define (two-items los op)
  (cond
    [(empty? los) '()]
    [(cons? los) (op (first los) (two-items (rest los) op))]))

; h-g : X [List of X] -> [List of X]
; helper function for hello-goodbye
; produces two strings for each string NAME:
; "Hello NAME!" and "Goodbye NAME!" for just the first item
(check-expect (h-g "Jack" LOS-1) (cons "Hello Jack!" (cons "Goodbye Jack!" LOS-1)))
(check-expect (h-g "Alivia" LOS-2) (cons "Hello Alivia!" (cons "Goodbye Alivia!" LOS-2)))

(define (h-g first rest)
  (cons (string-append "Hello " first "!")
        (cons (string-append "Goodbye " first "!")
              rest))) 

; d-d : X [List of X] -> [List of X]
; helper function for double-double
; produces a list with two numbers for each number x:
; (* 2 x) and (* 4 x) for just the first item
(check-expect (d-d 5 LON-1) (cons 10 (cons 20 LON-1)))
(check-expect (d-d 7 LON-2) (cons 14 (cons 28 LON-2)))


(define (d-d first rest)
  (cons (* first 2)
        (cons (* first 4)
              rest)))

; s-l-l : X [List of X] -> [List of X]
; helper function for string-length-length
; produces a list with two numbers for each string:
; the length, followed by half the length for just the first item
(check-expect (s-l-l "Jack" LOS-1) (cons 4 (cons 2 LOS-1)))
(check-expect (s-l-l "Alivia" LOS-2) (cons 6 (cons 3 LOS-2)))

(define (s-l-l first rest)
  (cons (string-length first)
        (cons (* (string-length first) 0.5)
              rest))) 


; all signatures, purposes, and check-expects for the following functions were provided

(define (hello-goodbye los)
  (two-items los h-g))

(define (double-double lon)
  (two-items lon d-d))

(define (string-length-length los)
  (two-items los s-l-l))

