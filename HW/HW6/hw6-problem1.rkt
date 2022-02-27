;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; NOTE: In this problem (and all subsequent problems), for every function
;; design, you must write (1) *at least* two tests, and (2) ensure that there
;; are no "halloween colors". To address the latter, you may have to write
;; additional tests.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the three functions below (we have deliberately omitted tests):

;; hello-everyone: [List-of String] -> [List-of String]
;; Greets everyone in the list.
(define (hello-everyone los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append "Hello, " (first los) "!")
                       (hello-everyone (rest los)))]))

;; words-until-period: [List-of String] -> [List-of String]
;; Produces the words in the list up to the first period.
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los) 
     (if (string=? (first los) ".")
         '()
         (cons (first los) (words-until-period (rest los))))]))


;; starting-positive-numbers : [List-of Number] -> [List-of Number]
;; Produces the prefix of positive numbers in the list.
(define (starting-positive-numbers lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (if (<= (first lon) 0)
         '()
         (cons (first lon) (starting-positive-numbers (rest lon))))]))


;; Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Design that list abstraction.

;; [TODO] Design a list abstraction.


;; Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can.


