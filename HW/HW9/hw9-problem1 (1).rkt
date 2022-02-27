;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; NOTE #1: You may use list abstractions if you wish. However, they are not
;; mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists of numbers and produces a list of
;; numbers where each number is the average of the two corresponding numbers in 
;; the original lists. You may assume that the two lists have equal length.

;; [TODO]

;; average-list : [List-of Numbers] [List-of Numbers] -> [List-of Numbers]
;; Given two lists of numbers, produces a list where each number is
;; the average of the two corresponding numbers

(check-expect (average-list (list 1 2 5) (list 2 3 5))
              (list 1.5 2.5 5))

(check-expect (average-list '() '())
              '())

(check-expect (average-list (list 8 6 11) (list 2 4 7))
              (list 5 5 9))


(define (average-list l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (cons? l1) (cons? l2))
     (cons (* .5 (+ (first l1) (first l2)))
           (average-list (rest l1) (rest l2)))]))

