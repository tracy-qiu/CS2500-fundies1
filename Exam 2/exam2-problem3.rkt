;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; Recall the data definition and template for non-empty lists:
;;
;; An [NEList-of X] is one of:
;; - (cons X '())
;; - (cons X [NEList-of X])
;; Interpretation: A list with at least one item.
(define (nelist-template alist)
  (cond
    [(empty? (rest alist)) (... (first alist) ...)]
    [(cons? (rest alist))
     (... (first alist) ... 
          (nelist-template (rest alist)) ...)]))

;; Design a function called repeat-right that receives an [NEList-of String]
;; and produces an [NEList-of String]. However, each string in the output list
;; should be the string that you get when you append all strings to the right
;; of the corresponding string in the input.
;;
;; For example, here is a check-expect that should succeed. You should write
;; at least one more.

(check-expect (repeat-right (list "Alice" "Bob" "Carol"))
              (list "AliceBobCarol" "BobCarol" "Carol"))
(check-expect (repeat-right (list "Josh" "Carrie")) (list "JoshCarrie" "Carrie"))
(check-expect (repeat-right (list "A" "B" "C" "D")) (list "ABCD" "BCD" "CD" "D"))
(check-expect (repeat-right '()) '()) 

;; You will receive 95% credit for a perfect solution that uses the template
;; for non-empty lists. For 100% credit, make effective use of list 
;; abstractions.

;; Hint: it might help to create a test with just Alice, and then Alice and Bob
; to understand the base case and recursive step.

;; [TODO] Function design.

; repeat-right : [List-of String] -> [List-of String]
; given a list of strings, returns a list of strings where all strings to the right
; of the corresponding string in the input

(define (repeat-right alist)
  (cond
    [(empty? alist) '()]
    [(empty? (rest alist)) (cons (first alist) '())]
    [(cons? (rest alist))                      
     (cons
      (foldr string-append "" alist) 
      (repeat-right (rest alist)))]))


