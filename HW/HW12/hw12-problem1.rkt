;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw12-master) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Consider the following function (and its two helpers), which you may have
;; seen in class:
;;
;; chunkify : (X) [List-of X] Nat[>0] -> [List-of [List-of X]]
;; Groups the input list into sub-lists of supplied length.
 
(check-expect (chunkify '() 2) '())
(check-expect (chunkify (list 1 2 3) 1) (list (list 1) (list 2) (list 3)))
(check-expect (chunkify (list 1 2 3) 2) (list (list 1 2) (list 3)))
(check-expect (chunkify (list "a" "b" "c" "d" "e" "f") 3)
              (list (list "a" "b" "c") (list "d" "e" "f")))

(define (chunkify lox nat)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (cons
      (take lox nat)
      (chunkify (drop lox nat) nat))]))


;; take : (X) [List-of X] Nat -> [List-of X]
;; Returns up the Nat elements of the list.
 
(check-expect (take '() 2) '())
(check-expect (take (list 1 2 3) 1) (list 1))
(check-expect (take (list 1 2 3) 4) (list 1 2 3))
 
(define (take lox nat)
  (cond
    [(or (empty? lox) (zero? nat)) '()]
    [(and (cons? lox) (positive? nat))
     (cons (first lox)
           (take (rest lox) (sub1 nat)))]))


;; drop : (X) [List-of X] Nat -> [List-of X]
;; Drops Nat elements of the list.
 
(check-expect (drop '() 2) '())
(check-expect (drop (list 1 2 3) 1) (list 2 3))
(check-expect (drop (list 1 2 3) 4) '())
 
(define (drop lox nat)
  (cond
    [(or (empty? lox) (zero? nat)) lox]
    [(and (cons? lox) (positive? nat))
     (drop (rest lox) (sub1 nat))]))

;; The chunkify function is an example of generative recursion, since
;; it does not recur on (rest lox), but on (drop lox nat).

;; The chunkify function visits each item in lox twice: once in take and
;; then again in drop. For example (chunkify (list 1 2 3 4) 2) produces
;; the following applications of the helper functions:
;;
;; - (chunkify (list 1 2 3 4) 2)
;;    - (take (list 1 2 3 4) 2) => (list 1 2)
;;    - (drop (list 1 2 3 4) 2) => (list 3 4)
;; - (chunkify (list 3 4) 2)
;;   - (take (list 3 4) 2) => (list 3 4)
;;   - (drop (list 3 4) 2) => '()
;; - (chunkify '() 2) => '()
;;

;; Your goal is to write an alternative version of chunkify that visits each
;; item at most once.
;;
;; A few things to keep in mind:
;;
;; - An accumulator may be helpful, but is *not required*.
;;
;; - When you design helper functions, ensure you follow the design recipe.
;;   You will *need* at least one helper function.
;;
;; - If your helper function requires an auxiliary data definition,
;;   even if its really simple, ensure you follow the data design recipe.
;;
;; Hint: Is it possible to take and drop at the same time?


