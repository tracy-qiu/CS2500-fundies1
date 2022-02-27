;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab6-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Consider the two following definitions (do not change them).

;; A Posn is a (make-posn Real Real)
;;
;; Interpretation: an (x, y) coordinate

;; matching-x-posn : [List-of Posn] Number Posn -> Posn
;;
;; Find the first Posn in the list with the given x-coordinate, or produce the
;; given Posn if no such Posn can be found.

(check-expect (matching-x-posn '() 10 (make-posn 0 0)) (make-posn 0 0))
(check-expect
 (matching-x-posn (cons (make-posn 1 2) (cons (make-posn 3 4) '())) 3 (make-posn 5 6))
 (make-posn 3 4))

#|
(define (matching-x-posn lop desired-x default)
  (cond [(empty? lop) default]
        [(cons? lop)
         (if (= (posn-x (first lop)) desired-x)
             (first lop)
             (matching-x-posn (rest lop) desired-x default))]))
|#

;; string-with-length : [List-of String] Nat -> String
;;
;; Find the first String in the list with the given length, or produce
;; "no such string" if no such String can be found.

(check-expect (string-with-length '() 10) "no such string")
(check-expect
 (string-with-length (cons "hi" (cons "hello" (cons "aloha" '()))) 5)
 "hello")

#|
(define (string-with-length los desired-length)
  (cond [(empty? los) "no such string"]
        [(cons? los)
         (if (= (string-length (first los)) desired-length)
             (first los)
             (string-with-length (rest los) desired-length))]))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: attempt this function design

;; TODO: Design the function find-first-match that abstracts thes two functions
;; defined above.

; find-first-match : (X, Y) [List of X] Number X Y -> X
; abstracts the matching-x-posn and string-with-length functions

(define LOS-1 (cons (make-posn 1 2) (cons (make-posn 3 4) '())))
(define POS-1 (make-posn 0 0))
(define LOS-2 (cons "hi" (cons "hello" (cons "aloha" '()))))

(check-expect (find-first-match '() 10 POS-1 posn-x) POS-1)
(check-expect
 (find-first-match LOS-1 3 (make-posn 5 6) posn-x)
 (make-posn 3 4))  
 
(check-expect (find-first-match '() 10 "no such string" string-length) "no such string")
(check-expect
 (find-first-match LOS-2 5 "no such string" string-length)
 "hello")

(define (find-first-match los desired-something default op)
  (cond [(empty? los) default]
        [(cons? los)
         (if (= (op (first los)) desired-something)
             (first los)
             (find-first-match (rest los) desired-something default op))])) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: attempt this design *only if* you complete Problem 1

;; TODO: Redefine matching-x-posn and string-with-length using find-first-match.
(define (matching-x-posn lop desired-x default)
  (find-first-match lop desired-x default posn-x))


  
(define (string-with-length los desired-length)
  (find-first-match los desired-length "no such string" string-length))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3: attempt this design *only if* you complete Problem 1

;; TODO: Design the function any-true? that returns #true if a list of Boolean 
;; data contains at least one #true, otherwise #false. Use find-first-match to
;; do so.

(define (find-first-match los desired-something default op)
  (cond [(empty? los) default]
        [(cons? los)
         (if (= (op (first los)) desired-something)
             (first los)
             (find-first-match (rest los) desired-something default op))])) 

