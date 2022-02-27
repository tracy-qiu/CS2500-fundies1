;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 31-review1122) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Local/Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO
; Design a function that determines if every number in a list is bigger than a supplied number.

; all-bigger-than-n? : [List-of Number] Number -> Boolean
; Are all numbers bigger than n?

(check-expect (all-bigger-than-n? (list 1 2 3) 0) #true)
(check-expect (all-bigger-than-n? (list 1 2 3) 2) #false)

(define (all-bigger-than-n? lon n)
  (andmap
   (λ (list-n) (> list-n n))
   lon))


; TODO
; Design a function that negates a predicate.

; negate : (X) [X -> Boolean] -> [X -> Boolean]
; Negates p

(check-expect ((negate even?) 2) (odd? 2))
(check-expect ((negate even?) 3) (odd? 3))

(define (negate p?)
  (λ (x) (not (p? x))))

#|
(define (negate p?)
  (local [; negate
          ]))
|#

; TODO
; Design a function which composes two functions.

; mycompose : (X Y Z) [X -> Y] [Y -> Z] -> [X -> Z]
; Composes two functions

(check-expect ((mycompose add1 add1) 2) 4)
(check-expect ((mycompose add1 sqr) 4) 25)

(define (mycompose f_xy g_yz)
  (λ (x) (g_yz (f_xy x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO
; Courses at Northeastern have a subject, a number, a title, and a set of prerequisites.
; Design a data definition for a course.

(define-struct course [subject number title prereqs])

; A Course is a (make-course String Nat String [List-of Course])

(define C-CS2500 (make-course "CS" 2500 "Fundies 1" '()))
(define C-CS2510 (make-course "CS" 2510 "Fundies 2" (list C-CS2500)))

; CS2500 (Fundamentals of Computer Science 1) needs nothing
; CS2510 (Fundamentals of Computer Science 2) needs CS2500
; CS3500 (Object Oriented Design) needs CS2510
; CS3200 (Database Design) needs CS2510 <-- I wish
; CS4500 (Software Development) needs CS3500, CS3200 <-- I wish

(define (course-temp c)
  (... (course-subject c) ...
       (course-number c) ...
       (course-title c) ...
       (loc-temp (course-prereqs c)) ...))

(define (loc-temp loc)
  (...
   (cond
     [(empty? loc) ...]
     [(cons? loc) ...
      (course-temp (first loc)) ...
      (loc-temp (rest loc)) ...])))

; TODO
; Given a course, design the function how-many-courses-do-I-have-to-take that computes the
; total number of courses that require this course as a pre-req (or a pre-req of a pre-req, etc).

; how-many-courses-do-I-have-to-take : Course -> Nat
; Computes the number of courses that have to be taken before you can take this course

; CS2500 = 0
; CS2510 = 1
; CS3500 = 2
; CS3200 = 2
; CS4500 = 4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Multiple Inputs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A BasePair is one of
; - "A"
; - "C"
; - "T"
; - "G"
; Interpretation: A single base pair in DNA
 
; A Sequence is a [List-of BasePair]
; Interpretation: A sequence of DNA


; TODO
; Design the function same-sequence? that accepts two sequences
; and returns whether or not they are the same.

(define (s-s-temp s1 s2)
  (...
   (cond
     [(and (empty? s1) (empty? s2)) ...]
     [(and (empty? s1) (cons? s2)) ...
      (first s2) ...
      (s-s-temp s1 (rest s2)) ...]
     [(and (cons? s1) (empty? s2)) ...
      (first s1) ...
      (s-s-temp (rest s1) s2) ...]
      [(and (cons? s1) (cons? s2)) ...
      (first s1) ... (frist s2) ...
      (s-s-temp (rest s1) (rest s2)) ...])))
     
; same-sequence? : Sequence Sequence -> Boolean
; Returns whether or not the sequences are the same

(check-expect (same-sequence? (explode "ATGACT") (explode "TGCATT")) #false)
(check-expect (same-sequence? (explode "ATGACT") (explode "ATG")) #false)
(check-expect (same-sequence? (explode "AATGACTA") (explode "ATGCATTA")) #false)
(check-expect (same-sequence? (explode "AATGACTA") (explode "AATGACTA")) #true)
(check-expect (same-sequence? (explode "AA") (explode "AATGACTA")) #false)

(define (same-sequence? s1 s2)
   (cond
     [(and (empty? s1) (empty? s2)) #true]
     [(and (empty? s1) (cons? s2)) #false]
     [(and (cons? s1) (empty? s2)) #false]
      [(and (cons? s1) (cons? s2))
       (and
        (string=? (first s1) (first s2))
        (same-sequence? (rest s1) (rest s2)))]))


; TODO
; Design the number num-matches that accepts two sequences and
; returns the number of places where they contain the same base pair.

; num-matches : Sequence Sequence -> Nat
; Returns the numebr of places where the sequence matches

(check-expect (num-matches (explode "ATGACT") (explode "A")) 1)
(check-expect (num-matches (explode "ATGACT") (explode "TGCATT")) 2)
(check-expect (num-matches (explode "TGCC") (explode "TGCATT")) 3)
(check-expect (num-matches (explode "AATGACTA") (explode "ATGCATTA")) 4)

(define (num-matches s1 s2)
   (cond
     [(and (empty? s1) (empty? s2)) 0]
     [(and (empty? s1) (cons? s2)) 0]
     [(and (cons? s1) (empty? s2)) 0]
      [(and (cons? s1) (cons? s2))
       (+
      (if (string=? (first s1) (first s2)) 1 0)
        (num-matches (rest s1) (rest s2)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct count [x n])
 
; A [Count X] is a (make-count X Nat)
; An element and how often it occurs in some context


; TODO
; Design the function compute-frequencies that produces a list of counts
; for a supplied list with equivalence relation.

; compute-frequencies : (X) [List-of X] [X X -> Boolean] -> [List-of [Count X]]
; Counts all the distinct elements in the list per the equivalence relation

(check-expect (compute-frequencies '() =) '())
 
(check-expect (compute-frequencies (list 1 2 2) =) (list (make-count 1 1)
                                                         (make-count 2 2)))
 
(check-expect (compute-frequencies (list 1 1 2 2) =) (list (make-count 1 2)
                                                           (make-count 2 2)))
 
(check-expect (compute-frequencies (list "a" "b" "c" "c" "c") string=?)
              (list (make-count "a" 1)
                    (make-count "b" 1)
                    (make-count "c" 3)))


