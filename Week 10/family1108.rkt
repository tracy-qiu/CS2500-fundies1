;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname family1108) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct person [name parent1 parent2])

; A Person is one of:
; - #false
; - (make-person String Person Person)
; Interpretation: a person, or a #false for unknown identity 

(define PERSON-UNKNOWN #false)
(define PERSON-ALICE (make-person "Alice" PERSON-UNKNOWN PERSON-UNKNOWN))
(define PERSON-BOB (make-person "Bob" PERSON-UNKNOWN PERSON-UNKNOWN))
(define PERSON-CHRIS (make-person "Chris" PERSON-ALICE PERSON-BOB))

(define (person-temp p)
  (...
   (cond
     [(boolean? p) ...]
     [(person? p) ...
      (person-name p) ...
      (person-temp (person-parent1 p)) ...
      (person-temp (person-parent2 p)) ...])))

; TODO #1
; Design the function num-peeps that takes in a person
; and returns the number of named ancestors in their family

; num-peeps : Person -> Nat
; counts the number of peeps

(check-expect (num-peeps PERSON-UNKNOWN) 0)
(check-expect (num-peeps PERSON-ALICE) 1)
(check-expect (num-peeps PERSON-BOB) 1)
(check-expect (num-peeps PERSON-CHRIS) 3)

(define (num-peeps p)
  (cond
    [(boolean? p) 0]
    [(person? p)
     (+
      1
      (num-peeps (person-parent1 p))
      (num-peeps (person-parent2 p)))]))


; E.g., Alice + Bob are the parents of Chris; so 3 

; TODO #2 
; Design the function num-gens that takes in a person and
; returns the depth of their known ancestry

; E.g., Chris's depth is 2

; num-gens : Person -> Nat
; counts total depth of the given person's known ancestry

(define CRAZINESS
  (make-person
   "crazy"
   PERSON-UNKNOWN
   (make-person
    "crazy-parent"
    PERSON-UNKNOWN
    (make-person
     "crazy-grandparent"
     PERSON-UNKNOWN
     (make-person
      "crazy-greatgrandparent"
      PERSON-UNKNOWN
      PERSON-UNKNOWN)))))

(check-expect (num-gens PERSON-UNKNOWN) 0)
(check-expect (num-gens PERSON-ALICE) 1)
(check-expect (num-gens PERSON-BOB) 1)
(check-expect (num-gens PERSON-CHRIS) 2)
(check-expect (num-gens CRAZINESS) 4)

(define (num-gens p)
  (cond
    [(boolean? p) 0]
    [(person? p)
     (add1
      (max
       (num-gens (person-parent1 p)) 
       (num-gens (person-parent2 p))))]))




