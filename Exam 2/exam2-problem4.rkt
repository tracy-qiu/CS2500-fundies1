;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam2-problem4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;; Consider the following data definition:

(define-struct person [name parent1 parent2])
;; A Person is one of:
;; - #false
;; - (make-person String Person Person)
;; Interpretation:  A person's name and two biological parents.
;; Or a sentinel value representing that a person's identity/lineage is not 
;; known.

(define (person-template p)
  (cond
    [(boolean? p) ...]
    [(person? p)
     (... (person-name p) ...
          (person-temp (person-parent1 p)) ...
          (person-temp (person-parent2 p)) ...)]))

(define PERSON-0 #false)
(define PERSON-1 (make-person "Alice" PERSON-0 PERSON-0))
(define PERSON-2 (make-person "Bob" PERSON-0 PERSON-0))
(define PERSON-3 (make-person "Carol" PERSON-1 PERSON-2))
(define PERSON-4 (make-person "David" PERSON-3 (make-person "Erin" PERSON-0 PERSON-0)))

;; Design a function called remove-ancestors that receives a Person and a
;; list of names (strings) to remove, and produces a Person without those names.
;; If a name in the list has ancestors, all their ancestors will be removed
;; as well.
;;
;; For example, here is a check-expect that should succeed. You should write
;; at least one more.

(check-expect
 (remove-ancestors PERSON-4 (list "Erin" "Bob"))
 (make-person "David" 
              (make-person "Carol" PERSON-1 #false)
              #false))
(check-expect (remove-ancestors PERSON-3 (list "Bob"))
              (make-person "Carol" PERSON-1 #false))                                                     
(check-expect (remove-ancestors PERSON-0 (list "Erin" "Bob")) PERSON-0) 

; remove-ancestors : Person [List-of Strings] -> Person 
; given a person and a list of names, returns a person with those ancestors' names removed

(define (remove-ancestors p loa)
  (local [; remove-one : Person Person -> Boolean
          ; given a person and one parent, returns if parents name is in the list of people to remove
          (define (remove-one person parent)
            (cond
              [(boolean? parent) #false]
              [(string? (person-name parent))
               (if (ormap (λ (name) (string=? name (person-name parent))) loa)
                   #false
                   (remove-ancestors parent loa))]))]
    (cond
      [(boolean? p) p]
      [(person? p)
       (make-person (person-name p)
                    (remove-one p (person-parent1 p))
                    (remove-one p (person-parent2 p)))])))
                               
