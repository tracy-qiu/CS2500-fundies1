;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1
;;
;; Attempt all three TODO items for this problem.

;; Consider the following data definitions & interpretations:

(define-struct address [num st city us-state zip])

;; An Address is a (make-address Nat String String String Nat)
;; - where num is the number of the building on the street
;; - st is the name of the street
;; - city is the city the building is in
;; - us-state is the state the city is in
;; - and zip is the zipcode of the building
;; Interpretation: a US address
 
(define ADDRESS-NEU (make-address 360 "Huntington ave" "Boston" "MA" 02115))
(define ADDRESS-MARINO (make-address 369 "Huntington ave" "Boston" "MA" 02115))
(define ADDRESS-IV (make-address 1155 "Tremont st" "Boston" "MA" 02120))

(define (address-temp a)
  (... (address-num a) ...
       (address-st a) ...
       (address-city a) ... 
       (address-us-state a) ...
       (address-zip a) ... ))


(define-struct student [first last nuid local perm])

;; An NUStudent is a (make-student String String PositiveNumber Address Address)
;; - where first is the student's first name
;; - last is the student's last name
;; - nuid is the student's NUID #
;; - local is the student's local address
;; - and perm is the student's permanent address
;; Interpretation: a Northeastern student

(define NUSTUDENT-ALICE (make-student "Alice" "Smith" 001234568 ADDRESS-NEU ADDRESS-IV))
(define NUSTUDENT-BOB (make-student "Bob" "Ross" 0123456789 ADDRESS-IV ADDRESS-MARINO))
(define NUSTUDENT-JACK (make-student "Jack" "Beanstalk" 0123456789 ADDRESS-NEU ADDRESS-MARINO))

(define (student-temp s)
  (... (student-first s) ...
       (student-last s) ...
       (student-nuid s) ... 
       (student-local s) ...
       (student-perm s) ... ))

;; TODO #1: complete the data design recipe for both the data definitions above.


;; TODO #2: Design the function student-email which takes an NUStudent and
;; produces a string representing that student’s email address. For simplicity
;; we will say that a student’s email address is always their last name 
;; (all lowercase),  followed by a period, followed by the first initial
;; of their first name (also lowercase), and finished
;; with "@northeastern.edu".

(require mzlib/string)

; student-email : NUStudent -> String
; takes given NUStudent and produces a string representing student's email address

(check-expect (student-email NUSTUDENT-ALICE) "smith.a@northeastern.edu")
(check-expect (student-email NUSTUDENT-BOB) "ross.b@northeastern.edu")
(check-expect (student-email NUSTUDENT-JACK) "beanstalk.j@northeastern.edu")

(define (student-email s)
  (string-downcase(string-append (student-last s) "." (substring (student-first s) 0 1) "@northeastern.edu")))


;; TODO #3: Design the function update-zipcode which takes an NUStudent and a 
;; number, representing the new zip code of the person and updates their permanent
;; address to have that zip code. Be sure to follow the template!

; update-zipcode : NUStudent Number -> NUStudent
; takes given NUStudent and number(new zip code), updates the students' permanent address with new zip code

(define ADDRESS-IV2 (make-address 1155 "Tremont st" "Boston" "MA" 02116))
(define NUSTUDENT-ALICE2 (make-student "Alice" "Smith" 001234568 ADDRESS-NEU ADDRESS-IV2))

(check-expect (update-zipcode NUSTUDENT-ALICE 02116) NUSTUDENT-ALICE2)

(define (update-zipcode s zc)
  (make-student (student-first s)
                (student-last s)
                (student-nuid s)
                (student-local s)
                (update-address (student-perm s) zc)))
              
(define (update-address a zc)
  (make-address (address-num a)
                (address-st a)
                (address-city a)
                (address-us-state a)
                zc))
                                                                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2
;;
;; Submit work that shows some progress. You *do not* have to complete all TODO
;;  items.

;; You are to design a program text-mover to display and manipulate text on a
;; background. Your program should accept some phrase to show, as well as initial
;; location and color (we only support three: red, black, or purple) - you should
;; then display the phrase on the screen as described.

;; When the user presses a mouse button, the program should move the text to the
;; location that they clicked. When the user presses a key on the keyboard, the
;; program should rotate colors.

;; TODO #1: design the text-mover function think through the arguments to the
;; function, how you will represent the world state, and what handlers you need
;; to support.
;; - Hint A: since your state has multiple parts that change, you'll need a
;;           structure to hold them, but the parts themselves might also be new.
;; - Hint B: you've been provided some data definitions below that will be quite
;;           useful.

;; TODO #2: Finish designing the data from #1; think ahead to make examples that
;; are useful; for testing such operations as changing location and color.

;; TODO #3: design your to-draw handler, making use of the template(s) you 
;; designed in #2.

;; TODO #4: design your remaining handler(s), again following the appropriate
;; template(s).
;; - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;;            event, which you can check using the mouse=? function.
;; - Hint #2: make sure to follow your templates, which may involve breaking 
;;            the handlers  into helper functions.


;; A Position is a (make-posn Real Real)
;; Interpretation: a 2D location


;; A RedBlackPurple (RBP) is one of:
;; - "red"
;; - "black"
;; - "purple"
;; Interpretation: available font colors

(define RBP-RED "red")
(define RBP-BLACK "black")
(define RBP-PURPLE "purple")

(define (rbp-temp rbp)
  (...
   (cond
     [(string=? rbp RBP-RED) ...]
     [(string=? rbp RBP-BLACK) ...]
     [(string=? rbp RBP-PURPLE) ...])))

(define-struct tm [str pos col])

;; A TextMover (TM) is a (make-tm String Position RBP)
;; - str is the text to be displayed
;; - pos is the location of the text
;; - col is the color of the text
;; Interpretation: all the information needed for the text-mover program.

(define TM-HELLO (make-tm "hello" (make-posn 20 20) RBP-RED))
(define TM-WORLD (make-tm "world" (make-posn 30 30) RBP-BLACK))

(define (tm-temp t)
  (... (tm-str t) ...
       (tm-pos t) ...
       (tm-col t) ...))

; text-mover : TM -> TM
; simulates text mover

(define (text-mover tm)
  (big-bang tm
    [to-draw draw-tm]
    [on-mouse move-tm]
    [on-tick rotate-tm]))

(check-expect (draw-tm TM-HELLO) ((make-tm "hello" (make-posn 20 20) RBP-RED)))

(define (draw-tm t)
  (... (tm-str t) ...
       (tm-pos t) ...
       (tm-col t) ...))


                     
