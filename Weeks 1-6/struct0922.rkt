;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname struct0922) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Position is a (make-posn Real Real)
; Interpretation: the x- and y-coordinates of an object

(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn 3 4))
(define POSN-2 (make-posn 89.2 pi))

(define (posn-temp p)
  (...
   (posn-x p) ...
   (posn-y p) ...))

; (define-struct posn [x y])


; Design the data to represent a Northeastern student, covering
; their name, NUID, and their hometown as an (x,y) location.
; For practice, let;s list all the  functions that become available,
; and what their signatures are in the context of our program.

; A NUStudent is a (namke-posn String (make-posn Int (make-posn Real Real)))

; very ineffective way of creating student  
; (define ALICE (make-posn "alice" (make-posn 123 (make-posn 20 50))))

(define-struct nustudent [name nuid hometown-loc])

; An NUStudent is a (make-nustudent String PosInt Position)
; Interpretation : a Northeastern student
; - name is the name of the studnet
; - nuid is the unique identifier
; - hometown-loc is the (x,y) locaiton of their hometown on an American map of the world

(define NUSTUDENT-ALICE (make-nustudent "alice" 12345 POSN-0))
(define NUSTUDENT-BOB (make-nustudent "bob" 98765 POSN-1))
(define NUSTUDENT-CHRIS (make-nustudent "chris" 32458 POSN-2))

; template - go through selectors 
(define (nustudent-temp nustudent) 
  (... (nustudent-name nustudent)...
       (nustudent-nuid nustudent) ...
       (posn-temp (nustudent-hometown-loc nustudent))...))
   

; (constructor) make-nustudent : String PosInt Position  -> NUStudent 
; (predicate) nustudent? : Any -> Boolean
; (seclectors: 3)
; nustudent-name : NUStudent -> String
; nustudent-nuid : NUStudent -> PosInt
; nustudent-hometown-loc : NUStudent -> Position

; Now, implement a function letter-header that accepts an NU student and
; produces the opening line of a letter ("Dear Alice Doe,").

; letter-header : NUStudent -> String
; produces the opening line of a letter

(check-expect (letter-header NUSTUDENT-ALICE) : "Dear alice, (1234; from (0,0),") 
(check-expect (letter-header NUSTUDENT-BOB) : "Dear bob, (98765; from (3,4),")
; (check-expect (letter-header NUSTUDENT-CHRIS) : "Dear chris,")

(define (letter-header nustudent) 
  (string-append "Dear "
                 (nustudent-name nustudent)
                 ","))

(define (letter-header nustudent)
  (string-append
   "Dear"
   (nustudent-name nustudent)
   " ("
   (number->string (nustudent-nuid nustudent))
   "; from"
   (loc->string (nustudent-hometown-loc nustudent))
   "),"))

; pos->string : Position -> String
; produces a "(x,y)" from a position

(check-expect (pos->string POS0) "0,0)")
(check-expect (pos->string POS1) "3,4)")

; (check-expect (how-far-from-home NUSTUDENT-ALICE) "alice is 

(define (posn->string p)
  (string-append
   "(" 
   (number->string (posn-x p))
   ", "
   (number->string (posn-y p))
   ")"))






