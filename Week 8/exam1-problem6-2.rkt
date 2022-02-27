;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exam1-problem6-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 6
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; Consider the following data definition that represents the available letters
;; in a simplified game of Spelling Bee (identical to the project you've worked
;; on before).

(define-struct letters [center left right top bottom])
;; A Letters is a (make-letter 1String 1String 1String 1String 1String)
;; Interpretation: the letters available in a game of Spelling Bee:
;; - center: the letter at the center that must be used
;; - left, right, top, bottom: the four other letters

(define (letters-template l)
  (... (letters-center l) ...
       (letters-left l) ...
       (letters-right l) ...
       (letters-top l) ...
       (letters-bottom l) ...))

;; The letter "Q" is typically followed by the letter "U". So, it would be
;; very unfortunate if the available letters had a "Q" and *not* a "U".
;; Design a function that produces #false when a Letters has a "Q" but
;; omits a "U".

;; Ensure you follow the complete function design recipe, including for any
;; appropriate helper functions you may choose to write.

;; [TODO] Function design

(define LETTERS-QU (make-letters "Z" "U" "W" "X" "Q"))
(define LETTERS-NEITHER (make-letters "A" "B" "C" "D" "E"))
(define LETTERS-U (make-letters "A" "B" "C" "D" "U"))
(define LETTERS-Q (make-letters "Q" "A" "W" "X" "Y"))

;; letters-has-usable-q? : Letters -> Boolean
;; Produces #true if Letters has both "Q" and "U", or does not have a "Q".

(check-expect (letters-has-usable-q? LETTERS-QU) #true)
(check-expect (letters-has-usable-q? LETTERS-NEITHER) #true)
(check-expect (letters-has-usable-q? LETTERS-U) #true)
(check-expect (letters-has-usable-q? LETTERS-Q) #false)

(define (letters-has-usable-q? l)
  (or (not (letters-contains-q? l))
      (letters-contains-u? l)))

; letters-contains-q? : Letters -> Boolean
; returns #true if available letters contains "Q"

(check-expect (letters-contains-q? LETTERS-QU) #true)
(check-expect (letters-contains-q? LETTERS-NEITHER) #false)
(check-expect (letters-contains-q? LETTERS-U) #false)
(check-expect (letters-contains-q? LETTERS-Q) #true)

(define (letters-contains-q? l)
  (or (string=? (letters-center l) "Q")
      (string=? (letters-left l) "Q")
      (string=? (letters-right l) "Q")
      (string=? (letters-top l) "Q")
      (string=? (letters-bottom l) "Q")))


; letters-contains-u? : Letters -> Boolean
; returns #true if available letters contains "U"

(check-expect (letters-contains-u? LETTERS-QU) #true)
(check-expect (letters-contains-u? LETTERS-NEITHER) #false)
(check-expect (letters-contains-u? LETTERS-U) #true)
(check-expect (letters-contains-u? LETTERS-Q) #false)

(define (letters-contains-u? l)
  (or (string=? (letters-center l) "U")
      (string=? (letters-left l) "U")
      (string=? (letters-right l) "U")
      (string=? (letters-top l) "U")
      (string=? (letters-bottom l) "U")))







