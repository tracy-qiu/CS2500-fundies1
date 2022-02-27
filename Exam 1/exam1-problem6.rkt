;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exam1-problem6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; q-u-both? : Letters -> Boolean
; checks if q and u are available
; returns #false when Letters has a "Q" but omits a "U"


(check-expect (q-u-both? (make-letters "A" "B" "C" "D" "E")) #true)
(check-expect (q-u-both? (make-letters "Q" "U" "C" "D" "E")) #true)
(check-expect (q-u-both? (make-letters "U" "A" "C" "D" "E")) #true)
(check-expect (q-u-both? (make-letters "J" "I" "Q" "L" "X")) #false)

(define (q-u-both? l)
  (cond
    [(and (availableQ? l) (availableU? l)) #true]
    [(and (not (availableQ? l)) (not (availableU? l))) #true]
    [(and (not (availableQ? l)) (availableU? l)) #true]
    [(and (availableQ? l) (not (availableU? l))) #false]))
    

(check-expect (availableQ? (make-letters "A" "B" "C" "D" "E")) #false)
(check-expect (availableQ? (make-letters "Q" "B" "C" "D" "E")) #true)
(check-expect (availableQ? (make-letters "J" "I" "Q" "L" "X")) #true) 
  
; availableQ? : Letters -> Boolean
; checks if "Q" is one of the letters in given Letters

(define (availableQ? l)
  (or (string=? (letters-center l) "Q")
      (string=? (letters-top l) "Q")
      (string=? (letters-left l) "Q")
      (string=? (letters-bottom l) "Q")
      (string=? (letters-right l) "Q")))


(check-expect (availableU? (make-letters "A" "B" "C" "D" "E")) #false)
(check-expect (availableU? (make-letters "Q" "U" "C" "D" "E")) #true)
(check-expect (availableU? (make-letters "J" "I" "Q" "L" "X")) #false)

; availableU? : Letters -> Boolean
; checks if "U" is one of the letters in given Letters

(define (availableU? l)
  (or (string=? (letters-center l) "U")
      (string=? (letters-top l) "U")
      (string=? (letters-left l) "U")
      (string=? (letters-bottom l) "U")
      (string=? (letters-right l) "U")))