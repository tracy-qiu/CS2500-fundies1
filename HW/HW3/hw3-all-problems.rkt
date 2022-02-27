;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-all-problems) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; Spelling Bee is a vocabulary game hosted by the New York Times. The project
;; for this class is to build a program similar to Spelling Bee over the course
;; of several homework assignments. Before reading any further, you should play
;; the New York Times' Spelling Bee to understand the rules of the game:
;;
;; https://www.nytimes.com/puzzles/spelling-bee
;;
;; Your first version of Spelling Bee, which you will design for this
;; assignment, will make several simplifications:
;;
;; 1. You will construct words using five letters, and not seven,
;; 2. You will be able to enter nonsensical words,
;; 3. You will not be able to correct mistakes (i.e., backspace will not
;;      work),
;; 4. You will be able enter the same word several times, and
;; 5. You won't keep score.
;;
;; In later homework, you will remove these restrictions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Representing and Displaying Available Letters
;;

;; Design data called Letters that holds the five letters that are
;; available for your simplified Spelling Bee. Note that one of the letters is
;; distinguished as the required letter (i.e., the letter displayed at the
;; center).
;;
;; Tip: Use "1String" to refer to a one-character string in your data 
;; definition.

;; [TODO] Data design recipe

(define-struct letters [l1 l2 l3 l4 lr])

; A Letter is a (make-letters 1String 1String 1String 1String 1String)
; - where l1 is the first letter in the Spelling Bee
; - where l2 is the second letter in the Spelling Bee
; - where l3 is the third letter in the Spelling Bee
; - where l4 is the fourth letter in the Spelling Bee
; - where lr is the required letter in the Spelling Bee
; Interpretation: five letters for Spelling Bee with one distinguished required letter

(define LETTERS-ABCDE (make-letters "a" "b" "c" "d" "e"))
(define LETTERS-VWXYZ (make-letters "v" "w" "x" "y" "z"))

(define (letters-temp l)
  (... (letters-l1 l) ...
       (letters-l2 l) ... 
       (letters-l3 l) ...
       (letters-l4 l) ...
       (letters-l5 l) ...))


;; Design a function to display Letters as an Image. Feel free to layout the
;; letters in any way you like. However, the required letter must be
;; distinguished in some way (e.g., by position, color, font, etc).
;;
;; NOTE: You must not use substring in this function, or in any helper function
;; that it employs.

;; [TODO] Function design recipe

; display-letters : Letters -> Image
; displays Letters as an Image 

(define TEXT (string-append "  " (letters-l1 LETTERS-ABCDE) "\n" 
                            (letters-l2 LETTERS-ABCDE)
                            "   " (letters-l3 LETTERS-ABCDE) "\n"
                            "  " (letters-l4 LETTERS-ABCDE)))
(define background (square 500 "solid" "white"))
(define FONT 20) 

(check-expect
 (display-letters LETTERS-ABCDE)
 (place-image
  (text (letters-lr LETTERS-ABCDE) FONT "red")
  50 50
  (place-image
   (text TEXT FONT "black") 
   50 50
   background)))

(define (display-letters l)
  (place-image
   (text (letters-lr LETTERS-ABCDE) FONT "red")
   50 50
   (place-image
    (text (string-append "  " (letters-l1 l) "\n" 
                         (letters-l2 l)
                         "   " (letters-l3 l) "\n"
                         "  " (letters-l4 l))
          FONT "black")
    50 50
    background)))

                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: Representing the World

;; A game of Spelling Bee must track the available letters and the partial word
;; that the player has entered. Design data called World that holds
;; this data, and design a function called world->image that displays a World as
;; an image. You can produce any image that you like, but it should clearly show
;; both the available letters and the partial word.

;; Note: The final step of this homework has you revise the data definition for
;; World. We recommend completing this step before the revision. However, we
;; recommend you read the whole assignment first so that you can understand
;; the revision that you will have to do.

;; [TODO] Data design recipe

(define-struct world [letters word prevwords])

; A World is a (make-world letters String)
; - where letters are the available letters in the Spelling Bee
; - word is the partial word that the player has entered
; Interpretation: a tracks available letters and the partial word
; and words so far the player has entered 

(define WORLD-1 (make-world LETTERS-ABCDE "bead" ""))

(define (world-temp w)
  (... (world-letters w) ...
       (world-word w) ...
       (world-prevwords w) ...))

#|
(define (world-temp w)
  (... (letters-l1 (world-letters w)) ...
       (letters-l2 (world-letters w)) ...
       (letters-l3 (world-letters w)) ...
       (letters-l4 (world-letters w)) ...
       (letters-lr (world-letters w)) ...
       (world-word w) ...))
|#                

;; [TODO] Function design recipe

; world->image : World -> Image
; displays a World as an Image

(check-expect (world->image WORLD-1)
              (place-image
               (text (string-append
                      "Words so far:\n"
                      (world-word WORLD-1)
                      "\n"
                      (world-prevwords WORLD-1))
                     FONT "black")
               200 60
               (display-letters (world-letters WORLD-1))))

(define (world->image w)
  (place-image
   (text (string-append
          "Words so far:\n"
          (world-word w)
          "\n"
          (world-prevwords w))
         FONT "black")
   200 60 
   (display-letters (world-letters w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3:  Player Interactions

;; In a game of Spelling Bee, the player can either enter a letter to add to the
;; current word, or press the "Enter" key to complete the word. Design a
;; function with the following signature:

;; key-pressed : World KeyEvent -> World
; simulates the Spelling Bee producing a new world based on the input key event

;; [TODO] Complete the function design recipe

(define WORLD-0 (make-world LETTERS-ABCDE "bad" "bead"))

(check-expect (key-pressed WORLD-0 "\r") WORLD-0)
(check-expect (key-pressed WORLD-0 "b") (world->image (make-world LETTERS-ABCDE "b" "")))
(check-expect (key-pressed WORLD-0 "z") WORLD-0)

(define (key-pressed w k) 
  (cond
    [(string=? k (letters-l1 (world-letters w))) (world->image (make-world (world-letters w) k ""))]
    [(string=? k (letters-l2 (world-letters w))) (world->image (make-world (world-letters w) k ""))]
    [(string=? k (letters-l3 (world-letters w))) (world->image (make-world (world-letters w) k ""))]
    [(string=? k (letters-l4 (world-letters w))) (world->image (make-world (world-letters w) k ""))] 
    [(string=? k (letters-lr (world-letters w))) (world->image (make-world (world-letters w) k ""))]
    [(string=? k "\r") w]
    ; [(and (string=? k "\r") (string-contains (world-word w) (letters-lr (world-word w)))  w]
    [else w])) 

;; When the key is an an available letter, key-pressed should produce a new
;; world that adds that letter to the end of the current word. If the key is not
;; an available letter, it should produce the previous world. If the key is the
;; special string "\r" (which stands for the Enter key), it should produce a new
;; world with the empty string for the current word, as long as the current word
;; contains the center letter. If the player presses any other key, produce the
;; previous world unchanged. In other words, your program should not produce an
;; error if the player presses the "wrong" key.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 4: World Program

;; At this point, you have enough in place to play a basic game. Use the
;; following world program to play spelling bee.

;; play : World -> World
;;
;; Uses big-bang to play a game of Spelling Bee, given Letters.

(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed))) 

;; [TODO] Click Run. Then, in Interactions, use the function play on an 
;; example of Letters.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Keeping Track of Words

;; The Spelling Bee program that you have so far doesn't even keep track of the
;; words you've entered! We really need the game to show the words that
;; you've already entered. For example, if you previously entered the words
;; "bell" and "well", and the available letters are "b", "o", "w", "e", and "l",
;; with "l" at the center, the program should really display something that
;; looks like this:
;;
;;   O     Words so far:
;; B L W   bell
;;   E     well
;;
;; Here is how you can display text on multiple lines. In a string, the special
;; character "\n" represents a new line. So, the string
;; "Words so far:\nbell\nwell" represents a string that appears on three lines.
;;
;; Modify the data World to include the words so far, represented
;; as a string. You will also have modify the functions you've written so far.
;; But, if you carefully followed the design recipe, the changes will be minor.
;;
;; NOTE: Your program will show the words entered so far, but it will not
;; check that the player does not enter a duplicate word. We will address that
;; problem later.

;; [TODO] Modifications

