;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 11/5/21

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

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

;; <solution>
;; Needed for the call to (tests) at the end of the file.
(require test-engine/racket-tests)

(define LETTERS-1 (list "l" "o" "e" "b" "w"))
(define LETTERS-2 (list "z" "x" "w" "q" "y")) ;; An essentially impossible set of letters!
(define LETTERS-3 (list "b" "s" "d" "a" "e" "t" "g"))
(define LETTERS-4 (list "j" "l" "s" "i" "a" "n" "t"))

;; </solution>

;; Design a function to display Letters as an Image. Feel free to layout the
;; letters in any way you like. However, the required letter must be
;; distinguished in some way (e.g., by position, color, font, etc).
;;
;; NOTE: You must not use substring in this function, or in any helper function
;; that it employs.

;; [TODO] Function design recipe

;; <solution>
;; word->image : String -> Image
;;
;; Displays a word as an image with the color and size that we want
(define (word->image w)
  (text w 20 "orange"))

(check-expect (word->image "hello") (text "hello" 20 "orange"))

;; letter->image : 1String -> Image
;;
;; Displays a single letter as an image
(define (letter->image s)
  (overlay (text s 20 "blue")
           (square 30 "outline" "black")))

(check-expect (letter->image "X") (overlay (text "X" 20 "blue") (square 30 "outline" "black")))

;; letters->image : [NE-LIST-of 1String] -> Image
;;
;; Displays a Letters.
(define (letters->image l)
  (foldr beside (square 10 "outline" "black") (map letter->image l)))

(check-expect (letters->image LETTERS-1)
              (foldr beside (square 10 "outline" "black") (map letter->image LETTERS-1)))
(check-expect (letters->image LETTERS-3)
              (foldr beside (square 10 "outline" "black") (map letter->image LETTERS-3)))
(check-expect (letters->image LETTERS-4)
              (foldr beside (square 10 "outline" "black") (map letter->image LETTERS-4)))
;; </solution>

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

;; <solution>
(define-struct world [letters word-so-far constructed-words])
;; A World is a (make-world [NE-List-of 1String] String [List-of String])
;;
;; Interpretation: The letters on the board, the word the user has typed so far,
;; and the words the user has already entered, in a string with newlines.


;; world-template : World -> ?
(define (world-template w)
  (... (world-letters w) ...
       (world-word-so-far w) ...
       (world-constructed-words w) ...))

(define INIT-WORLD-1 (make-world LETTERS-1 "" (list )))
(define INIT-WORLD-2 (make-world LETTERS-1 "bell" (list )))
(define INIT-WORLD-3 (make-world LETTERS-1 "" (list "bell")))

;; </solution>

;; [TODO] Function design recipe

;; <solution>

;; world->image : World -> Image
;;
;; Display a World.
(define (world->image w)
  (above
   (text "How many words can you construct" 14 "red")
   (beside/align
    "top"
    (above (letters->image (world-letters w)) (word->image (world-word-so-far w)))
    (foldr beside (square 10 "outline" "black") (map word->image (world-constructed-words w))))))
(check-expect (world->image INIT-WORLD-1)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image ""))
                (foldr beside (square 10 "outline" "black") (map word->image (list )))))) 
(check-expect (world->image INIT-WORLD-2)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image "bell"))
                (foldr beside (square 10 "outline" "black") (map word->image (list )))))) 
(check-expect (world->image INIT-WORLD-3)
              (above
               (text "How many words can you construct" 14 "red")
               (beside/align
                "top"
                (above (letters->image LETTERS-1)
                       (word->image ""))
                (foldr beside (square 10 "outline" "black") (map word->image (list "bell")))))) 
;; </solution>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3:  Player Interactions

;; In a game of Spelling Bee, the player can either enter a letter to add to the
;; current word, or press the "Enter" key to complete the word. Design a
;; function with the following signature:

;; key-pressed : World KeyEvent -> World

;; [TODO] Complete the function design recipe

;; <solution>
;; available? : [List-of Strings] String -> Boolean
;;
;; Check if a string is available in a list 

(define (available? l givenString)
  (ormap [λ (s) (string=? s givenString)] l)) 

(check-expect (available? LETTERS-1 "b") #true)
(check-expect (available? LETTERS-2 "a") #false)
(check-expect (available? LETTERS-3 "e") #true)

;; key-pressed : World KeyEvent -> World
;;
;; Produce a new World in reaction to a key-press.
(define (key-pressed w k)
  (cond
    ; 1. checks if entered letter is one of the available letters 
    [(available? (world-letters w) k) 
     (make-world (world-letters w)
                 (string-append (world-word-so-far w) k)
                 (world-constructed-words w))]
    [(and (string=? k "\r")
          ; 2. checks if the word so far contains the center letter
          (string-contains?
           (list-ref (world-letters w) (* 0.5 (- (length (world-letters w)) 1))) 
           (world-word-so-far w))
          ; 3. checks if word so far is a dictionary word
          (available? (read-words "words.txt") (world-word-so-far w))
          ; 4. checks if the word so far is at least four letters long
          (>= (string-length (world-word-so-far w)) 4) 
          ; 5. checks if the word so far is not a duplicate of an already entered word 
          (not (available? (world-constructed-words w) (world-word-so-far w))))
     (make-world (world-letters w) 
                 ""
                 (append (world-constructed-words w) (list (world-word-so-far w))))]
    [(and (string=? k "\b")
          (not (= (string-length (world-word-so-far w)) 0))) 
     (make-world (world-letters w)
                 (substring (world-word-so-far w) 0 (- (string-length (world-word-so-far w)) 1))
                 (world-constructed-words w))]
    [else w]))

(check-expect (key-pressed INIT-WORLD-2 "\r") INIT-WORLD-3)  
(check-expect (key-pressed INIT-WORLD-3 "x") INIT-WORLD-3)
(check-expect (key-pressed INIT-WORLD-1 "b") (make-world LETTERS-1 "b" (list )))
(check-expect (key-pressed INIT-WORLD-2 "\b") (make-world LETTERS-1 "bel" (list ))) 
 
#|
; dont need these because available-letter? dictionary-word? and duplicate-word? all do the same thing
; so just replaced those three functions with available?
; they all do the same thing checking if string exists in list

(define (dictionary-word? w)
  (ormap [λ (s) (string=? s world-letters w)] (read-words "words.txt")))

(define (duplicate-word? w)
  (ormap [λ (s) (string=? s world-letters w)] (read-words words.txt)))
|#

;; </solution>

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