;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 11/5/21

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction

;; In this assignment, you will continue working on Spelling Bee (Homework 3).
;; You will rely on several concepts and techniques that you've learned since
;; Homework 3, to build a version of Spelling Bee that is significantly closer
;; to the real game. In particular, you will:
;;
;; 1. Move to a seven-letter Spelling Bee, instead of a five-letter game,
;;
;; 2. Implement scoring,
;;
;; 3. Support the backspace / delete key, so that players can correct their
;;    word, and
;;
;; 4. Check that the entered word is in a dictionary.

;; NOTE #1: Follow the "no halloween colors" and 2+ check-expects rule for
;; all function designs.
;;
;; NOTE #2: In the original Spelling Bee, we restricted you from using certain
;; functions. For this assignment, the only restricted functions are those
;; in the class style guide.
;;
;; NOTE #3: Despite having fewer restrictions, we still expect good program
;; design. For example, lists are the appropiate type of data to represent
;; scored words and available letters, and this assignment asks you to update
;; your program to use lists. It is possible to immediately convert a 
;; [List-of 1String] into a String. But, that is not the approach we want you
;; to take. We want you to use list abstractions when possible.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 0: Meet With Your Partner

;; You must do this assignment with your assigned partner. However, since you
;; did the previous stage alone, that means you have multiple implementations to
;; Spelling Bee to use as a starting point. Which one will you use? Making that
;; decision is part of the assignment.
;;
;; Note: If neither you nor your partner did well on HW3, we strongly encourage
;; you to use our HW3 sample solution as a starting point.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 1: Introducing Lists
;;
;; When we worked on HW3, we did not know about lists, which are fundamental to
;; good program design. Instead, we played tricks with strings, such as using
;; newlines to separate found words. Now that we are familiar with lists, we
;; are going to modify Spelling Bee to use them in two places:
;;
;; a. Revise your data definition for Letters to either:
;;
;;    - Represent the available letters as an [NE-List-of 1String], or
;;
;;    - Represent the non-center letters as a [List-of 1String]
;;
;; b. Revise your World data definition to represent the list of words found
;;    as a [List-of String]. Thus you should no longer use "\n" in your
;;    examples of World.
;; [TODO] Revise World and Letters as described above.

;; <solution>
;; Needed for the call to (tests) at the end of the file.
(require test-engine/racket-tests)
;; A letters is a [NE-List-of 1String]
;; Interpretation: A non-empty list that represents any number of letters
;; that are available for game of Spelling Bee. The center letter of the list is the required letter
(define LETTERS-1 (list "l" "o" "e" "b" "w"))
(define LETTERS-2 (list "z" "x" "w" "q" "y")) ;; An essentially impossible set of letters!
(define LETTERS-3 (list "b" "s" "d" "a" "e" "t" "g"))
(define LETTERS-4 (list "j" "l" "s" "i" "a" "n" "t"))
(define LETTERS-5 (list "e" "x" "p" "l" "a" "i" "n"))

(define (los-template los)
  (...
   (cond
     [(empty? (rest los)) ...]
     [(cons? (rest los)) (... (first los) ...
                              (los-template (rest los)) ...)])))

;; word->image : String -> Image
;;
;; Displays a word as an image with the color and size that we want
(define (word->image w)
  (text w 20 "orange"))

(check-expect (word->image "hello") (text "hello" 20 "orange"))
(check-expect (word->image "snail") (text "snail" 20 "orange"))

;; letter->image : 1String -> Image
;;
;; Displays a single letter as an image
(define (letter->image s)
  (overlay (text s 20 "blue")
           (square 30 "outline" "black")))

(check-expect (letter->image "X") (overlay (text "X" 20 "blue") (square 30 "outline" "black")))
(check-expect (letter->image "h") (overlay (text "h" 20 "blue") (square 30 "outline" "black")))

(define SQUARE (square 5 "outline" "black"))
;; letters->image : [NE-LIST-of 1String] -> Image
;;
;; Displays a Letters.
(define (letters->image l)
  (foldr beside SQUARE (map letter->image l)))

(check-expect (letters->image LETTERS-1)
              (foldr beside SQUARE (map letter->image LETTERS-1)))
(check-expect (letters->image LETTERS-3)
              (foldr beside SQUARE (map letter->image LETTERS-3)))
(check-expect (letters->image LETTERS-4)
              (foldr beside SQUARE (map letter->image LETTERS-4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 2: More Letters
;;
;; The update that you made in Step 1 should allow your program to support any
;; number of available letters (though you need at least one letter at the
;; center). However, your old examples use exactly five letters.
;;
;; 1. Construct new examples and check-expects that have varying numbers of
;;    available letters.
;;
;;  2. Modify letters->image so that it either:
;;
;;     - Assumes that there are exactly seven letters (i.e., as in real
;;       Spelling Bee), or
;;
;;     - Supports any number of available letters (this is not required)

;; [TODO] New examples and check-expects with varying numbers of available
;; letters.

;; [TODO] Update letters->image to display seven letters (or optionally,
;; any number of letters)

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

(define INIT-WORLD-1 (make-world LETTERS-1 "" (list)))
(define INIT-WORLD-2 (make-world LETTERS-1 "bell" (list)))
(define INIT-WORLD-3 (make-world LETTERS-1 "" (list "bell")))
(define INIT-WORLD-4 (make-world LETTERS-4 "snai" (list)))
(define INIT-WORLD-5 (make-world LETTERS-4 "nail" (list "snail")))
(define INIT-WORLD-6 (make-world LETTERS-4 "" (list "nail" "snail")))
(define INIT-WORLD-7 (make-world LETTERS-4 "na" (list "snail")))
(define INIT-WORLD-8 (make-world LETTERS-4 "snail" (list "snail")))
(define INIT-WORLD-9 (make-world LETTERS-5 "e" (list)))

;; </solution>

;; [TODO] Function design recipe

;; <solution>

;; world->image : World -> Image
;;
;; Display a World.

(define (world->image w)
  (overlay
   (above
    (text "SCORE: " 14 "red")
    (text (number->string (score-tracker w)) 14 "red")
    (letters->image (world-letters w))
    (word->image (world-word-so-far w))
    (text "Words so far:" 12 "red")
    (foldr above empty-image (map word->image (world-constructed-words w))))
   (rectangle 500 500 "solid" "gray")))

(check-expect (world->image INIT-WORLD-1)
              (overlay
               (above
                (text "SCORE: " 14 "red")
                (text (number->string (score-tracker INIT-WORLD-1)) 14 "red")
                (letters->image (world-letters INIT-WORLD-1))
                (word->image (world-word-so-far INIT-WORLD-1))
                (text "Words so far:" 12 "red")
                (foldr above empty-image (map word->image (world-constructed-words INIT-WORLD-1))))
               (rectangle 500 500 "solid" "gray")))
(check-expect (world->image INIT-WORLD-2)
              (overlay
               (above
                (text "SCORE: " 14 "red")
                (text (number->string (score-tracker INIT-WORLD-2)) 14 "red")
                (letters->image (world-letters INIT-WORLD-2))
                (word->image (world-word-so-far INIT-WORLD-2))
                (text "Words so far:" 12 "red")
                (foldr above empty-image (map word->image (world-constructed-words INIT-WORLD-2))))
               (rectangle 500 500 "solid" "gray")))
(check-expect (world->image INIT-WORLD-3)
              (overlay
               (above
                (text "SCORE: " 14 "red")
                (text (number->string (score-tracker INIT-WORLD-3)) 14 "red")
                (letters->image (world-letters INIT-WORLD-3))
                (word->image (world-word-so-far INIT-WORLD-3))
                (text "Words so far:" 12 "red")
                (foldr above empty-image (map word->image (world-constructed-words INIT-WORLD-3))))
               (rectangle 500 500 "solid" "gray")))

;; </solution>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3:  Player Interactions
;; Step 3: The Backspace/Delete Key
;;
;; Update your program so that when the player presses Backspace (Windows) or
;; Delete (Mac), the game clears the last letter that they entered. The special
;; string "\b" stands for delete/backspace.
;;
;; Note: Ensure your program is "well-behaved" when the player presses
;; delete/backspace and there are no characters to delete.

;; [TODO] Revise your program to support for backspace/delete.

;; available? : [List-of Strings] String -> Boolean
;;
;; Check if a string is available in a list 
(check-expect (available? LETTERS-1 "b") #true)
(check-expect (available? LETTERS-2 "v") #false)
(check-expect (available? LETTERS-3 "e") #true)

(define (available? l givenString)
  (ormap [λ (s) (string=? s givenString)] l)) 

;; key-pressed : World KeyEvent -> World
;;
;; Produce a new World in reaction to a key-press.
(check-expect (key-pressed INIT-WORLD-2 "\r") INIT-WORLD-3)  
(check-expect (key-pressed INIT-WORLD-3 "x") INIT-WORLD-3)
(check-expect (key-pressed INIT-WORLD-1 "b") (make-world LETTERS-1 "b" (list)))
(check-expect (key-pressed INIT-WORLD-2 "\b") (make-world LETTERS-1 "bel" (list)))

(define (key-pressed w k)
  (cond
    [(available? (world-letters w) k) ; checks if entered letter is one of the available letters 
     (make-world (world-letters w)
                 (string-append (world-word-so-far w) k)
                 (world-constructed-words w))]
    [(and (string=? k "\r")
          (contains-letter w)
          (dictionary-word? w)
          (correct-length? w)
          (duplicate? w))
     (make-world (world-letters w) 
                 ""
                 (append (world-constructed-words w) (list (world-word-so-far w))))]
    [(and (string=? k "\b")
          (not (= (string-length (world-word-so-far w)) 0))) 
     (make-world (world-letters w)
                 (substring (world-word-so-far w) 0 (- (string-length (world-word-so-far w)) 1))
                 (world-constructed-words w))]
    [else w]))

;; contains-letter : World -> Boolean
;; checks if the word so far contains the center letter
(check-expect (contains-letter INIT-WORLD-1) #false)
(check-expect (contains-letter INIT-WORLD-2) #true)
(check-expect (contains-letter INIT-WORLD-5) #true)
(define (contains-letter w)
  (string-contains?
   (list-ref (world-letters w) (* 0.5 (- (length (world-letters w)) 1))) 
   (world-word-so-far w)))

;; dictionary-word? : World -> Boolean
;; checks if word so far is a dictionary word
(check-expect (dictionary-word? INIT-WORLD-4) #false)
(check-expect (dictionary-word? INIT-WORLD-2) #true)
(check-expect (dictionary-word? INIT-WORLD-5) #true)
(define (dictionary-word? w)
  (available? (read-words "words.txt") (world-word-so-far w))) 

;; correct-length? : World -> Boolean
;; checks if the word so far is at least four letters long
(check-expect (correct-length? INIT-WORLD-7) #false)
(check-expect (correct-length? INIT-WORLD-2) #true)
(check-expect (correct-length? INIT-WORLD-6) #false)
(define (correct-length? w)
  (>= (string-length (world-word-so-far w)) 4)) 

;; duplicate? : World -> Boolean
;; checks if the word so far is not a duplicate of an already entered word 
(check-expect (duplicate? INIT-WORLD-7) #true)
(check-expect (duplicate? INIT-WORLD-8) #false)
(check-expect (duplicate? INIT-WORLD-6) #true)
(define (duplicate? w)
  (not (available? (world-constructed-words w) (world-word-so-far w))))

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
;; Step 4: More Checks: Duplicate Words, 4+ Letter-Words, Dictionary Word
;;
;; Revise your program to ensure that the word entered by the player is:
;; 1. An available letter (already done in HW3),
;; 2. Contains the center letter (already done in HW3),
;; 3. A dictionary word,
;; 4. At least four letters long, and
;; 5. Is not a duplicate of an already entered word.
;;
;; We've given you a file called words.txt, which you can use as a dictionary.
;; It is not a comprehensive dictionary, but has roughly 50,000 words. Every
;; line of the file is a word, and they are arranged in alphabetical order
;; (technically, lexicographic order), if W1 appears before W2 in the file,
;; then (string<? W1 W2) is true.
;;
;; Suggestion #1: you can use the read-lines function in the 2htdp/batch-io
;; library to read the dictionary to a constant.
;;
;; Suggestion #2: It is very difficult to work with a list of 50,000 words.
;; So, to get started we recommend defining a small dictionary of words, and
;; then replacing it the the code that reads words from the file. E.g.,

;; [TODO] Revise your program to implement the five checks above.
;; play : World -> World
;; Uses big-bang to play a game of Spelling Bee, given Letters.
(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 5: Scoring a Game
;;
;; Finally, revise your program to display the current score. The score for
;; each word is:
;;
;; 1. One point for a four-letter word,
;;
;; 2. An additional point for every additional letter beyond the first four, and
;;
;; 3. An additional seven bonus points for using all seven letters.

;; [TODO] Revise your program to support scoring.
;; score-tracker : World -> Number
;; Given a world, calculates the score of the constructed words

(check-expect (score-tracker INIT-WORLD-1) 0)  
(check-expect (score-tracker INIT-WORLD-2) 0)
(check-expect (score-tracker INIT-WORLD-3) 1)
(check-expect (score-tracker INIT-WORLD-4) 0)
(check-expect (score-tracker INIT-WORLD-5) 2)
(check-expect (score-tracker INIT-WORLD-6) 3)

(define (score-tracker w)
  (foldr + 0 (map score-counter (world-constructed-words w))))

;; score-counter : String -> Number 
;; Given a word, determines how many score points it yields
(check-expect (score-counter "snail") 2)  
(check-expect (score-counter "nail") 1)
(check-expect (score-counter "steast") 3)
(check-expect (score-counter "explain") 11)
(check-expect (score-counter "") 0)

(define (score-counter l)
  (if (< (string-length l) 4)
      0
      (if (seven-unique-letters? l)
          (+ 7 (- (string-length l) 3))
          (- (string-length l) 3))))


;; seven-unique-letters? : String -> Boolean 
;; Given a word, determines if it uses 7 unique letters
(check-expect (seven-unique-letters? "snail") #false)  
(check-expect (seven-unique-letters? "nail") #false)
(check-expect (seven-unique-letters? "steast") #false)
(check-expect (seven-unique-letters? "explain") #true)
(check-expect (seven-unique-letters? "") #false)

(define (seven-unique-letters? l)
  (>= (count-unique-chars (list) l) 7))

;; count-unique-chars : [List-of String] String -> Number 
;; Given a word and a list of current characters, counts the number of
;; unique characters in the given word that are also no in the given list
(check-expect (count-unique-chars (list) "snail") 5)  
(check-expect (count-unique-chars (list) "nail") 4)
(check-expect (count-unique-chars (list) "steast") 4)
(check-expect (count-unique-chars (list) "explain") 7)
(check-expect (count-unique-chars (list) "") 0)
(check-expect (count-unique-chars (list "a" "i" "l") "snail") 2)  
(check-expect (count-unique-chars (list "a" "i" "l") "nail") 1)
(check-expect (count-unique-chars (list "e" "a" "s" "t") "steast") 0)
(check-expect (count-unique-chars (list "a" "i" "n") "explain") 4) 

(define (count-unique-chars list l)
  (if (string=? l "")
      0
      (if (available? list (substring l 0 1))
          (count-unique-chars list (substring l 1 (string-length l)))
          (+ 1 (count-unique-chars (cons (substring l 0 1) list)
                                   (substring l 1 (string-length l)))))))

