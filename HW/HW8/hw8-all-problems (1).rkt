;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw8-all-problems (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;;

;; [TODO] Revise World and Letters as described above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step 3: The Backspace/Delete Key
;;
;; Update your program so that when the player presses Backspace (Windows) or
;; Delete (Mac), the game clears the last letter that they entered. The special
;; string "\b" stands for delete/backspace.
;;
;; Note: Ensure your program is "well-behaved" when the player presses
;; delete/backspace and there are no characters to delete.

;; [TODO] Revise your program to support for backspace/delete.

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

(define LITTLE-DICTIONARY (list "explain" "plain" "nail" "lap"))

;; [TODO] Revise your program to implement the five checks above.

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

