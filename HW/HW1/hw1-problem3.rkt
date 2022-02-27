;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; In western classical music, tones are typically placed on a scale called
;; the twelve-tone scale. We use non-negative integers to refer to each tone.
;; For example 60 refers to the tone called "C" (or the "do" in "do-re-mi")
;; near the middle of a piano, whereas 61 refers to the tone one unit higher.
;; We consider two tones with a gap of a multiple of 12 units between them as
;; equivalent. For example, the tones 0, 60 and 84 are all equivalent: they are
;; all the tone "C". However, tones 60 and 67 are not equivalent.

;; Part A

;; Define a function called tone-class which consumes a single tone as an
;; argument, and produces its *class*,  which is the smallest non-negative
;; integer that is equivalent to the tone. For example, the class of 60 is 0,
;; the class of 61 is 1, and the class of 0 is 0 itself. You must also write 
;; three examples for your function.
;;
;; Hint: Since there are 12 classes starting with zero, you can calculate the
;; class as the remainder. Try looking for relevant functions in the DrRacket
;; Help Desk.

;; [TODO] Function definition
;; [TODO] Three examples

; tone-class : Number -> Number
; given a single tone, turns the class (non negative integer that is equivalent to the tone)

(check-expect (tone-class 60) 0)
(check-expect (tone-class 61) 1)
(check-expect (tone-class 62) 2)

(define (tone-class tone)
  (remainder tone 12))
    
;; Part B

;; The distance between two tones is how far apart they are, while keeping
;; equivalence in mind. Since there are 12 tone classes, the maximum distance
;; between any pair of tones is 12. However, there are two distances you can
;; produce, depending on which tone you consider first:
;;
;; - The distance between tones 60 and 63 is either 3 (counting up) or 9
;;   (counting down).
;; - The distance between 60 and 75 is also either 3 or 9.
;; - The distance between 63 and 70 is 5 or 7.

;; Write a function called tone-distance which consumes two tones as arguments,
;; and produces their distance (either distance), as defined above.
;; Write three examples for tone-distance.

;; [TODO] Function definition
;; [TODO] Thee examples

; tone-distance : Number Number -> Number
; given two tones, returns the counting up distance of the tones

(check-expect (tone-distance 60 63) 3)
(check-expect (tone-distance 60 74) 2)
(check-expect (tone-distance 63 70) 5)

(define (tone-distance tone1 tone2)
  (cond
    [(<= (remainder (- tone2 tone1) 12) 6) (- (tone-class tone2) (tone-class tone1))]
    [else (- 12 (- (tone-class tone2) (tone-class tone1)))]))


;; Part C

;; On a piano keyboard, the each class of twelve tones (a.k.a., an octave) are
;; placed in a standard pattern of eight white and five black keys. If you are
;; not familiar with this pattern, here is a picture of a piano keyboard:
;;
;; https://en.wikipedia.org/wiki/Musical_keyboard#/media/File:Klaviatur-3-en.svg
;;
;; Write a function called keyboard that consumes the height and width
;; of the white keys, and produces an image that looks like a piano octave.
;; The black keys are roughly half the width and about 3/4 the length of the
;; white keys.
;;
;; Note: The picture linked above labels the white keys. Your image does not
;; have to do so.
;;
;; Hint 1: The overlay/align function may be very helpful.
;;
;; Hint 2: You can use "transparent" as a color for a rectangle.

(require 2htdp/image)
(require 2htdp/universe)

(define BASE (rectangle 30 50 "solid" "transparent"))
(define WHITE-KEYS (rectangle 20 6 "solid" "white"))
(define BLACK-KEYS (rectangle 15 3 "solid" "black"))

; draw-keyboard : Number Number -> Image
; given height and width of the white keys, produces piano octave image


(define (draw-keyboard white-width white-height)
  (overlay
   (overlay/align "middle" "top"
                  (rectangle  (* 0.5 white-width) (* 0.75 white-height) "solid" "black")
                  (beside 
                   (overlay/align "middle" "top"
                                  (rectangle  (* 0.5 white-width) (* 0.75 white-height)
                                              "solid" "black")
                                  (beside
                                   (overlay/align "middle" "top"
                                                  (rectangle  (* 0.5 white-width)
                                                              (* 0.75 white-height) "solid" "black")
                                                  (beside
                                                   (rectangle  white-width white-height
                                                               "outline" "black")
                                                   (rectangle  white-width white-height
                                                               "outline" "black")))
                                   (beside
                                    (rectangle  white-width white-height
                                                "outline" "black")
                                    (rectangle  white-width white-height
                                                "outline" "black"))))


   
                   (overlay/align "middle" "top"
                                  (rectangle  (* 0.5 white-width) (* 0.75 white-height)
                                              "solid" "black")
                                  (beside
                                   (overlay/align "middle" "top"
                                                  (rectangle  (* 0.5 white-width)
                                                              (* 0.75 white-height) "solid" "black")
                                                  (beside
                                                   (rectangle  white-width white-height
                                                               "outline" "black")
                                                   (rectangle  white-width white-height
                                                               "outline" "black")))
                                   (beside
                                    (rectangle  white-width white-height
                                                "outline" "black")
                                    (rectangle  white-width white-height
                                                "outline" "black"))))))
   (rectangle (* 8 white-width) white-height "solid" "white")))

   
   


