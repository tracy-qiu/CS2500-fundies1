;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Problem 1

;; Your goal is to design a slide show program. Every
;; slide will have a title and a list of bullets to
;; display, and each slide and item will be preceded
;; by a key press.

;; Consider the data definitions and examples below...

;; A ListOfStrings (LoS) is one of:
;; - '()
;; - (cons String LoS)
;; Interpretation: a list of strings

(define SLIDE-1-LOS
  (cons "Designing programs to solve problems"
        (cons "Building good habits for developing large systems with a team"
              (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-2-LOS
  (cons "Effectively using computers as tools"
        (cons "Breaking down problems" '())))

(define SLIDE-3-LOS
  (cons "Easy to start"
        (cons "Informative feedback"
              (cons "Functional programming is a useful paradigm" '()))))


(define-struct slide [title shown hidden])

;; A Slide is a (make-slide String LoS LoS)
;; Interpretation: a slide's title, what bullets
;; have been shown, and those that are hidden

(define SLIDE-1
  (make-slide
   "What is Fundies 1 About?"
   '() SLIDE-1-LOS))

(define SLIDE-1-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems" '())
   (cons "Building good habits for developing large systems with a team"
         (cons "CS: fundamental ideas, thinking" '()))))

(define SLIDE-1-NEXT-NEXT
  (make-slide
   "What is Fundies 1 About?"
   (cons "Designing programs to solve problems"
         (cons "Building good habits for developing large systems with a team" '()))
   (cons "CS: fundamental ideas, thinking" '())))

(define SLIDE-1-DONE
  (make-slide "What is Fundies 1 About?"
              SLIDE-1-LOS '()))

(define SLIDE-2
  (make-slide "What is Computer Science?"
              '() SLIDE-2-LOS))

(define SLIDE-3
  (make-slide "Why DrRacket?"
              '() SLIDE-3-LOS))


;; A Slideshow is one of:
;; - '()
;; - (cons Slide Slideshow)
;; Interpretation: an ordered slideshow

(define SLIDESHOW-1
  (cons SLIDE-1 (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-NEXT
  (cons SLIDE-1-NEXT (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-1-DONE
  (cons SLIDE-1-DONE (cons SLIDE-2 (cons SLIDE-3 '()))))

(define SLIDESHOW-2
  (cons SLIDE-2 (cons SLIDE-3 '())))

;; TODO #1: Complete the Design Recipe for LoS, Slide
;; and Slideshow by creating their templates.

;; <solution>
(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los) ...
      (los-temp (rest los)) ...])))

(define (slide-temp slide)
  (... (slide-title slide) ...
       (los-temp (slide-shown slide)) ...
       (los-temp (slide-hidden slide)) ...))

(define (slideshow-temp ss)
  (...
   (cond
     [(empty? ss) ...]
     [(cons? ss) ...
      (slide-temp (first ss)) ...
      (slideshow-temp (rest ss)) ...])))
;; </solution>

;; TODO #2: Design the function draw-slide that draws
;; a slide, showing only its title and unhidden content
;; on a large background of a fixed size. The text of
;; the bullets should be arranged above each other.
;; You are free to be creative about your slide design :)

;; <solution>

; draw-slide : Slide -> Image
; Visualize a slide

(define FONT-SIZE 15)
(define FONT-COLOR "black")
(define BG (empty-scene 500 500))

(check-expect
 (draw-slide SLIDE-1)
 (overlay
  (above
   (text/font "What is Fundies 1 About?"
              FONT-SIZE FONT-COLOR #false "default" "normal" "normal" #true)
   empty-image)
  BG))

(check-expect
 (draw-slide SLIDE-1-DONE)
 (overlay
  (above
   (text/font "What is Fundies 1 About?"
              FONT-SIZE FONT-COLOR #false "default" "normal" "normal" #true)
   (text "Designing programs to solve problems"
         FONT-SIZE FONT-COLOR)
   (text "Building good habits for developing large systems with a team"
         FONT-SIZE FONT-COLOR)
   (text "CS: fundamental ideas, thinking"
         FONT-SIZE FONT-COLOR))
  BG))

(define (draw-slide slide)
  (overlay
   (above
    (text/font (slide-title slide)
               FONT-SIZE FONT-COLOR #false "default" "normal" "normal" #true)
    (draw-bullets (slide-shown slide)))
   BG))

; draw-bullets : LoS -> Image
; Draw the bullets

(check-expect
 (draw-bullets '())
 empty-image)

(check-expect
 (draw-bullets (cons "a" (cons "b" '())))
 (above
  (text "a" FONT-SIZE FONT-COLOR)
  (text "b" FONT-SIZE FONT-COLOR)))

(define (draw-bullets los)
  (cond
    [(empty? los) empty-image]
    [(cons? los)
     (above
      (text (first los) FONT-SIZE FONT-COLOR)
      (draw-bullets (rest los)))]))
;; </solution>

;; TODO #3: Design the function draw-slideshow that
;; draws the slideshow’s first slide; if the slideshow
;; is complete, you should show "Fin" as the title to
;; an empty slide. Either way, this visualization should
;; be placed on a large background of fixed size.

;; <solution>
; draw-slideshow : Slideshow -> Image
; Visualize the slideshow

(define FIN "Fin")
(define FIN-SLIDE (make-slide FIN '() '()))

(check-expect
 (draw-slideshow '())
 (overlay
  (above
   (text/font FIN
              FONT-SIZE FONT-COLOR #false "default" "normal" "normal" #true)
   empty-image)
  BG))

(check-expect
 (draw-slideshow SLIDESHOW-1)
 (draw-slide SLIDE-1))

(define (draw-slideshow slideshow)
  (draw-slide
   (cond
     [(empty? slideshow) FIN-SLIDE]
     [(cons? slideshow) (first slideshow)])))
;; </solution>

;; TODO #4: Design the function advance-slide that
;; moves the first entry in a slide’s hidden content
;; to the end of its shown content if there is any
;; hidden content. As examples, look to SLIDE-1 ->
;; SLIDE-1-NEXT -> SLIDE-1-NEXT-NEXT -> SLIDE-1-DONE.
;; (Hint: the append function will be quite useful.)

;; <solution>
; advance-slide : Slide -> Slide
; Advance a slide

(check-expect
 (advance-slide SLIDE-1)
 SLIDE-1-NEXT)

(check-expect
 (advance-slide SLIDE-1-NEXT)
 SLIDE-1-NEXT-NEXT)

(check-expect
 (advance-slide SLIDE-1-NEXT-NEXT)
 SLIDE-1-DONE)

(check-expect
 (advance-slide SLIDE-1-DONE)
 SLIDE-1-DONE)

(define (advance-slide slide)
  (cond
    [(empty? (slide-hidden slide)) slide]
    [(cons? (slide-hidden slide))
     (make-slide
      (slide-title slide)
      (append
       (slide-shown slide)
       (cons (first (slide-hidden slide)) '()))
      (rest (slide-hidden slide)))]))
;; </solution>

;; TODO #5: Design the function slide-over? that
;; determines if a slide is over (none of its bullets
;; are hidden).

;; <solution>
; slide-over? : Slide -> Boolean
; Is the slide over?

(check-expect
 (slide-over? SLIDE-1) #false)

(check-expect
 (slide-over? SLIDE-1-NEXT) #false)

(check-expect
 (slide-over? SLIDE-1-NEXT-NEXT) #false)

(check-expect
 (slide-over? SLIDE-1-DONE) #true)

(define (slide-over? slide)
  (empty? (slide-hidden slide)))
;; </solution>

;; TODO #6: Design the function advance-slideshow that
;; either advances its first slide if it has more content
;; to be shown or moves onto the next slide if there
;; is one.

;; <solution>
; advance-slideshow : Slideshow -> Slideshow
; Advance the slideshow

(check-expect
 (advance-slideshow '())
 '())

(check-expect
 (advance-slideshow SLIDESHOW-1)
 SLIDESHOW-1-NEXT)

(check-expect
 (advance-slideshow SLIDESHOW-1-DONE)
 SLIDESHOW-2)

(define (advance-slideshow ss)
  (cond
    [(empty? ss) '()]
    [(cons? ss)
     (if (slide-over? (first ss))
         (rest ss)
         (cons (advance-slide (first ss))
               (rest ss)))]))
;; </solution>


;; TODO #7: Design the World program go-slideshow
;; that will advance a slideshow when any key is
;; pressed. The program should end (i.e., stop-when)
;; when there are no more slides left, showing
;; our elegant "Fin" as the last image.

;; <solution>
; go-slideshow : Slideshow -> Slideshow
; Play a slideshow

(define (go-slideshow slideshow)
  (big-bang slideshow
    [to-draw draw-slideshow]
    [on-key key-slideshow]
    [stop-when empty? draw-slideshow]))

; key-slideshow : Slideshow KeyEvent -> Slideshow
; Advances a slideshow, ignoring the key pressed

(check-expect
 (key-slideshow SLIDESHOW-1 "a")
 SLIDESHOW-1-NEXT)

(check-expect
 (key-slideshow SLIDESHOW-1-DONE " ")
 SLIDESHOW-2)

(define (key-slideshow ss ke)
  (advance-slideshow ss))
;; </solution>