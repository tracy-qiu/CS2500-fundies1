;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the following structure definitions:

#|
(define-struct guitar [brand-name color electric?])
(define-struct drum-kit [brand-name electric?])
(define-struct saxophone [brand-name])
(define-struct piano [brand-name])
|#

;; Part A

;; Design four data types called Guitar, DrumKit, Saxophone, and Piano: one for 
;; each structure. Ensure you complete all steps of the data design recipe for 
;; all four data types.

;; [TODO] Four complete data designs

(define-struct guitar [brand-name color electric?])

; A Guitar is a (make-guitar String String Boolean)
; - where brand-name is the brand name of the guitar
; - color is the color of the guitar
; - electric? is whether the guitar is electric or not
; Interpretation: description of a guitar

(define GUITAR-1 (make-guitar "Yamaha" "red" #true))
(define GUITAR-2 (make-guitar "Gibson" "blue" #false))

(define (guitar-temp g)
  (... (guitar-brand-name g) ...
       (guitar-color g) ...
       (guitar-electric? g) ...))





(define-struct drum-kit [brand-name electric?])

; A Drum-Kit is a (make-drum-kit String Boolean)
; - where brand-name is the brand name of the drum kit
; - electric? is whether the drum kit is electric or not
; Interpretation: description of a drum kit

(define DRUM-KIT-1 (make-drum-kit "Tama" #true))
(define DRUM-KIT-2 (make-drum-kit "DW" #false))

(define (drum-kit-temp dk)
  (... (drum-kit-brand-name dk) ...
       (drum-kit-electric? dk) ...))





(define-struct saxophone [brand-name])

; A Saxophone is a (make-saxophone String)
; - where brand-name is the brand name of the saxophone
; Interpretation: description of a saxophone

(define SAXOPHONE-1 (make-saxophone "Selmer"))
(define SAXOPHONE-2 (make-saxophone "Jean Paul"))

(define (saxophone-temp s)
  (... (saxophone-brand-name s) ...))




(define-struct piano [brand-name])

; A Piano is a (make-piano String)
; - where brand-name is the brand name of the piano
; Interpretation: description of a piano

(define PIANO-1 (make-piano "Kawai"))
(define PIANO-2 (make-piano "Baldwin"))

(define (piano-temp p)
  (... (piano-brand-name p) ...))




;; Part B

;; Design a data type called Instrument, which can represent any one of the
;; four instruments defined above.

;; [TODO] Data design recipe


; An Instrument is one of:
; - Guitar
; - Drum-Kit
; - Saxophone
; - Piano
; Represents either a guitar, drum kit, saxophone, piano

(define (instrument-temp i)
  (...
   (cond
     [(Guitar? i) (guitar-temp i)] ; 
     [(DrumKit? i) (drum-kit-temp i)]
     [(Saxophone? i) (saxophone-temp i)]
     [(Piano? i) (piano-temp i)])))

;; Part C

;; Design a data type called Band, which may have 1, 2, or 3 instruments.
;; The Band data type should hold information about all the instruments in
;; the band.

;; [TODO] Data design recipe

(define-struct 1band [instrument1])
; A 1Band is a (make-1band Instrument)
; - where instrument1 is the first instrument of the band 
; Interpretation: list of instruments in band composed of 1 instrument

(define-struct 2band [instrument1 instrument2])
; A 2Band is a (make-2band Instrument Instrument)
; - where instrument1 is the first instrument of the band 
; - where instrument2 is the second instrument of the band
; Interpretation: list of instruments in band composed of 2 instruments

(define-struct 3band [instrument1 instrument2 instrument3])
; A 3Band is a (make-3band Instrument Instrument Instrument)
; - where instrument1 is the first instrument of the band 
; - where instrument2 is the second instrument of the band
; - where instrument3 is the third instrument of the band
; Interpretation: list of instruments in band composed of 3 instruments

;; A Band is composed of:
;; - (make-1band Instrument)
;; - (make-2band Instrument Instrument)
;; - (make-3band Instrument Instrument Instrument)
;; Interpretation: list of instruments in band composed of 1, 2, or 3 instruments

(define BAND-1 (make-1band GUITAR-2))
(define BAND-2 (make-2band DRUM-KIT-1 SAXOPHONE-1))
(define BAND-3 (make-3band PIANO-2 DRUM-KIT-2 GUITAR-1))

(define (band-temp b)
  (...
   (cond
     [(1band? b) (instrument-temp (1band-instrument1 b)) ...]
     [(2band? b) (instrument-temp (2band-instrument1 b)) ...
                 (instrument-temp (2band-instrument2 b)) ...]
     [(3band? b) (instrument-temp (3band-instrument1 b)) ...
                 (instrument-temp (3band-instrument2 b)) ...
                 (instrument-temp (3band-instrument3 b)) ...])))
;; Part D

;; Design a function that takes a band and produces another band that is
;; identical, except that all guitars and drums become electric!

;; [TODO] Function design

(define GUITAR-2UPDATED (make-guitar "Gibson" "blue" #true))
(define BAND-1UPDATED (make-1band GUITAR-2UPDATED))

(define DRUM-KIT-2UPDATED (make-drum-kit "DW" #true))
(define BAND-3UPDATED (make-3band PIANO-2 DRUM-KIT-2UPDATED GUITAR-1))

; update-band : Band -> Band
; takes given band and produces another band that is identical
; except all guitars and drums become electric

(check-expect (update-band BAND-1) BAND-1UPDATED)
(check-expect (update-band BAND-2) BAND-2)
(check-expect (update-band BAND-3) BAND-3UPDATED)

(define (update-band b)
  (cond
    [(1band? b) (make-1band (update-instrument (1band-instrument1 b)))]
    [(2band? b) (make-2band (update-instrument (2band-instrument1 b))
                            (update-instrument (2band-instrument2 b)))]
    [(3band? b) (make-3band (update-instrument (3band-instrument1 b))
                            (update-instrument (3band-instrument2 b))
                            (update-instrument (3band-instrument3 b)))]))

; update-instrument : Instrument -> Instrument 
; takes given instrument and produces another instrumnet that is identical
; except guitar and drum become electric if not already 

(define (update-instrument i)
  (cond
    [(guitar? i) (update-guitar i)] 
    [(drum-kit? i) (update-drum-kit i)]
    [(saxophone? i) i]
    [(piano? i) i]))

; update-guitar : Guitar -> Guitar 
; takes given guitar and produces another guitar that is identical
; except becomes electric if not already

(define (update-guitar g)
  (make-guitar
   (guitar-brand-name g) 
   (guitar-color g)
   #true))

; update-drum-kit : Drum-Kit -> Drum-Kit 
; takes given drum kit and produces another drum kit that is identical
; except becomes electric if not already

(define (update-drum-kit dk)
  (make-drum-kit
   (drum-kit-brand-name dk)
   #true))






