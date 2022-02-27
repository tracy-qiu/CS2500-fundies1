;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lox) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ListOfNumber is one of:
; - '()
; - (cons Number ListOfNumber)

; A ListOfPosition is one of:
; - '()
; - (cons Position ListOfPosition)

; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
; Interpretation: a list of X!

; A BadList is one of:
; - '()
; - (cons Any BadList)

; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])

(define NELON-1 (cons 1 '()))
(define NELON-2 (cons 2 NELON-1))
(define NELON-3 (cons 3 NELON-2))

(define (nelox-temp nelox)
  (...
   (cond
     [(empty? (rest nelox))
      ... (first nelox) ...]
     [(cons? (rest nelox))
      ... (first nelox) ...
      ... (nelox-temp (rest nelox)) ...])))
             
           