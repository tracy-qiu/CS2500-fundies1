;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname scope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; f(x) = x + 7
; g(y) = x + y + 10

(define (f x)
(+ x 7))

(f 3)

; V: local
; S:

(local [definition1
         definition2
         ...]
  expr-possibly-using-local-definitions)