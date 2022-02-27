;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname templates0929) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Practice: Create templates for... ==

; A Silly is one of:
; - Number
; - (make-posn String Image)

(define (silly-temp s)
  (...
   (cond
     [(number? s) ... s ...]
     [(posn? s) ...
      (posn-x s) ...
      (posn-y s) ...])))



; A Bar is one of
; - "hello"
; - PositiveInteger

(define (bar-temp bar)
  (...
   (cond
     [(string? bar) ...]
     [(integer? bar) ... bar ...])))



(define-struct blah [ab cd])
; A Foo is a (make-blah Number Bar)

(define (foo-temp foo)
  (... (blah-ab foo) ...
       (bar-temp (blah-cd foo) ...)))