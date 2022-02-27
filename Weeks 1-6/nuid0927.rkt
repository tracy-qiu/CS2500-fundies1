;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nuid0927) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NUIDorFalse is one of:
; - #false
; - NaturalNumber
; Interpretation: A student's NUID, or #false if it is not known

(define NUIDORFALSE-f #false)
(define NUIDORFALSE-1 17)
(define NUIDORFALSE-2 192388)

(define (norf-temp norf)
  (...
   (cond
     [(boolean? norf) ... ]            ; dont need to read what norf is
                                       ; because if it is bool then will automatically be #false
     [(number? norf) ... norf ...])))


; Design the data to represented a Lyft you ordered,
; which can be either its (x,y) location or a status
; (such as "arrived").


; A Lyft is one of:
; - (make-posn Real Real)       ; (x,y) location 
; - String                      ; status 
; - 

(define LYFT-00 (make-posn 0 0))
(define LYFT-ARRIVED "arrived")

(define (lyft-temp lyft)
  (...
   (cond
     [(posn? lyft) ...
      (posn-x lyft) ...
      (posn-y lyft) ...]
     [(string? lyft) ... lyft ...])))
