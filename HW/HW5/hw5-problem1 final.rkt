;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw5-problem1 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; Consider the following data definition:

;; A ListofString (LoS) is one of:
;; - '()
;; - (cons String LoS)
;; Interpretation: A list of strings.

(define EX-LOS-1 '())
(define EX-LOS-2 (cons "susie"
                       (cons "robert baratheon"
                             (cons "alice" EX-LOS-1))))
(define EX-LOS-3 (cons "bob" EX-LOS-2))

;; los-template: LoS -> ?
(define (los-template los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) (... (first los) ...
                       (los-template (rest los)) ...)])))

;; Part A

;; Design a predicate called contains-three-letter-string? that consumes an LoS 
;; and determines if it contains a string that is exactly three letters long.

;; [TODO] Function design
;; signature - contains-three-letter-string? : LoS -> Boolean
;; purpose - consumes an LoS and determines if it contains a string that is exactly
;; three letters long

(check-expect (contains-three-letter-string? EX-LOS-1) '())
(check-expect (contains-three-letter-string? EX-LOS-2) #false)
(check-expect (contains-three-letter-string? EX-LOS-3) #true)

(define (contains-three-letter-string? los)
  (cond
    [(empty? los) '()]
    [(cons? los) (= 3 (string-length (first los)))]
    [(cons? los) (contains-three-letter-string? (rest los))]))

;; Part B

;; Design a function called hello-list that consumes an LoS and produces a new
;; LoS. The produced LoS prefixes every string in the consumed LoS with 
;; "Hello, ".

;; [TODO] Function design

;; signature - hello-list : LoS -> String
;; purpose - consumes an LoS and produces a new LoS that prefixes every string in the consumed
;; LoS with "Hello, "

(define HELLO-EX-LOS-2 (cons "Hello, susie"
                             (cons "Hello, robert baratheon"
                                   (cons "Hello, alice" EX-LOS-1))))
(define HELLO-EX-LOS-3 (cons "Hello, bob" HELLO-EX-LOS-2))

(check-expect (hello-list EX-LOS-1) '())
(check-expect (hello-list EX-LOS-2) HELLO-EX-LOS-2)
(check-expect (hello-list EX-LOS-3) HELLO-EX-LOS-3)

(define (hello-list los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (cons
      (string-append "Hello, " (first los))
      (hello-list (rest los)))]))

