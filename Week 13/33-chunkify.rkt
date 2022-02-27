;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 33-chunkify) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (gen-recurs-temp problem)
  (...
   (cond
     ([(trivial1? problem) ...]
      [(trivial2? problem) ...]
      ...
      [(non-trivial1? problem) ...
        (combine
         (gen-recurs-temp (subproblem1 problem))
         (gen-recurs-temp (subproblem2 problem)))]))))
         
#|   
; chunkify : (X) [List-of X] Nat[>0] -> [List-of [List-of X]]
; Groups the input list into sub-lists of supplied length
; Termination: we are always taking at least nat elements from the supplied list
 
(check-expect (chunkify '() 2) '())
(check-expect (chunkify (list 1 2 3) 1) (list (list 1) (list 2) (list 3)))
(check-expect (chunkify (list 1 2 3) 2) (list (list 1 2) (list 3)))
(check-expect (chunkify (list "a" "b" "c" "d" "e" "f") 3)
              (list (list "a" "b" "c") (list "d" "e" "f")))

; '() -> '()

; (cons (take Nat list) -> [List-of X]
;       (chunkify (drop Nat list) Nat))
;                        [List-of X]

(define (chunkify lox n)
   (cond
     [(empty? lox) '()]
      
      [(cons? lox) ...
        (cons
         (take n lox)
         (chunkify (drop n lox) n))]))

; take : (X) Nat[>0] [List-of X] -> [List-of X]
; produces a list with the first n values of the list

(check-expect (take 2 '()) '())
(check-expect (take 0 (list 1 2 3)) '())
(check-expect (take 1 (list 1 2 3)) (list 1))
(check-expect (take 2 (list 1 2 3)) (list 1 2))
(check-expect (take 3 (list 1 2 3)) (list 1 2 3))
(check-expect (take 4 (list 1 2 3)) (list 1 2 3))

(define (take n lox)
  (cond
    [(and (zero? n) (empty? lox)) '()]
    [(and (zero? n) (cons? lox)) '()]
    [(and (positive? n) (empty? lox)) '()]
    [(and (positive? n) (cons? lox))
     '()]))

(define (drop n lox)
  (cond
    [(or (zero? n) (empty? lox)) lox]
    [(and (positive? n



|#


; Design the function gcd that accepts two positive integers and computes their
; greatest common divisor (the largest integer that cleanly divides them both).
; Let's use an insight from math called Euclid's algorithm:
; if we take (gcd L S) with L > S, then (gcd L S) = (gcd S (remainder L S)) – until S=0.


; gcd : PositiveInteger PositiveInteger -> PositiveInteger
; Computes the GCD
; Termination: we're always recurring on something smaller

(check-expect (gcd2500 6 5) 1)
(check-expect (gcd2500 6 4) 2)
(check-expect (gcd2500 27 81) 27)
(check-expect (gcd2500 36 81) 9)

(define (gcd2500 n1 n2)
  (local [(define L (max n1 n2))
          (define S (min n1 n2))]
   (cond
     [(zero? S) L]
     [(positive? S)
      (gcd2500 S (remainder L S))])))
      












