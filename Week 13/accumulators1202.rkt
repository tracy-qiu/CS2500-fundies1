;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname accumulators1202) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (cumulative-distance (list 10 15 35))
              (list 10 25 60))
(check-expect (cumulative-distance (list 15 35))
              (list 15 50))
(check-expect (cumulative-distance '()) '())

; cumulative-distance : [List-of Number] -> [List-of Number]

#|
; a lot slower approach to cumulative - we use accumulators below much faster 
(define (cumulative-distance alist)
  (cond
    [(empty? alist) '()]
    [(cons? alist) (cons (first alist)
                         (map
                          (λ (n) (+ n (first alist)))
                          (cumulative-distance (rest alist))))]))
|#

(define (cumulative-distance alist)
  (local [ ; cumulative-distance/a : Number [List-of Number] -> Number
          (define (cumulative-distance/a sum-so-far alist)
            (cond
              [(empty? alist) '()]
              [(cons? alist) (cons (+ sum-so-far (first alist))
                                   (cumulative-distance/a  (+ sum-so-far (first alist))
                                                           (rest alist)))]))]
    (cumulative-distance/a 0 alist)))



(define (list-template alist)
  (cond
    [(empty? alist) ...]
    [(cons? alist) (... (first alist) ...
                        (list-template (rest alist)) ...)]))

; A Digit is a Nat in the range 0 .. 9 inclusive

(check-expect (digits->number (list 1 3 4 9)) 1349)
(check-expect (digits->number (list 1 3 4 9)) (+ 1000 300 40 9))
(check-expect (digits->number '()) 0)

; digits->number : [List-of Digit] -> Number

#|
(define (digits->number lod)
  (cond
    [(empty? lod) 0]
    [(cons? lod) (+ (* (first lod) (expt 10 (length (rest lod))))
                        (digits->number (rest lod)))]))
|#

(define (digits->number lod)
  (local [; digits->number/a len lod
          (define (digits->number/a len lod)
            (cond
              [(empty? lod) 0]
              [(cons? lod) (+ (* (first lod) (expt 10 (- len 1)))
                              (digits->number/a (- len 1) (rest lod)))]))]
    (digits->number/a (length lod) lod)))








   
