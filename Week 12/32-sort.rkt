;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 32-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 2500sort : [List-of Number] -> [SortedList-of Number]
; Sorts a list of numbers
 
(check-expect (2500sort '()) '())
(check-expect (2500sort (list 9 2 5)) (list 2 5 9))
(check-expect (2500sort (list 8 6 7 5 3 0 9))
              (list 0 3 5 6 7 8 9))

; 9 2 5  -> '() <- sorted
; 2 5 -> (list 9)
; 5 -> (list 2 9)
; '() -> (list 2 5 9)



; 3 1 9 2 5 -> (list 1 2) | 3 | (list 5 9) -> (list 1 2 3 5 9)
; pivot: 3

; left: 1 2 -> (list 1 2)
; left-pivot: 1
; left-left: '() -> '()
; left-right: (list 2) -> (list 2)

; right: 9 5 -> (list 5 9)
; right-pivot: 9
; right-left: 5
; right-right: '()

(define (2500sort/structural lon)
  (local [; insert : Number [SortedList-of Number] -> [SortedList-of Number]
          ; Inserts the element at the right spot
          ; if given 3 and (list 1 2 4), returns (list 1 2 3 4)
          (define (insert n slon)
            (cond
              [(empty? slon) (list n)]
              [(cons? slon)
               (if (<= n (first slon))
                   (cons n slon)
                   (cons (first slon) (insert n (rest slon))))]))]
    (foldr insert '() lon)))


; generative recursion
; Termination: always takes the pivot, 
(define (2500sort lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (local [(define PIVOT (first lon))
             (define LEFT (filter
                           (λ (n) (<= n PIVOT))
                           (rest lon)))
             (define RIGHT (filter
                           (λ (n) (> n PIVOT))
                           (rest lon)))]
       (append
        (2500sort LEFT)
        (list PIVOT)
        (2500sort RIGHT)))]))

(time (list? (2500sort (map random (build-list 1000 add1))))) 



  





