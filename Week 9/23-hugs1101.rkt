;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 23-hugs1101) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
 
; hug-counter : String -> String
; Output how many clicks have been made until the user exits
; reads/writes to supplied filename 
 
(define (hug-counter filename)
  (local [(define LIST-O-HUGS (read-numbers-from-file filename))
          (define OLD-SUM-O-HUGS (foldr + 0 LIST-O-HUGS))]
    (write-file
     filename
     (lon->lines 
     (add-to-end
      (-
       (big-bang OLD-SUM-O-HUGS
         [to-draw draw-count]
         [on-mouse handle-click])
       OLD-SUM-O-HUGS)
      LIST-O-HUGS)))))

; add-to-end : (X) X [List-of X] -> [List-of X]
; puts X at the end of the list
 
(check-expect (add-to-end "a" '()) (list "a"))
(check-expect (add-to-end 5 (list 1 2 3 4)) (list 1 2 3 4 5))

(define (add-to-end x lox)
  (append lox (list x)))

; lon->lines : [List-of Number] -> String
; converts a list of number to a string with one number per line

(check-expect (lon->lines '()) "")
(check-expect (lon->lines (list 1 2 3)) "1\n2\n3\n")

(define (lon->lines lon)
  (foldr
   (λ (s-num s-lines) (string-append s-num "\n" s-lines))
   ""
   (map number->string lon))) 


; read-number-from-file : String -> Nat
; reads a number from a file, or 0 if the file doesn't exist

(check-expect (read-number-from-file "CRAZY_weird_NAME.BAD") 0)

(define (read-number-from-file filename)
  (if (file-exists? filename)
      (string->number (read-file filename))
      0))

; read-numbers-from-file : String -> [List-of Nat]
; reads a number form a file, or 0 if the file doesnt exist

(check-expect (read-numbers-from-file "CRAZY_weird_NAME.BAD") '())

(define (read-numbers-from-file filename)
  (if (file-exists? filename)
      (map string->number (read-lines filename))
      '()))

(define BG (text "🐼" 200 "black"))
; https://en.wikipedia.org/wiki/File:Panda_hug_by_comicmasterx.jpg
 
(define TEXT-SIZE 30)
(define TEXT-COLOR "black")
 
; draw-count : Nat -> Image
; Draw the count of clicks made
 
(check-expect (draw-count 5)
              (overlay/align
               "right" "top"
               (text "5" TEXT-SIZE TEXT-COLOR)
               BG))
  
(define (draw-count n)
  (overlay/align
   "right" "top"
   (text (number->string n) TEXT-SIZE TEXT-COLOR)
   BG))
 
; handle-click : Nat Number Number MouseEvent -> Nat
; add1 if click
 
(check-expect (handle-click 3 0 0 "button-down") 4)
(check-expect (handle-click 3 0 0 "button-up") 3)
 
(define (handle-click n x y me)
  (cond
    [(mouse=? me "button-down") (add1 n)]
    [else n]))
 
(hug-counter "hugs.txt")
