;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname web1108) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct webpage [title links])

; mutually referrential data - multiple types that refer to each other 
; A [ List-of WebPage] is one of:
; - '()
; - (cons WebPage [List-of WebPage])


; A WebPage is a (make-webpage String [List-of Webpage])
; Interpretation: a webpage's title and links 

(define WP-KHOURY (make-webpage "Khoury" '()))
(define WP-NU (make-webpage "NU" (list WP-KHOURY)))
(define WP-BOSTON (make-webpage "Boston" (list WP-NU WP-KHOURY)))

(define (wp-temp wp)
  (... (webpage-title wp) ...
       (lowp-temp (webpage-links wp)) ...))

(define (lowp-temp lowp)
  (...
   (cond
     [(empty? lowp) ...]
     [(cons? lowp) ...
      (wp-temp (first lowp)) ...
      (lowp-temp (rest lowp)) ...])))
   

; TODO #1
; Design a function exists? that accepts a webpage
; and a string, and determines whether a page with
; that title exists (as either the supplied webpage
; or by following all of its links).

; E.g., NU -> Khoury, Boston -> NU, Khoury

; exists? : Webpage String -> Boolean
; does a page with the supplied title exist?

(check-expect (exists? WP-KHOURY "Khoury") #true)
(check-expect (exists? WP-KHOURY "Boston") #false)
(check-expect (exists? WP-BOSTON "Khoury") #true)
(check-expect (exists? WP-BOSTON "Boston") #true)
(check-expect (exists? WP-BOSTON "foo") #false)

(define (exists? wp title)
  (local [; exists?/page : WebPage -> Boolean
          ; does a page with the supplied title exist?
          (define (exists?/page p)
            (or (string=? (webpage-title p) title)
       (exists?/list (webpage-links p))))

          ; exists?/list : [List-of WebPage] -> Boolean
          ; does the supplied title exist in the list of webpages?
          (define (exists?/list l)
            (ormap (exists?/page l)))]
    (exists?/page wp)))
          
(define (exists? wp title)
  (or (string=? (webpage-title wp) title)
       (exists?/lowp (webpage-links wp) title)))

; exists?/lowp : [List-of Webpage] String -> Boolean
; does the supplied title exist in the list of webpages?

(check-expect (exists?/lowp '() "Boston") #false)
(check-expect (exists?/lowp (list WP-KHOURY) "Boston") #false)
(check-expect (exists?/lowp (list WP-KHOURY WP-NU WP-BOSTON) "Boston") #true)
(check-expect (exists?/lowp (list WP-BOSTON) "Khoury") #true)
(check-expect (exists?/lowp (list WP-BOSTON) "foo") #false)

(define (exists?/lowp lowp title)
  (ormap
   (λ (wp) (exists? wp title))
   lowp))

(define (exists?/lowp-v1 lowp title)
   (cond
     [(empty? lowp) #false]
     [(cons? lowp)
      (or
      (exists? (first lowp) title)
      (exists?/lowp (rest lowp) title))]))
   




