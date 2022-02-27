;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname html1110) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


(define-struct ol [content])
; A OL is a (make-ol [List-of Div])
 
(define-struct ul [content])
; A UL is a (make-ul [List-of Div])
 
; A Div is one of:
; - String
; - Image
; - OL
; - UL
; Interpretation: The structure of an HTML page,
; where OL is an ordered list and UL is an unordered list
 
(define DIV-1 "Hi!")
(define DIV-2 "Hello!")
(define DIV-3 (star 40 "solid" "yellow"))
(define DIV-4 (make-ul (list DIV-1 DIV-2)))
(define DIV-5 (make-ol (list DIV-1 DIV-2 DIV-3 DIV-4)))


; TODO #1
; Create templates for all types above

(define (div-temp div)
  (...
   (cond
     [(string? div) ... div ...]
     [(image? div) ... div ...]
     [(ol? div) ... (ol-temp div) ...]
     [(ul? div) ... (ul-temp div) ...])))


(define (ol-temp div)
  (... (lod-temp (ol-content div))))

(define (ul-temp div)
  (... (lod-temp (ul-content div))))

(define (lod-temp lod)
  (...
   (cond
     [(empty? lod) ...]
     [(cons? lod) ...
      (div-temp (first lod)) ...
      (lod-temp) (rest lod)] ...)))



; TODO #2
; Design a function num-divs that accepts a div and
; returns the total number of divs inside of it.

; num-divs : Div -> NatNum
; Counts the total number of Divs contained within this Div

(check-expect (num-divs DIV-1) 1)
(check-expect (num-divs DIV-2) 1)
(check-expect (num-divs DIV-3) 1)
(check-expect (num-divs DIV-4) 3)
(check-expect (num-divs DIV-5) 7)

(define (num-divs div)
  (local [; num-divs/lod : [List-of Div] -> NatNum
          ; how many divs are inside the list?
          (define (num-divs/lod lod)
            (cond
              [(empty? lod) 0]
              [(cons? lod)
               (+
                (num-divs (first lod))
                (num-divs/lod (rest lod)))]))]
               ;(foldr num-divs/lod 0 lod)]))] 
    (cond
      [(string? div) 1]
      [(image? div) 1]
      [(ol? div) (add1 (num-divs/lod (ol-content div)))]
      [(ul? div) (add1 (num-divs/lod (ul-content div)))])))


; TODO #3
; Design the function render-div that accepts a div and
; renders it like a web browser would.

; render-div : Div -> Image
; Renders this Div
  
(define TEXT-SIZE 12)
(define TEXT-COLOR "black")
(define BULLET (circle 2 "outline" "black"))
 
(define RENDER-1 (text DIV-1 TEXT-SIZE TEXT-COLOR))
(define RENDER-2 (text DIV-2 TEXT-SIZE TEXT-COLOR))
(define RENDER-3 DIV-3)
(define RENDER-4 (above/align "left"
                              (beside BULLET RENDER-1)
                              (beside BULLET RENDER-2)))

(define RENDER-5 (above/align
                  "left"
                  (beside/align "top"
                                (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-1)
                  (beside/align "top"
                                (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-2)
                  (beside/align "top"
                                (text "3. " TEXT-SIZE TEXT-COLOR) RENDER-3)
                  (beside/align "top"
                                (text "4. " TEXT-SIZE TEXT-COLOR) RENDER-4)))
 
(check-expect (render-div DIV-1) RENDER-1)
(check-expect (render-div DIV-2) RENDER-2)
(check-expect (render-div DIV-3) RENDER-3)
(check-expect (render-div DIV-4) RENDER-4)
(check-expect (render-div DIV-5) RENDER-5)

(define (render-div div)
 (cond
     [(string? div) (text div TEXT-SIZE TEXT-COLOR)]
     [(image? div) div]
     [(ol? div) (render-div/ol div)]
     [(ul? div) (render-div/ul div)]))

; render-div/ul : UL -> Image
; makes an image of bulleted renderings

(check-expect (render-div/ul (make-ul '())) empty-image)
(check-expect (render-div/ul DIV-4) RENDER-4)

(define (render-div/ul ul)
  (foldr
   (λ (new-render old-renders)
     (above/align "left" new-render old-renders))
    empty-image
    (map
     (λ (d) (beside BULLET (render-div d)))
     (ul-content ul))))

; render-div/ol : OL -> Image
; makes an image of numbered renderings

(check-expect (render-div/ol (make-ul '())) empty-image) 
(check-expect (render-div/ol DIV-5) RENDER-5)

(define (render-div/ol ol)
  (local [(define USEFUL-LIST (ol-content ol))

          ; render-numbered-item : Nat -> Image
          (define (render-numbered-item n)
            (beside/align
             "top"
             (text (string-append (number->string (add1 n)) ". ")
                   TEXT-SIZE TEXT-COLOR)
             (render-div (list-ref USEFUL-LIST n))))]
             
  (foldr
   (λ (new-render old-renders)
     (above/align "left" new-render old-renders))
    empty-image
    (build-list (length USEFUL-LIST) render-numbered-item))))
      






