;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |hw5-problem2 final|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

(require 2htdp/image)

;; There are many common examples of concentric circles. For example,
;; the following set of concentric circles looks like the Target corporate logo:

(define TARGET-LOGO-IMAGE
  (overlay
   (circle 10 "solid" "red")
   (circle 20 "solid" "white")
   (circle 30 "solid" "red")))

;; And these concentric circles look like an archery target:

(define ARCHERY-TARGET-IMAGE
  (overlay
   (circle 5 "solid" "yellow")
   (circle 10 "solid" "red")
   (circle 15 "solid" "blue")
   (circle 20 "solid" "black")))

;; Now consider the following data definitions:

(define-struct ring [radius color])
;; A Ring is a (make-ring Nat Color)
;; Interpretation: a ring of color where
;; - radius is the radius of the ring
;; - color is the color of the ring

(define RING-EXAMPLE-1 (make-ring 10 "red"))
(define RING-EXAMPLE-2 (make-ring 5 "yellow"))
(define RING-EXAMPLE-3 (make-ring 20 "black"))

;; ring-template: Ring -> ?
(define (ring-template ring)
  (... (ring-radius ring) ...
       (ring-color ring) ...))

;; A ListofRings (LoR) is one of:
;; - '()
;; - (cons Ring LoR)
;;
;; Interpretation: A list of rings.

;; Part A
;;
;; Write three examples and the template for LoR.

;; [TODO] Template and three examples

(define LOR-0 '())
(define LOR-1 (cons RING-EXAMPLE-3 LOR-0))
(define LOR-2 (cons RING-EXAMPLE-1 LOR-1))
(define LOR-3 (cons RING-EXAMPLE-2 LOR-2))

(define (lor-temp lor)
  (...
   (cond
     [(empty? lor) ...]
     [(cons? lor) ...
      (ring-template (first lor)) ...
      (lor-temp (rest lor)) ...])))

;; Part B
;;
;; Design a function called rings->image that consumes an LoR and
;; produces an image that displays the rings as concentric circles.
;;
;; You may assume that the radius of the rings decreases. Thus the first ring
;; has a larger radius than the second ring, and so on.

;; rings->image : LOR -> Image
;; produces an image that displays the rings as concentric circles
;; from the given list of rings 

(define RING-IMAGE-1
  (overlay
   (circle (ring-radius RING-EXAMPLE-2) "solid" (ring-color RING-EXAMPLE-2))
   (circle (ring-radius RING-EXAMPLE-1) "solid" (ring-color RING-EXAMPLE-1))
   (circle (ring-radius RING-EXAMPLE-3) "solid" (ring-color RING-EXAMPLE-3))))
(define RING-IMAGE-2
  (overlay
   (circle (ring-radius RING-EXAMPLE-1) "solid" (ring-color RING-EXAMPLE-1))
   (circle (ring-radius RING-EXAMPLE-3) "solid" (ring-color RING-EXAMPLE-3))))
(define RING-IMAGE-3
  (overlay
   (circle (ring-radius RING-EXAMPLE-3) "solid" (ring-color RING-EXAMPLE-3))
   empty-image))
    
(check-expect (rings->image LOR-3) RING-IMAGE-1)
(check-expect (rings->image LOR-2) RING-IMAGE-2)
(check-expect (rings->image LOR-1) RING-IMAGE-3)


(define (rings->image lor)
  (cond
    [(empty? lor) empty-image]
    [(cons? lor)
     (overlay
      (circle-maker (first lor))
      (rings->image (rest lor)))]))

;; circle-maker : Ring -> Image
;; makes a circle

(check-expect (circle-maker RING-EXAMPLE-1) (circle 10 "solid" "red"))
(check-expect (circle-maker RING-EXAMPLE-2) (circle 5 "solid" "yellow"))
(check-expect (circle-maker RING-EXAMPLE-3) (circle 20 "solid" "black"))

(define (circle-maker c)
  (circle (ring-radius c) "solid" (ring-color c)))








