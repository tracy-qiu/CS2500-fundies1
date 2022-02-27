;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exam1-problem7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 7
;;
;; *This is a programming problem that you should do in DrRacket.*
;;
;; You have been tasked with building a web-based takeout menu for a pizzeria.
;; The pizzeria sells pizzas and soft drinks. Every pizza has a name
;; (e.g., "margherita" or "pepperoni"), and size (small, medium, or large).
;; Soft drinks only have a name (e.g., "Pepsi" or "Harmony Springs Cola").
;;
;; Design a data definition that represents a single menu item for the pizzeria.
;; Ensure you follow the entire data design recipe for all data that you design.

;; [TODO] Data designs

; Size is one of:
; - "small" 
; - "medium"
; - "large"
; Interpretation: represents the size of a pizza

(define SIZE-SMALL "small")
(define SIZE-MEDIUM "medium")
(define SIZE-LARGE "large")

(define (size-temp s)
  (...
   (cond
     [(string=? s SIZE-SMALL) ...]
     [(string=? s SIZE-MEDIUM) ...]
     [(string=? s SIZE-LARGE) ...])))



(define-struct pizza [name size])

; A Pizza is a (make-pizza String Size)
; - where name is the type of pizza
; - where Size is the size of the pizza: small, medium, or large
; Interpretation: description of a pizza

(define PIZZA-1 (make-pizza "pepperoni" "small"))
(define PIZZA-2 (make-pizza "pepperoni" "medium"))

(define (pizza-temp p)
  (... (pizza-name p) ...
       (size-temp (pizza-size p) ...)))


(define-struct soft-drink [name])

; A Soft-Drink is a (make-soft-drink String)
; - where name is the name of the of the soft drink
; Interpretation: name of soft drink

(define SOFT-DRINK-1 "Pepsi")
(define SOFT-DRINK-2 "Harmony Springs Cola")
(define SOFT-DRINK-3 "Sprite")

(define (soft-drink-temp sd)
  (... (soft-drink-name sd) ...))



(define ITEM-1 PIZZA-1)
(define ITEM-2 SOFT-DRINK-3)

; A MenuItem is one of the:
; - Pizza
; - Soft-Drink
; Interpretation: Represents a single item on the pizzeria's menu 

(define (menu-item-temp i)
  (...
   (cond
     [(pizza? i) ... (pizza-temp i) ...]
     [(soft-drink? i) ... (soft-drink-temp i) ...])))


