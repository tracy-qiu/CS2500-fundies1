;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; This problem has a partially-completed data design for representing DNA
;; sequences. A DNA molecule is a long sequence of four nucleotides: adenine,
;; cytosine, guanine, and thymine. (More information here:
;; https://en.wikipedia.org/wiki/DNA.)

(define-struct adenine [rest])
(define-struct guanine [rest])
(define-struct cytosine [rest])
(define-struct thymine [rest])

;; A DNASeq is one of:
;; - (make-adenine DNASeq)
;; - (make-guanine DNASeq)
;; - (make-cytosine DNASeq)
;; - (make-thymine DNASeq)
;; - "empty sequence"
;; Interpretation: A DNASeq represents a sequence of nucleotides.

;; Part A

;; Write at least three examples of DNASeq. Across all examples, ensure you
;; have an example of all nucleotides.

;; [TODO] Three 
(define DNASEQ1-1 (make-adenine "empty sequence"))
(define DNASEQ1-2 (make-cytosine DNASEQ1-1))
(define DNASEQ1-3 (make-thymine DNASEQ1-2))
(define DNASEQ1 (make-guanine DNASEQ1-3))

(define DNASEQ2-1 (make-guanine "empty sequence"))
(define DNASEQ2-2 (make-thymine DNASEQ2-1))
(define DNASEQ2-3 (make-cytosine DNASEQ2-2))
(define DNASEQ2 (make-adenine DNASEQ2-3))

(define DNASEQ3-1 (make-thymine "empty sequence"))
(define DNASEQ3-2 (make-adenine DNASEQ3-1))
(define DNASEQ3-3 (make-guanine DNASEQ3-2))
(define DNASEQ3 (make-cytosine DNASEQ3-3))



;; Part B

;; Write a template for DNASeq.
; An DNASeq is one of:
;; - Adenine
;; - Guanine
;; - Cytosine
;; - Thymine
;; - "empty sequence"
;; Interpretation: A DNASeq represents a sequence of nucleotides.

;; [TODO] Template
(define (dnaseq-temp d)
  (...
   (cond
     [(adenine? d) ... (dnaseq-temp (adenine-rest d)) ...]
     [(guanine? d) ... (dnaseq-temp (guanine-rest d)) ...]
     [(cytosine? d) ... (dnaseq-temp (cytosine-rest d)) ...]
     [(thymine? d) ... (dnaseq-temp (thymine-rest d)) ...])))

;; Part C

;; Every  DNA sequence has a complementary sequence, which substitutes
;; As with Ts, Ts with As, Cs with Gs, and Gs with Cs. Design a function
;; to calculate the complement of a DNA sequence.

;; [TODO] Function design recipe

(define DNASEQ1-1u (make-thymine "empty sequence"))
(define DNASEQ1-2u (make-guanine DNASEQ1-1u))
(define DNASEQ1-3u (make-adenine DNASEQ1-2u)) 
(define DNASEQ1u (make-cytosine DNASEQ1-3u))

(define DNASEQ2-1u (make-cytosine "empty sequence"))
(define DNASEQ2-2u (make-adenine DNASEQ2-1u))
(define DNASEQ2-3u (make-guanine DNASEQ2-2u))
(define DNASEQ2u (make-thymine DNASEQ2-3u))

(define DNASEQ3-1u (make-adenine "empty sequence"))
(define DNASEQ3-2u (make-thymine DNASEQ3-1u))
(define DNASEQ3-3u (make-cytosine DNASEQ3-2u))
(define DNASEQ3u (make-guanine DNASEQ3-3u))

(check-expect (pair-dna DNASEQ1) DNASEQ1u)
(check-expect (pair-dna DNASEQ2) DNASEQ2u)
(check-expect (pair-dna DNASEQ3) DNASEQ3u)

; pair-dna : DNASeq -> DNASeq
; calculates the complement of a given DNA sequence

(define (pair-dna d)
  (cond
    [(adenine? d) (make-thymine (pair-dna (adenine-rest d)))]
    [(guanine? d) (make-cytosine (pair-dna (guanine-rest d)))]
    [(cytosine? d) (make-guanine (pair-dna (cytosine-rest d)))]
    [(thymine? d) (make-adenine (pair-dna (thymine-rest d)))]
    [(string? d) "empty sequence"]))   

