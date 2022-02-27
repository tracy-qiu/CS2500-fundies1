;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; Part A

;; The VIA Rail Canadian is a sleeper train that runs from Vancouver to Toronto.
;; It is also featured on the Canadian $10 bill. Here is a brochure:
;;
;; https://www.viarail.ca/sites/all/files/media/pdfs/111139179-1-Sales-tools_Anglais_WEB.pdf
;;
;; Look up the list of stops the Canadian makes, and write a data definition
;; called CanadianTrainStop that can represent any of these stops.
;; Ensure you follow _all_ steps of the data design recipe.

;; [TODO] Data design recipe

; A CanadianTrainStop is one of:
; - "Vancouver"
; - "Kamloops"
; - "Jasper"
; - "Edmonton"
; - "Saskatoon"
; - "Winnipeg"
; - "Sioux Lookout"
; - "Sudbury JCT"
; - "Toronto"
; Interpretation: stops the Canadian makes


(define CTS-VANCOUVER "Vancouver")
(define CTS-KAMLOOPS "Kamloops")
(define CTS-JASPER "Jasper")
(define CTS-EDMONTON "Edmonton")
(define CTS-SASKATOON "Saskatoon")
(define CTS-WINNIPEG "Winnipeg")
(define CTS-SIOUXLOOKOUT "Sioux Lookout")
(define CTS-SUDBURYJCT "Sudbury JCT")
(define CTS-TORONTO "Toronto")


(define (cts-temp cts)
  (...
   (cond
     [(string=? cts CTS-VANCOUVER) ...]
     [(string=? cts CTS-KAMLOOPS) ...]
     [(string=? cts CTS-JASPER) ...]
     [(string=? cts CTS-EDMONTON) ...]
     [(string=? cts CTS-SASKATOON) ...]
     [(string=? cts CTS-WINNIPEG) ...]
     [(string=? cts CTS-SIOUXLOOKOUT) ...]
     [(string=? cts CTS-SUDBURYJCT) ...]
     [(string=? cts CTS-TORONTO) ...]))) 

 
;; Part B

;; Design a function called province to determine the province in
;; which a stop is located. Ensure that you *strictly* follow the design
;; recipe.

;; [TODO] Function design recipe

; province : CanadianTrainStop -> String
; returns the province a stop is located 

(check-expect (province CTS-VANCOUVER) "British Columbia")
(check-expect (province CTS-KAMLOOPS) "British Columbia")
(check-expect (province CTS-JASPER) "Alberta")
(check-expect (province CTS-EDMONTON) "Alberta")
(check-expect (province CTS-SASKATOON) "Saskatchewan")
(check-expect (province CTS-WINNIPEG) "Manitoba")
(check-expect (province CTS-SIOUXLOOKOUT) "Ontario")
(check-expect (province CTS-SUDBURYJCT) "Ontario")
(check-expect (province CTS-TORONTO) "Ontario")

(define (province cts)
  (cond
    [(string=? cts CTS-VANCOUVER) "British Columbia"]
    [(string=? cts CTS-KAMLOOPS) "British Columbia"]
    [(string=? cts CTS-JASPER) "Alberta"]
    [(string=? cts CTS-EDMONTON) "Alberta"]
    [(string=? cts CTS-SASKATOON) "Saskatchewan"]
    [(string=? cts CTS-WINNIPEG) "Manitoba"]
    [(string=? cts CTS-SIOUXLOOKOUT) "Ontario"]
    [(string=? cts CTS-SUDBURYJCT) "Ontario"]
    [(string=? cts CTS-TORONTO) "Ontario"]))



;; Part C

;; Some stops on the Canadian connect to other train lines. Write a predicate
;; called can-transfer? that produces #true when a stop has an available
;; transfer to another line. You must follow all steps of the design recipe.
;; However, you may shorten your function definition if you find it convenient
;; to do so.

;; [TODO] Function design recipe

; can-transfer? : CanadianTrainStop -> Boolean
; returns true if the given stop has an available transfer to another line

(check-expect (can-transfer? CTS-VANCOUVER) #false)
(check-expect (can-transfer? CTS-KAMLOOPS) #false)
(check-expect (can-transfer? CTS-JASPER) #true)
(check-expect (can-transfer? CTS-EDMONTON) #false)
(check-expect (can-transfer? CTS-SASKATOON) #false)
(check-expect (can-transfer? CTS-WINNIPEG) #true)
(check-expect (can-transfer? CTS-SIOUXLOOKOUT) #false)
(check-expect (can-transfer? CTS-SUDBURYJCT) #false)
(check-expect (can-transfer? CTS-TORONTO) #true)

(define (can-transfer? cts)
  (cond
    [(string=? cts CTS-VANCOUVER) #false]
    [(string=? cts CTS-KAMLOOPS) #false]
    [(string=? cts CTS-JASPER) #true]
    [(string=? cts CTS-EDMONTON) #false]
    [(string=? cts CTS-SASKATOON) #false]
    [(string=? cts CTS-WINNIPEG) #true]
    [(string=? cts CTS-SIOUXLOOKOUT) #false]
    [(string=? cts CTS-SUDBURYJCT) #false]
    [(string=? cts CTS-TORONTO) #true]))