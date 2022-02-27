;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; NOTE #1: Feel free to use list abstractions if you like.
;; NOTE #2: The "2+ check-expects and no-halloween-colors" rule applies.

;; Consider the following data definitions for a network (graph) of Subway
;; stations.

(define-struct station [name connections])
;; A Station is a (make-station String [List-of String])
;; Interpretation: The name of a station and the names of stations that directly
;; connect to it.

(define EX-STATION-1 (make-station "Newton Centre" (list "Fenway" "Kenmore")))
(define EX-STATION-2 (make-station "Fenway" (list "Newton Highlands" "Newton Centre")))
(define EX-STATION-3 (make-station "Kenmore" (list "Newton Centre")))
(define EX-STATION-4 (make-station "Newton Highlands" (list "Fenway")))

;; station-template : Station -> ?
(define (station-template s)
  (... (station-name s) ...
       (los-template (station-connections s)) ...))

;; A Subway is a [List-of Station]
;; Interpretation: A list of stations that make a subway network.

(define EX-SUBWAY-1 (list EX-STATION-1 EX-STATION-2 EX-STATION-3 EX-STATION-4))

;; A silly example, but technically fits the data definition.
(define EX-SUBWAY-2 '())

;; Wow, a circuit in a subway! The Moscow Metro actual has a circuit.
(define EX-SUBWAY-3 (list (make-station "A" (list "B" "D"))
                          (make-station "B" (list "C" "A"))
                          (make-station "C" (list "D" "B"))
                          (make-station "D" (list "A" "C"))))

;; subway-template : Subway -> ?
(define (subway-template s)
  (...
   (cond
     [(empty? s) ...]
     [(cons? s) (... (station-template (first s)) ... 
                     (subway-template (rest s)) ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design a function called add-edge that adds an edge to a Subway from one
;; station to another *in just one direction*. We have given you the
;; signature, purpose statement, and check-expects, so you only need to
;; write the function body, and potentially a helper function.

;; add-edge : String String Subway -> Subway
;; (add-edge from to station) adds a connection to the subway, if it does not
;; already exist. Assumes that both from and to stations in the Subway.

(check-expect
 (add-edge "Fenway" "Kenmore" EX-SUBWAY-2) EX-SUBWAY-2)

(check-expect
 (add-edge "Fenway" "Kenmore" EX-SUBWAY-1)
 (list EX-STATION-1
       (make-station "Fenway" (list "Kenmore" "Newton Highlands" "Newton Centre"))
       EX-STATION-3
       EX-STATION-4))

(check-expect (add-edge "Kenmore" "Newton Centre" EX-SUBWAY-1) EX-SUBWAY-1)

(check-expect (add-edge "A" "C" EX-SUBWAY-3)
              (list (make-station "A" (list "C" "B" "D"))
                    (make-station "B" (list "C" "A"))
                    (make-station "C" (list "D" "B"))
                    (make-station "D" (list "A" "C"))))


;; Do not alter the signature, purpose, and check-expects written above!

(define (add-edge s1 s2 subway)
  (cond
    [(empty? subway) '()]
    [(cons? subway)
     (cons (add-station s1 s2 (first subway))  
           (add-edge s1 s2 (rest subway)))]))  


(check-expect (add-station "Fenway" "Kenmore" EX-STATION-2)
              (make-station "Fenway" (list "Kenmore" "Newton Highlands" "Newton Centre")))
(check-expect (add-station "Newton Centre" "Waban" EX-STATION-1)
              (make-station "Newton Centre" (list "Waban" "Fenway" "Kenmore")))
(check-expect (add-station "Newton Highlands" "Fenway" EX-STATION-4) EX-STATION-4)

; add-station : String String -> Station
; given a station and stop to be added as a connection, creates a station that
; adds that destination to the list of station connections

(define (add-station s1 s2 station)
  (if (and (string=? (station-name station) s1) 
           (not (ormap [λ (s) (string=? s s2)] (station-connections station))))
      (make-station s1 (cons s2 (station-connections station))) station))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

(require racket/list)

;; We will use the following data definition in this problem:

;; A [List-of-2-or-more X] is one of:
;; - (cons X (cons X '()))
;; - (cons X [List-of-2-or-more X])
;; A list with two or more items.

(define EX-LO2OM-1 (list "Newton Centre" "Fenway"))
(define EX-LO2OM-2 (list "Kenmore" "Landsdowne" "Newton Centre"))

(define (list-of-2-or-more-template lo2om)
  (...
   (cond
     [(empty? (rest (rest lo2om))) 
      (... (first lo2om) ...
           (first (rest lo2om)) ...)]
     [(cons? (rest (rest lo2om)))
      (... (first lo2om) ...
           ;; The line below is the second item in the list. We know it exists
           ;; because (rest (rest lo2om)) has 2+ items. 
           (first (rest lo2om)) ... 
           (list-of-2-or-more-template (rest lo2om)) ...)])))

;; Design a function that consumes a [List-of-2-or-more String], where each 
;; String represents a station on a subway line. The function should produce a 
;; Subway with those stations connected appropriately. i.e.,  each station in
;; the line is directly connected to both the next and previous stations
;; on the line. Here is one example test (you should add more):

(check-expect (line->subway (list "A" "B" "C"))
              (list (make-station "A" (list "B")) 
                    (make-station "B" (list "A" "C")) 
                    (make-station "C" (list "B"))))
(check-expect (line->subway EX-LO2OM-1)
              (list (make-station "Newton Centre" (list "Fenway"))
                    (make-station "Fenway" (list "Newton Centre"))))
(check-expect (line->subway (list "Northeastern" "Symphony" "Prudential" "Copley"))
              (list (make-station "Northeastern" (list "Symphony"))
                    (make-station "Symphony" (list "Northeastern" "Prudential"))
                    (make-station "Prudential" (list "Symphony" "Copley"))
                    (make-station "Copley" (list "Prudential"))))

;; Hint #1: It helps to begin with a subway where all stations are disconnected.
;; from each other.
;;
;; Hint #2: Design a helper function that creates connections in just one
;; direction (which will likely use add-edge function Part A!). 
;; - Perhaps the disconnected stations could be fed to this helper?
;; - What if you also had the stations listed in the opposition direction too?

;; [TODO] Function design

;; line->subway : [List-of-2-or-more X] -> Subway
;; Given a list of station names, [List-of-2-or-more String], produces a 
;; Subway with those stations connected appropriately.

(define (line->subway line) 
  (cond
    [(empty? (rest line)) '()]
    [(empty? (rest (rest line)))
     (make-connections
      (first (rest line)) (reverse line) 
      (make-connections
       (first (rest line)) line 
       (make-connections
        (first line) (reverse line) 
        (make-connections
         (first line) line
         (map (λ (name) (make-station name '())) line)))))]
    [(cons? (rest (rest line)))
     (append
      ;(merge
      (make-connections
       (first (rest line)) (reverse line) 
       (make-connections
        (first (rest line)) line 
        (make-connections
         (first line) (reverse line) 
         (make-connections
          (first line) line
          (map (λ (name) (make-station name '())) line)))))
      (line->subway (rest line)))]))

(check-expect (merge 
               (list
                (make-station "A" (list "B"))
                (make-station "B" (list "A" "C"))
                (make-station "C" '()))
               (list
                (make-station "B" (list "C"))
                (make-station "C" (list "B"))))
              (list
               (make-station "A" (list "B"))
               (make-station "B" (list "A" "C"))
               (make-station "C" (list "B"))))
(check-expect (merge (list (make-station "Copley" (list "Arlington"))
                           (make-station "Boylston" (list "Park St")))
                     (list (make-station "Copley" (list "Arlington" "Boylston"))))
              (list (make-station "Copley" (list "Arlington" "Boylston"))
                    (make-station "Boylston" (list "Park St"))))
                           
                           
                                         

; merge : [List-of Stations] [List-of Stations] -> [List-of Stations]
; merge lists of stations, keeping only the stations with the longer list of connections

(define (merge l1 l2)
  (cond
    [(empty? l1) l2] 
    [(empty? l2) l1]
    [(cons? l1)
     (local [(define DISTINCT-REST (merge (map station-name (rest l1)) (map station-name l2)))]
       (if (ormap [λ (s) (string=? s (station-name (first l1)))] DISTINCT-REST)
           (if (> (length (station-connections (first l1)))
                  (length (station-connections (first l2)))) 
               l1 l2) 
           (cons (first l1) DISTINCT-REST)))])) 
     
;; make-connections : X [List-of-2-or-more X] Subway -> Subway
;; creates connections of adjacent stations in just one direction

(define start (list (make-station "A" '())
                    (make-station "B" '())
                    (make-station "C" '())))
                                   
(check-expect (make-connections "A" (list "A" "B" "C") start)
              (list (make-station "A" (list "B"))
                    (make-station "B" '()) 
                    (make-station "C" '())))
(check-expect (make-connections "B" (list "A" "B" "C") start)
              (list (make-station "A" '())
                    (make-station "B" (list "C"))
                    (make-station "C" '()))) 
(check-expect (make-connections "C" (list "A" "B" "C") start) 
              (list (make-station "A" '())
                    (make-station "B" '())
                    (make-station "C" '())))
 
(define (make-connections name line subway) 
  (if (< (+ (index-of line name) 1) (length line))  
      (add-edge name (list-ref line (+ (index-of line name) 1)) subway)
      subway)) 
  






