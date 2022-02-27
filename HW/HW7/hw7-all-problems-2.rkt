;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7-all-problems-2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All Problems for Homework 7

;; This assignment asks you to design several functions that employ the
;; following data designs. The functions that you design *must* use list
;; abstraction(s) when appropriate.
;;
;; NOTE #1: Part of the credit for each problem will be based on the choice of
;; list abstractions, so make sure that they are a good match for the problem.
;;
;; NOTE #2: For certain problems, you will have to design helper functions that
;; do not use list abstractions. You should follow the full design recipe
;; (including templates) for all problems.
;;
;; NOTE #3: For every function that you design, follow the "2+ tests and no
;; halloween colors" rule.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Designs (do not modify these)

;; A CallType is one of:
;; - "zoom"
;; - "teams"
;; - "phone"
;; Interpretation: a type of call

(define CT-ZOOM "zoom")
(define CT-TEAMS "teams")
(define CT-PHONE "phone")

(define (calltype-temp ct)
  (cond
    [(string=? ct CT-ZOOM) ...]
    [(string=? ct CT-TEAMS) ...]
    [(string=? ct CT-PHONE) ...]))

(define-struct call [type duration attendees description])
(define-struct mtg [duration attendees description])
(define-struct alone [duration description])

;; An Event is one of:
;; - (make-call CallType PosInt [NEList-of String] String)
;; - (make-mtg PosInt [NEList-of String] String)
;; - (make-alone PosInt String)
;; Interpretation: an event in some period of time, which is either:
;; - A call using some technology, lasting some number of minutes with attendees
;;  (by name), and a description;
;; - An in-person meeting lasting some number of minutes
;;   with attendees (by name) and a description; or
;; - Time spent alone for some number of minutes with a description.

(define E-ZOOM-DOC
  (make-call CT-ZOOM 22 (list "Dr. Zoidberg")
             "Doctor appointment about a stomach ache after some bad seafood :("))
(define E-TEAMS-OH
  (make-call CT-TEAMS 7 (list "Mike" "Tajel")
             "Office hours with my partner to ask clarifying questions about the Design Recipe!"))
(define E-PHONE-SPAM
  (make-call CT-PHONE 1 (list "Unknown")
             "Who calls!? I think it was a scam..."))
;; These are characters from a TV show called "Friends", which was popular in
;; the 90s, which is when many of your instructors grew up.
(define E-MTG-STUDY
  (make-mtg 62 (list "Rachel" "Ross" "Joey" "Phoebe" "Chandler" "Monica")
            "Getting ahead on studying for Exam 2!"))
(define E-MTG-ADVISOR
  (make-mtg 28 (list "Ali")
            "Meeting with advisor to talk about a combined major"))
(define E-ALONE-LUNCH
  (make-alone 34 "Lunch"))
(define E-ALONE-READING
  (make-alone 25 "The Three-Body Problem isn't going to read itself!"))
(define LOE-1
  (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM
        E-ALONE-LUNCH E-TEAMS-OH E-MTG-ADVISOR E-MTG-STUDY))

(define (e-temp e)
  (cond
    [(call? e)
     (... (calltype-temp (call-type e)) ...
          (call-duration e) ...
          (los-temp (call-attendees e)) ...
          (call-description e) ...)]
    [(mtg? e)
     (... (mtg-duration e) ...
          (los-temp (mtg-attendees e)) ...
          (mtg-description e) ...)]
    [(alone? e)
     (... (alone-duration e) ...
          (alone-description e) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design a function called weekend that reminds you to rest. The function
;; consumes an argument -- a positive integer -- and produces a list of that
;; many alone events. Each of these is 30mins each, with the description "rest".

;; [TODO] Function design. Use list abstractions when appropriate.
;; Signature : weekend : PosInt -> [List-of Event]
;; Purpose: takes a given positive integer and produces a list of that many alone events

(check-expect (weekend 3)
              (list (make-alone 30 "rest")
                    (make-alone 30 "rest")
                    (make-alone 30 "rest")))
(check-expect (weekend 5)
              (list (make-alone 30 "rest")
                    (make-alone 30 "rest")
                    (make-alone 30 "rest")
                    (make-alone 30 "rest")
                    (make-alone 30 "rest")))
(check-expect (weekend 2)
              (list (make-alone 30 "rest")
                    (make-alone 30 "rest")))

(define (weekend w)
  (make-list w (make-alone 30 "rest")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function small-group that consumes a list of events and only
;; produces those that have fewer than three participants. In all cases,
;; there is an implied attendee (you!) that counts as one person. So,
;; calls and meetings with two or more attendees are *not* small groups.

;; [TODO] Function design. Use list abstractions when appropriate.
(define LOE-2 (list E-PHONE-SPAM E-ZOOM-DOC E-MTG-STUDY))

;; signature - small-group : [List-of Event] -> [List-of Event]
(define (small-group loe)
  (filter small-enough? loe))


;; signature - small-enough? : Event -> Boolean
(define (small-enough? e)
  (cond
    [(alone? e) #true]
    [(call? e) (> 2 (length (call-attendees e)))]
    [(mtg? e) (> 2 (length (mtg-attendees e)))]))

(check-expect (small-group LOE-1)
              (list E-ZOOM-DOC E-ALONE-READING E-PHONE-SPAM E-ALONE-LUNCH E-MTG-ADVISOR))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function had-lunch? that accepts a list of events and determines
;; if it contains an event whose description contains the word "lunch" 
;; (without regard for upper/lower-case).

;; [TODO] Function design. Use list abstractions when appropriate.
;; signature - had-lunch? : [List-of Event] -> Boolean

(define (had-lunch? loe)
  (ormap contains-lunch? loe))

(define (contains-lunch? e)
  (cond
    [(call? e) (string-contains? (call-description e) "Lunch")]
    [(mtg? e)
     (string-contains? (mtg-description e) "Lunch")]
    [(alone? e)
     (string-contains? (alone-description e) "Lunch")]))

(check-expect (had-lunch? LOE-1) #true)
(check-expect (had-lunch? LOE-2) #false)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part D

;; Design the function social-time that accepts a list of events and produces
;; the total minutes spent on calls or meetings.

;; [TODO] Function design. Use list abstractions when appropriate.
;; Signature - [List-of Events] -> Natural Number
;; Purpose - Takes a list of events and adds the duration of only
;; calls and meetings. Alone events = 0
(define LOE-D2 (list E-PHONE-SPAM E-ZOOM-DOC E-MTG-ADVISOR))

(check-expect (social-time LOE-1) 120)
(check-expect (social-time LOE-D2) 51)

(define (social-time loe)
(foldr + 0 (all-durations loe)))

;; all-durations : Listof Events -> Listof Natural Numbers **
;; Purpose - outputs duration for an event
(check-expect (all-durations LOE-1) (list 22 0 1 0 7 28 62))
(check-expect (all-durations LOE-D2) (list 1 22 28))

(define (all-durations loe)
(map event-duration loe))

;; all-durations : Event -> Natural Number **
;; Purpose - outputs duration for an event
(check-expect (event-duration E-PHONE-SPAM) 1)
(check-expect (event-duration E-MTG-ADVISOR) 28)

(define (event-duration e)
(cond
[(call? e) (call-duration e)]
[(mtg? e) (mtg-duration e)]
[(alone? e) 0]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Part E

;; Design the function anything-but-phone? that accepts a list of events and
;; and produces #true if none of the calls were phone calls. (Zoom and Teams
;; calls are not phone calls.)

;; [TODO] Function design. Use list abstractions when appropriate.
(define LOE-E1 (list E-ZOOM-DOC E-TEAMS-OH E-MTG-STUDY E-ALONE-READING))
(define LOE-E2 (list E-PHONE-SPAM E-MTG-ADVISOR E-ALONE-LUNCH))

; anything-but-phone? : [ListOf Events] -> Boolean
; from a given list of events, determines if none of the calls were phone calls

(check-expect (anything-but-phone? LOE-1) #false)
(check-expect (anything-but-phone? LOE-E1) #true)
(check-expect (anything-but-phone? LOE-E2) #false)
              
(define (anything-but-phone? loe)
  (andmap event-nophone? loe))

; event-nophone? : Event -> Boolean
; given an event, returns true if the event is not a phone call

(check-expect (event-nophone? E-ZOOM-DOC) #true)
(check-expect (event-nophone? E-PHONE-SPAM) #false)
(check-expect (event-nophone? E-MTG-ADVISOR) #true)
(check-expect (event-nophone? E-ALONE-LUNCH) #true)
 
(define (event-nophone? e)
  (cond
    [(call? e)
     (no-phonetype? (call-type e))]
    [(mtg? e)
     #true]
    [(alone? e)
     #true]))
     
; no-calltype? : Call -> Boolean
; given a call, returns true if the call is not a phone call

(check-expect (no-phonetype? (call-type E-ZOOM-DOC)) #true)
(check-expect (no-phonetype? (call-type E-TEAMS-OH)) #true)
(check-expect (no-phonetype? (call-type E-PHONE-SPAM)) #false)

(define (no-phonetype? ct) 
  (cond
    [(string=? ct CT-ZOOM) #true]
    [(string=? ct CT-TEAMS) #true]
    [(string=? ct CT-PHONE) #false])) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part F

;; Design the function summary that accepts a list of
;; events and produces a list of textual summaries for each; for example...
;;
;; "Had a Teams call with Mike and Tajel for 7min:
;;  'Office hours with my partner to ask clarifying questions about the Design Recipe!'"
;;
;; "Met with Ali for 28min: 'Meeting with advisor to talk about a combined major'"
;;
;; "Spent 25min: 'The Three-Body Problem isn't going to read itself!'"
;;
;; Your summaries do not have exactly the same format as the examples above. 
;; But, they should contain the same information.

;; [TODO] Function design. Use list abstractions when appropriate.
(require racket/string)

(define LOE-F1 (list E-TEAMS-OH))
(define LOE-F2 (list E-TEAMS-OH E-MTG-ADVISOR))
(define LOE-F3 (list E-TEAMS-OH E-MTG-ADVISOR E-ALONE-READING))

; summary : [ListOf Events] -> String
; given a list of events, produces a list of textual summaries for each

(check-expect (summary LOE-F1)
              "Had a teams call with Mike and Tajel for 7min:
Office hours with my partner to ask clarifying questions about the Design Recipe!\n\n")

(check-expect (summary LOE-F2)
              "Had a teams call with Mike and Tajel for 7min:
Office hours with my partner to ask clarifying questions about the Design Recipe!

Met with Ali for 28min:
Meeting with advisor to talk about a combined major\n\n")

(check-expect (summary LOE-F3)
              "Had a teams call with Mike and Tajel for 7min:
Office hours with my partner to ask clarifying questions about the Design Recipe!

Met with Ali for 28min:
Meeting with advisor to talk about a combined major

Spent 25min:
The Three-Body Problem isn't going to read itself!\n\n")

(define (summary loe)
  (foldr string-append "" (map one-event-summary loe)))

; one-event-summary : Event -> String
; given an events, produces textual summary

(check-expect (one-event-summary E-TEAMS-OH)
              "Had a teams call with Mike and Tajel for 7min:
Office hours with my partner to ask clarifying questions about the Design Recipe!\n\n")
(check-expect (one-event-summary E-MTG-ADVISOR)
              "Met with Ali for 28min:
Meeting with advisor to talk about a combined major\n\n") 
(check-expect (one-event-summary E-ALONE-READING)  
              "Spent 25min:
The Three-Body Problem isn't going to read itself!\n\n")

(define (one-event-summary e)
  (cond
    [(call? e) (string-append
                "Had a " (call-type e)
                " call with " (attendees->string (call-attendees e))
                " for " (number->string (call-duration e)) "min:\n"
                (call-description e) "\n\n")]
    [(mtg? e) (string-append
               "Met with " (attendees->string (mtg-attendees e))
               " for " (number->string (mtg-duration e)) "min:\n"                            
               (mtg-description e) "\n\n")]
    [(alone? e) (string-append
                 "Spent " (number->string (alone-duration e)) "min:\n"
                 (alone-description e) "\n\n")]))
 
; attendees->string : [ListOf Attendees] -> String
; given a list of attendees, returns a string of all the attendees

(check-expect (attendees->string (call-attendees E-ZOOM-DOC)) "Dr. Zoidberg")
(check-expect (attendees->string (call-attendees E-TEAMS-OH)) "Mike and Tajel")
(check-expect (attendees->string (mtg-attendees E-MTG-STUDY))
              "Rachel and Ross and Joey and Phoebe and Chandler and Monica")

(define (attendees->string los)
  (string-join los " and "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part G

;; Design the function peeps that accepts a list of
;; events and produces an alphabetically sorted list of attendees
;; at all the calls/meetings (including any duplicates).

;; [TODO] Function design. Use list abstractions when appropriate.
;; Signature - peeps : [List-of Event] -> [List-of Attendees]
;; Purpose - Produces an alphabetically sorted list of attendees

(define LOE-G1 (list E-PHONE-SPAM E-TEAMS-OH E-ALONE-LUNCH))
(define LOE-G2 (list E-ALONE-READING E-TEAMS-OH E-MTG-ADVISOR))
(define LOE-G3 (list E-TEAMS-OH E-PHONE-SPAM E-ZOOM-DOC))

(define (peeps loe)
  (sort loe string<?))
(check-expect (peeps (list "Tracy" "Anusha" "Sara")) (list "Anusha" "Sara" "Tracy"))

; make a list all the attendees with only meetings and calls
; sort it lexographically

;; Signature - attendee-list : [List-of Event] -> [List-of Attendees]
(check-expect (attendee-list (list E-PHONE-SPAM E-TEAMS-OH)) (list (list "Unknown")
                                                                   (list "Mike" "Tajel")))
(define (attendee-list loe)
  (foldr attendee-event '() loe))

;; Signature - attendee-event : Event -> [List-of Attendees]
(check-expect (attendee-event '() E-TEAMS-OH (list "Mike" "Tajel")))
(define (attendee-event start e)
  (cond
    [(call? e) (call-attendees e)]
    [(mtg? e) (mtg-attendees e)]))

;; Signature - call-and-mtgs-list : [List-of Event] -> [List-of Event]
(check-expect (call-and-mtgs-list LOE-G1) (list E-PHONE-SPAM E-TEAMS-OH))

(define (call-and-mtgs-list loe)
  (filter right-event? loe))


;; Signature - right-event? : Event -> Boolean
(define (right-event? e)
  (cond
    [(alone? e) #false]
    [(call? e) #true]
    [(mtg? e) #true]))













