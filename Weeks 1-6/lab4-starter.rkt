;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1: Attempt at least the first TODO item (the data design) for this
;; problem.

;; TODO #1: design the data necessary to represent a book, which can
;; either be physical or electronic. All books have a title and author.
;; Physical books are either paperback or harcover, and have some number of 
;; pages. Electronic (e-books) have a format (pdf, epub, txt) and a source URL.


; ElectronicType is one of:
; - "pdf"
; - "epub"
; - "txt"

(define (electronictype-temp b)
  (...
   (cond
     [string=? b "pdf" ...]
     [string=? b "epub" ...]
     [string=? b "txt" ...])))

(define-struct physical [title author paperback?])

; Physical is a (make-physical String String Boolean)
; - where title is the title of the book
; - where author is the author of the book
; - where paperback? is paperback if true and hardcover if false
; Interpretation: description of a physical book 

(define PHYSICAL-1 (make-physical "Harry Potter" "J.K. Rowling" #true))
(define PHYSICAL-2 (make-physical "Magic Tree House" "Mary Jane" #false))

(define (physical-temp b)
  (... (physical-title b) ...
       (physical-author b) ...
       (physical-paperback? b) ...))


(define-struct electronic [title author electronictype URL])

; Electronic is a (make-electronic String String ElectronicType String)
; - where title is the title of the book
; - where author is the author of the book
; - where electronictype is either pdf, epub, or txt
; - where URL is the source URL of the book
; Interpretation: description of an electronic book

(define ELECTRONIC-1 (make-electronic "Harry Potter" "J.K. Rowling" "pdf" "www.harrypotter.com"))
(define ELECTRONIC-2 (make-electronic "Sorcerer's Stone" "J.K. Rowling" "epub" "www.jkrowling.com"))

(define (electronic-temp b)
  (... (electronic-title b) ...
       (electronic-author b) ...
       (electronic-electronictype b) ...
       (electronic-URL b) ...))

; A book is one of:
; - Physical 
; - Electronic 
; Interpretation: description of physical or electronic book with its details

(define (book-temp b)
  (...
   (cond
     [(physical? b) (physical-temp b)]
     [(electronic? b) (electronic-temp b)])))



; TODO #2: now design the function where-to-find that
; accepts a book and returns where you can find it:
; physical books are either in the "hardcover section"
; or "paperback section", whereas electronic books are
; found at their URL.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2: Attempt at least one function design for this problem. We
;; recommend doing more!

;; Consider the following data definitions:

;; A Genre is one of:
;; - "comedy"
;; - "drama"
;; - "action"
;; - "education"
;; Interpretation: genre for a video

(define GENRE-COMEDY "comedy")
(define GENRE-DRAMA "drama")
(define GENRE-ACTION "action")
(define GENRE-EDUCATION "education")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-COMEDY) ...]
     [(string=? g GENRE-DRAMA) ...]
     [(string=? g GENRE-ACTION) ...]
     [(string=? g GENRE-EDUCATION) ...])))


(define-struct video [name duration hd? genre next])
;; A StreamingQueue is one of:
;; - #false
;; - (make-video String PosInteger Boolean Genre StreamingQueue)
;; Interpretation: either an empty queue
;; or a video with a name, duration in minutes,
;; whether it's available in HD, and its genre.

(define QUEUE-EMPTY #false)

(define QUEUE-CRASH
  (make-video "Crash Course Organic Chemistry #5"
              14 #true GENRE-EDUCATION
              QUEUE-EMPTY))

(define QUEUE-OLIVER
  (make-video
   "Prisons & Jails: Last Week Tonight with John Oliver"
   18 #true GENRE-COMEDY
   QUEUE-CRASH))

(define QUEUE-DUEL
  (make-video
   "Duel" 2 #false GENRE-ACTION QUEUE-OLIVER))

(define QUEUE-STORM
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

(define (sq-temp sq)
  (...
   (cond
     [(boolean? sq) ...]
     [(video? sq) ...
      (video-name sq) ...
      (video-duration sq) ...
      (video-hd? sq) ...
      (genre-temp (video-genre sq)) ...
      (sq-temp (video-next sq)) ...])))


;; TODO: Design one of the following functions.

;; Design the function good-for-friday? that determines if
;; a streaming queue has content that is comedy or action

(check-expect (good-for-friday? QUEUE-STORM) #true)
(check-expect (good-for-friday? QUEUE-CRASH) #false)
 
(define (good-for-friday? sq)
  (cond
    [(boolean? sq) #false]
    [(video? sq)
     (or (goodgenre? (video-genre sq))  
         (good-for-friday? (video-next sq)))]))

(define (goodgenre? g)
  (cond
    [(string=? g GENRE-COMEDY) #true]
    [(string=? g GENRE-DRAMA) #false]
    [(string=? g GENRE-ACTION) #true]
    [(string=? g GENRE-EDUCATION) #false]))

  
;; Design the function duration that returns the number of minutes
;; of content in a streaming queue.




;; Design the function upgrade that takes a streaming queue
;; and produces HD versions of all the videos


