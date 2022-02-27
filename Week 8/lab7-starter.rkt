;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 7

;; For each TODO below, refer to the following data definitions, and use
;; pre-defined list abstraction(s) when appropriate. As a reminder, they
;; include (but are not limited to):
;;
;; - map
;; - filter
;; - andmap
;; - ormap
;; - foldr
;; - foldl
;;
;; There are other list abstractions, but you aren't going to need them for this
;; lab!

;; Just because we now have cool abstractions doesn't mean you should forget
;; about the design recipe and following templates (which particularly come up
;; for abstraction helpers)!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions (do not change these)

;; A Genre is one of
;; - "Pop"
;; - "Classical"
;; - "Country"
;; - "Rock"
;; Interpretation: a song genre

(define GENRE-POP "Pop")
(define GENRE-CLASSICAL "Classical")
(define GENRE-COUNTRY "Country")
(define GENRE-ROCK "Rock")

(define (genre-temp g)
  (cond
    [(string=? g GENRE-POP) ...]
    [(string=? g GENRE-CLASSICAL) ...]
    [(string=? g GENRE-COUNTRY) ...]
    [(string=? g GENRE-ROCK) ...]))


(define-struct song [name artist duration genre fav?])

;; A Song is a (make-song String String Nat Genre Boolean)
;; Interpretation: a song
;; - name is the title of the song
;; - artist is the song's artist
;; - duration is the length in seconds
;; - genre is the song's genre
;; - fav? is this a liked song?

(define SONG-1 (make-song "Redesigning Women" "The Highwomen" 174 GENRE-COUNTRY #true))
(define SONG-2 (make-song "Your Song" "Elton John" 241 GENRE-POP #true))
(define SONG-3 (make-song "All Along the Watchtower" "Jimi Hendrix" 241 GENRE-ROCK #false))
(define SONG-4 (make-song "Nessun Dorma" "Luciano Pavarotti" 184 GENRE-CLASSICAL #false))

(define (song-temp song)
  (... (song-name song) ...
       (song-artist song) ...
       (song-duration song) ...
       (genre-temp (song-genre song)) ...
       (song-fav? song) ...))

(define-struct pl [name songs])
;; A Playlist is a (make-pl String [List-of Song])
;; Interpretation: a sequence of songs
(define PL-0 (make-pl "Quiet :)" '()))
(define PL-1 (make-pl "Coding Beats" (list SONG-1 SONG-2 SONG-3 SONG-4)))
(define (pl-temp pl)
  (... (pl-name pl) ...
       (los-temp (pl-songs pl)) ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design the function all-names that produces a list of all the names of all
;; the songs on a playlist. We have given you a signature, purpose statement,
;; and check-expects. So, you only need to write the function body!

;; all-names : Playlist -> [List-of String]
;; Produces all the song titles from a playlist.
(check-expect (all-names PL-0) '())
(check-expect
 (all-names PL-1) 
 (list "Redesigning Women" "Your Song" "All Along the Watchtower" "Nessun Dorma"))

(define (all-names pl)
  (map song-name (pl-songs pl)))
   
  

;; [TODO] Complete the function body

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function any-pop? that determines if a playlist has any pop songs.
;; We have given you the signature, purpose statement, and check-expects. So,
;; you just need to complete the body, and design a helper function.

;; any-pop? : Playlist -> Boolean
;; Determines if any are pop songs.

(check-expect (any-pop? PL-0) #false)
(check-expect (any-pop? PL-1) #true)
(check-expect (any-pop? (make-pl "Infinite repeat" (list SONG-3))) #false)

(define (any-pop? pl)
  (ormap genre-pop? (pl-songs pl)))
   
(define (genre-pop? s)
   (string=? (song-genre s) GENRE-POP))

;; [TODO] Function design.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function only-faves that when supplied a playlist returns a new 
;; playlist (with the name "Faves") that only contains the liked songs.

;; only-faves : Playlist -> Playlist
; given a playlist, returns a new playlist that only contains liked songs 

(define PL-2 (make-pl "Coding Beats v2" (list SONG-2 SONG-4)))

(check-expect (only-faves PL-0) '())
(check-expect (only-faves PL-1) (list SONG-1 SONG-2))
(check-expect (only-faves PL-2) (list SONG-2))

;; [TODO] Function design

(define (only-faves pl)
  (filter song-fav? (pl-songs pl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part D

;; Design the function all-short? that determines if a playlist contains only 
;; songs shorter than three minutes (180 seconds).

;; [TODO] Function design.

; all-short? : Playlist -> Boolean
; determines if a playlist contains only songs shorter than three minutes 

(check-expect (all-short? PL-0) #true)
(check-expect (all-short? PL-1) #false)
(check-expect (all-short? (make-pl "Short Song" (list SONG-1))) #true)

(define (all-short? pl)
  (andmap short-song? (pl-songs pl)))

(define (short-song? s)
   (< (song-duration s) 180))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part E

;; Design the function total-duration that returns  the total length of a 
;; playlist.

;; [TODO] Function design.

; total-duration : Playlist -> Nat
; given a playlist, returns the total length of the playlist

(check-expect (total-duration PL-0) 0)
(check-expect (total-duration PL-1) 840)

(define (total-duration pl)
  (foldr + 0 (all-durations pl)))

(define (all-durations pl)
  (map song-duration (pl-songs pl)))
