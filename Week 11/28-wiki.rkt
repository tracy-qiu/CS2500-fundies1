;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 28-wiki) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct page [title links])
 
; A WebPage is a (make-page String [List-of String])
; Interpretation: a web page's title and links to other pages.
 
(define PAGE-0 (make-page "Khoury" (list "NEU")))
(define PAGE-1 (make-page "NEU" (list "Khoury" "Boston")))
(define PAGE-2 (make-page "Boston" (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE-4 (make-page "New Orleans" (list "Mardi Gras")))
 
(define (wp-temp wp)
  (... (page-title wp) ...
       (los-temp (page-links wp)) ...))
 
; A Wiki is a [List-of WebPage]
; Interpretation: A list of pages in a wiki
 
(define WIKI-1 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3 PAGE-4))
 
(define (wiki-temp w)
  (...
   (cond
     [(empty? w) ...]
     [(cons? w)
      (...
       (wp-temp (first w)) ...
       (wiki-temp (rest w)) ...)])))


; TODO #1: design a function num-pages that
; accepts a Wiki and returns the number of
; pages mentioned.

; num-pages : Wiki -> Nat
; Returns the number of pages in a wiki

(check-expect (num-pages (list PAGE-0)) 2)
(check-expect (num-pages WIKI-1) 6)

(define (num-pages w)
  (local [; get-ALL-the-pages : Wiki -> [List-of String]
          ; returns the names of all pages referenced
          (define (get-ALL-the-pages wiki)
            (cond 
              [(empty? wiki) '()]
              [(cons? wiki)
               (merge
                (cons (page-title (first wiki))
                      (page-links (first wiki)))
                get-ALL-the-pages (rest wiki))]))
          ; merge : [List-of String] [List-of String] -> [List-of String]
          ; adds those strings from the first list that don't already exist in the second
          (define (merge l1 l2)
            (cond
              [(empty? l1) l2]
              [(cons? l1)
               (local [(define DISTINCT-REST (merge (rest l1) l2))]
               (if (s-in-los? (first l1) DISTINCT-REST)
                   DISTINCT-REST
                   (cons (first l1) DISTINCT-REST)))]))]
    (length (get-ALL-the-pages w))))

; s-in-los? : String [List-of String] -> Boolean
; is the supplied string in the supplied list?

(check-expect (s-in-los? "a" '()) #false)
(check-expect (s-in-los? "a" (list "a" "b" "c")) #true)

(define (s-in-los? s los)
  (ormap))



; TODO #2: Design a function connected? that
; accepts two strings representing two pages
; in a wiki, and a wiki. It should return whether
; or not there is a path from the first page to
; the second.

; connected? : String String Wiki -> Boolean
; Determines whether or not the two pages have a path between them
; Termination: each time we recur, we remove a page from the wiki;
; eventually the finite-sized wiki will run out of pages and thus
; there will be nothing left to recur upon 

(check-expect (connected? "NEU" "Khoury" WIKI-1) #true)
(check-expect (connected? "Boston" "Khoury" WIKI-1) #true)
(check-expect (connected? "New Orleans" "Khoury" WIKI-1) #false)

(define (gen-recur-f p)
  (...
   (cond
     [(trivial1? p) ...]
     [(trivial1? p) ...]
     ...
     [(not-so-tricial? p)
      (combine-problems
       (gen-recur-f (generate-subproblem1 p))
       (gen-recur-f (generate-subproblem2 p))
       ...)])))
     

(define (connected? p1 p2 w)
  (local [(define LINKS-FROM-P1 (get-links w p1))
          (define IN-P1-LINKS? (s-in-los? p2 LINKS-FROM-P1))]
    (or IN-P1-LINKS?
        (ormap (λ (p) (connected? p p2 w)) LINKS-FROM-P1))))


; get-links : Wiki String -> [List-of String]
; gets the links of the named page in a wiki

(check-expect (Get-links WIKI-1 "Boston") (list "NEU" "Shillman Hall"))
(check-expect (Get-links WIKI-1 "BADLINK") '())

(define (get-links w search-title)
  (cond
     [(empty? w) '()]
     [(cons? w)
      (if (string=? (page-title (first w)) search-title)
          (page-links (first w))
          (get-links (rest w) search-title))]))













