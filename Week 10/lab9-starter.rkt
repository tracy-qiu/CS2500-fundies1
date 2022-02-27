;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 9

(require 2htdp/batch-io)

;; You are to design a set of functions below. Each has a set of
;; restrictions that apply to the problem, so pay attention to
;; the instructions for each TODO.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A

;; Design the function alternate which, given two lists
;; produces a list of alternating elements from each list (and if
;; one list runs out of elements you should place all the remaining
;; elements in the other list at the end). Some tests have been
;; supplied for clarity.

;; Restrictions:
;; - Do NOT use ISL list abstractions.

;; alternate : [List-of X] [List-of X] -> [List-of X]
;; Produces a list resulting from alternating between the two
;; supplied lists.

(define (alternate l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (cons? l2)) l2]
    [(and (cons? l1) (empty? l2)) l1]
    [(and (cons? l1) (cons? l2))
     (cons (first l1)
           (cons (first l2)
                 (alternate (rest l1) (rest l2))))]))

(check-expect
 (alternate '() '())
 '())

(check-expect
 (alternate '() (list 1 "a"))
 (list 1 "a"))

(check-expect
 (alternate (list 1 2 3) '())
 (list 1 2 3))

(check-expect
 (alternate (list 1 2)
            (list "a" "b" "c"))
 (list 1 "a" 2 "b" "c"))

(check-expect
 (alternate (list "a" "b" "c") '())
 (list "a" "b" "c"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function cross-product which, given two lists
;; produces a cross product of their elements. Some tests have been
;; supplied for clarity.
;;
;; Restrictions:
;; - Do NOT use ISL list abstractions.

;; cross-product : [List-of Any] [List-of Any] -> [List-of (list Any Any)]
;; produces the cross product of the two lists

#|
(define (cross-product l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (cons? l2)) '()]
    [(and (cons? l1) (empty? l2)) '()]
    [(and (cons? l1) (cons? l2))
     (list (cons (first l1) (first l2)) (cons (cross-product (first l1) (rest l2))))]))
 

; products : Any [List-of Any] -> (list Any Any)

(define (products thing1 thing2)
  (list thing1 thing2))

   
(check-expect
 (cross-product '() '())
 '())

(check-expect
 (cross-product (list 1 2) '())
 '())

(check-expect
 (cross-product '() (list 1 2))
 '())

(check-expect
 (cross-product (list 1 2) (list "a" "b" "c"))
 (list (list 1 "a")
       (list 1 "b")
       (list 1 "c")
       (list 2 "a")
       (list 2 "b")
       (list 2 "c")))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function mymap2, which takes in two lists and returns
;; the list of results of performing on operation on the first from each list,
;; the second, and so on. If one of the list runs out of elements, then stop
;; producing results. Some tests have been supplied for clarity.
;;
;; Restrictions:
;; - Do NOT use ISL list abstractions.


;; mymap2 : [List-of X] [List-of Y] [X Y -> Z] -> [List-of Z]
;; produces a result list by applying the function to pairs of elements
;; from the supplied list (until one is empty)

(check-expect
 (mymap2 '() '() *)
 '())

(check-expect
 (mymap2 (list 2 3) (list 4) *)
 (list 8))

(check-expect
 (mymap2 (list "a" "b")
         (list "1" "2" "3")
         string-append)
 (list "a1" "b2"))

(check-expect
 (mymap2 (list "a" "c")
         (list (list "x") (list 3 1 4))
         (λ (s l) (string-append
                   s
                   "-"
                   (number->string (length l)))))
 (list "a-1" "c-3"))

(define (mymap2 l1 l2 f)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(and (empty? l1) (cons? l2)) '()]
    [(and (cons? l1) (empty? l2)) '()]
    [(and (cons? l1) (cons? l2))
     (cons (f (first l1) (first l2))
                (mymap2 (rest l1) (rest l2) f))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part D

;; Design the function read-lines-or-empty that attempts to read
;; the lines of a file (via a supplied name) each as a string in a list,
;; or returns an empty list if the file is not found. Some tests have been
;; supplied for clarity.
;;
;; Restrictions:
;; - Do NOT use ISL list abstractions.

;; read-lines-or-empty : String -> [List-of String]
;; attempts to read the lines of a file
  
(define BADFILE "BADFILENAME.SAD") 

(check-expect
 (read-lines-or-empty BADFILE)
 '())

(check-expect
 (read-lines-or-empty "progs.txt")
 (list "CS" "DS" "CY"))

(check-expect
 (read-lines-or-empty "nums.txt")
 (list "1800" "3000" "2550" "4300"))

(define (read-lines-or-empty file)
  (if (file-exists? file) (read-words file) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part E

;; TODO #5: design the function cat-files, which takes two files and produces
;; a list of all the lines of the first file (as strings) followed by the
;; second. If either file doesn't exist, interpret that as having no lines.
;; Some tests have been supplied for clarity.
;;
;; Restrictions:
;; - Do NOT use ISL list abstractions.
;; - You are NOT allowed to use append; that would make it a bit too easy ;)
;;
;; (But yes, you are encouraged to make use of prior functions you wrote
;; in this file to solve this problem.)
;;


;; cat-files : String String -> [List-of String]
;; concatenates the supplied files

(check-expect
 (cat-files "progs.txt" "nums.txt")
 (list "CS" "DS" "CY"
       "1800" "3000" "2550" "4300"))

(check-expect
 (cat-files "progs.txt" BADFILE)
 (list "CS" "DS" "CY"))

(check-expect
 (cat-files BADFILE "progs.txt")
 (list "CS" "DS" "CY"))

(define (cat-files file1 file2)
  (cond
    [(and (not (file-exists? file1)) (not (file-exists? file2))) '()]
    [(and (file-exists? file1) (not (file-exists? file2))) (read-words file1)]
    [(and (not (file-exists? file1)) (file-exists? file2)) (read-words file2)]
    [(and (file-exists? file1) (file-exists? file2)) (append (read-words file1) (read-words file2))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part F

;; Design the function cat-lines, which takes two files and produces
;; the result of concatenating all their lines together (with an intermediate
;; space). Some tests have been supplied for clarity.
;;
;; Restrictions:
;; - Do NOT use ISL list abstractions.
;;
;; (But yes, you are encouraged to make use of prior functions you wrote
;; in this file to solve this problem.)

;; cat-lines : String String -> [List-of String]
;; glues together the matching file lines

(define (cat-lines file1 file2)
  (cond
    [(or (not (file-exists? file1)) (not (file-exists? file2))) '()]
    [(and (file-exists? file1) (file-exists? file2))
     (mymap2 (read-lines file1) (read-lines file2) string-append-space)]))

(define (string-append-space s1 s2)
  (string-append s1 " " s2))

(check-expect
 (cat-lines "progs.txt" "nums.txt")
 (list "CS 1800" "DS 3000" "CY 2550"))

(check-expect
 (cat-lines "progs.txt" BADFILE)
 '())

(check-expect
 (cat-lines BADFILE "progs.txt")
 '())


