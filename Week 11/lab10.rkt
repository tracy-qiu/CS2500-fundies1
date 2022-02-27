;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 10

;; Consider the following data definitions and examples.

(define-struct file [name size])

;; A File is a (make-file String Nat)
;; - name is the name of the file (including extension)
;; - size is the size of the file in bytes

(define FILE-CV (make-file "cv.pdf" 466000))
(define FILE-HELLO (make-file "hello.rkt" 888))
(define FILE-PIC (make-file "pic.jpg" 968000))
(define FILE-SCHED (make-file "schedule.pdf" 288000))
(define FILE-P1 (make-file "p1.sql" 348))
(define FILE-P2 (make-file "p2.sql" 265))

(define-struct dir [name dirs files])

;; A Directory is a (make-dir String [List-of Directory] [List-of File])
;; - name is the name of the directory
;; - dirs is the list of sub-directories in this directory
;; - files is the list of files in this directory
;;   (not including the ones in sub-directories)
 
(define DIR-EMPTY (make-dir "nada" '() '()))

(define DIR-PERSONAL (make-dir "personal"
                               (list DIR-EMPTY)
                               (list FILE-CV FILE-PIC)))

(define DIR-CS2500 (make-dir "fundies" '() (list FILE-HELLO)))
(define DIR-CS3200 (make-dir "db" '() (list FILE-P1 FILE-P2)))

(define DIR-SCHOOL (make-dir "school"
                             (list DIR-CS2500 DIR-CS3200)
                             (list FILE-SCHED)))

(define DIR-ALL (make-dir "stuff" (list DIR-PERSONAL DIR-SCHOOL) '()))


;; TODO #1: write the templates for File, Directory, [List-of Directory],
;; and [List-of File].

;; <solution>
(define (file-temp f)
  (... (file-name f) ...
       (file-size f) ...))

(define (dir-temp d)
  (... (dir-name d) ...
       (lod-temp (dir-dirs d)) ...
       (lof-temp (dir-files d)) ...))

(define (lod-temp lod)
  (...
   (cond
     [(empty? lod) ...]
     [(cons? lod) ...
      (dir-temp (first lod)) ...
      (lod-temp (rest lod)) ...])))

(define (lof-temp lof)
  (...
   (cond
     [(empty? lof) ...]
     [(cons? lof) ...
      (file-temp (first lof)) ...
      (lof-temp (rest lof)) ...])))

;; </solution>

;; TODO #2: design the function total-files that takes a Directory
;; and produces the number of files in it, however deeply they might
;; be nested inside subdirectories. We have given you the check-expects
;; for it.

;; total-files : Directory -> Nat
;; Counts the total files in the directory
(check-expect (total-files DIR-EMPTY) 0)
(check-expect (total-files DIR-ALL) 6)

;; <solution>
(define (total-files d)
  (+
   (foldr + 0 (map total-files (dir-dirs d)))
   (length (dir-files d))))
;; </solution>

;; TODO #3: design the function file-found? that accepts a Directory and
;; a string and determines if a file with that name exists in the
;; directory or any of its subdirectories.

;; file-found? : Directory String -> Boolean
;; is a file with the supplied name in the directory?

(check-expect (file-found? DIR-EMPTY "hello.rkt") #false)
(check-expect (file-found? DIR-EMPTY "BAD.FILE") #false)
(check-expect (file-found? DIR-ALL "hello.rkt") #true)
(check-expect (file-found? DIR-ALL "BAD.FILE") #false)

;; <solution>
(define (file-found? d fname)
  (or
   (ormap (λ (sd) (file-found? sd fname)) (dir-dirs d))
   (ormap (λ (f) (string=? (file-name f) fname)) (dir-files d))))
;; </solution>

;; TODO #4: design the function rename-files that accepts a Directory
;; and two Strings (src and dest), which produces a Directory with all
;; the same subdirectories, but with all files named src renamed to
;; dest.

;; rename-files : Directory String String -> Directory
;; renames all files in a directory based upon a src/dest pair

(check-expect (rename-files DIR-EMPTY "pic.jpg" "pic.jpeg") DIR-EMPTY)
(check-expect
 (rename-files DIR-ALL  "pic.jpg" "pic.jpeg")
 (make-dir "stuff"
           (list
            (make-dir "personal"
                      (list DIR-EMPTY)
                      (list
                       FILE-CV
                       (make-file "pic.jpeg" 968000)))
            DIR-SCHOOL)
           '()))

;; <solution>
(define (rename-files d src dest)
  (local [; rename-file : File -> File
          ; (potentially) renames a file
          (define (rename-file f)
            (make-file (if (string=? (file-name f) src)
                           dest
                           (file-name f))
                       (file-size f)))]
    (make-dir (dir-name d)
              (map (λ (sd) (rename-files sd src dest)) (dir-dirs d))
              (map rename-file (dir-files d)))))
;; </solution>

;; TODO #5: design the function big-files that accepts a Directory and a
;; number of bytes and returns a list of file names in the directory with
;; at least the supplied size, sorted alphabetically.

(check-expect (big-files DIR-EMPTY 0) '())
(check-expect (big-files DIR-EMPTY 1000) '())
(check-expect (big-files DIR-ALL 0)
              (list "cv.pdf" "hello.rkt"
                    "p1.sql" "p2.sql"
                    "pic.jpg" "schedule.pdf"))
(check-expect (big-files DIR-ALL 1000)
              (list "cv.pdf" "pic.jpg" "schedule.pdf"))

;; big-files : Directory Nat -> [List-of String]
;; Produces all the files in the directory with at least the supplied
;; size, sorted alphabetically.

;; <solution>
(define (big-files d n)
  (local [; file-big-enough? : File -> Boolean
          ; is the file big enough to keep?
          (define (file-big-enough? f)
            (>= (file-size f) n))

          ; files-to-keep : [List-of File] -> [List-of String]
          ; returns a file names to keep based on size
          (define (files-to-keep lof)
            (map file-name
                 (filter file-big-enough? lof)))

          ; get-files : Directory -> [List-of String]
          ; gets all the files of sufficient size
          (define (get-files d)
            (append
             (foldr append '()
                    (map get-files (dir-dirs d)))
             (files-to-keep (dir-files d))))]
    (sort (get-files d) string<?)))
;; </solution>
