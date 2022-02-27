;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; NOTE #1: You may use list abstractions if you wish. However, they are not
;; mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists of numbers and produces a list of
;; numbers where each number is the average of the two corresponding numbers in 
;; the original lists. You may assume that the two lists have equal length.

;; [TODO]

;; <solution>
;; NOTE: This is a parallel traversal.

;; average-of-two-lists : [List-of Number] [List-of Number] -> [List-of Number]
;; Produces a list of numbers where each number is the average of the
;; corresponding numbers in the two lists.

(check-expect (average-of-two-lists '() '()) '())

(check-expect (average-of-two-lists (list 1 2 3) (list 4 5 6))
              (list 2.5 3.5 4.5))


(define (average-of-two-lists list1 list2)
  (cond
    [(empty? list1) list1]
    [(cons? list1)
     (cons (/ (+ (first list1) (first list2)) 2)
           (average-of-two-lists (rest list1) (rest list2)))]))
;; </solution>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 2

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; NOTE #3: You *may not* use the builtin function replicate to solve this
;; problem.

;; Design a function that receives a number N and a list of strings, and
;; produces a list of strings where each output string is the corresponding
;; input string repeated N times.

;; [TODO] Function design *without using replicate*

;; <solution>
;; NOTE: This is a sequential traversal.

;; repeat-strings-solo : Nat [List-of String] -> [List-of String]
;; (repeat-strings-solo n slist) produces a list of strings from slist, where
;; each is duplicated n times.

(check-expect (repeat-strings-solo 3 '()) '())
(check-expect (repeat-strings-solo 0 (list "a" "b" "c")) (list "" "" ""))
(check-expect (repeat-strings-solo 2 (list "a" "b" "c")) (list "aa" "bb" "cc"))
(check-expect (repeat-strings-solo 3 (list "a" "b" "c")) (list "aaa" "bbb" "ccc"))

(define (repeat-strings-solo n slist)
  (cond
    [(empty? slist) slist]
    [(cons? slist) (cons (foldr string-append "" (build-list n (λ (_) (first slist))))
                         (repeat-strings-solo n (rest slist)))]))
;; </solution>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 3

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Design a function that receives two lists: a list of strings and a list of
;; numbers. The output should be a list of strings, where each string in the
;; output is the corresponding input string duplicated N times, where N
;; is the number in the corresponding list of numbers. However:
;;
;; 1. If there  are more strings than numbers, assume that the extra strings
;;    should be repeated twice each.
;; 1. If there are more numbers than strings, for each extra number N, 
;;    repeat the the string "Extra!" N times.

;; [TODO] Function design, and you *may* use replicate.

;; <solution>
;; NOTE: This is a parallel traversal.
;;
;; repeat-strings : [List-of String] [List-of Nat] -> [List-of String]
;; (repeat-strings slist nlist) produces a list of strings from slist, where
;; each is duplicated n times, where n is the corresponding number in
;; nlist. Repeats extra strings twice each, and for extra numbers, replicates
;; the string "Extra!".

(check-expect (repeat-strings '() '()) '())

(check-expect (repeat-strings (list "a" "b" "c") (list 1 2 3))
              (list "a" "bb" "ccc"))

(check-expect (repeat-strings (list "xx" "yy") (list 2 0))
              (list "xxxx" ""))
(check-expect (repeat-strings (list "xx" "yy" "zz") (list 0))
              (list "" "yyyy" "zzzz"))
(check-expect (repeat-strings (list "x") (list 2 3 4))
              (list "xx" "Extra!Extra!Extra!"  "Extra!Extra!Extra!Extra!"))

(define (repeat-strings slist nlist)
  (cond
    [(and (empty? slist) (empty? nlist)) '()]
    [(and (cons? slist) (cons? nlist))
       (cons (replicate (first nlist) (first slist))
             (repeat-strings (rest slist) (rest nlist)))]
    [(and (cons? slist) (empty? nlist))
        (cons (replicate 2 (first slist)) (repeat-strings (rest slist) '()))]
    [(and (empty? slist) (cons? nlist))
        (cons (replicate (first nlist) "Extra!") (repeat-strings '() (rest nlist)))]))
;; </solution>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 4

;; NOTE #1: You may use list abstractions if you wish. However,
;; they are not mandatory.

;; NOTE #2: The "2+ check-expects and no halloween colors" rule
;; applies.

;; Consider the following data definitions:
(define-struct student [name nuid])
;; A Student is a (make-student String Number)
;; Interpretation: (make-student name nuid) represents a student.

(define EX-STUDENT-1 (make-student "Alice" 1))
(define EX-STUDENT-2 (make-student "Bob" 2))
(define EX-STUDENT-3 (make-student "Carol" 3))

(define (student-template s)
  (... (student-name s) ... (student-nuid s) ...))

(define-struct grade [nuid course value])
;; A Grade is a (make-grade Number String Number)
;; Interpretation: (make-grade nuid course grade) represents the grade that
;; a student received in a course.

(define (grade-template g)
  (... (grade-nuid g) ... (grade-course g) ... (grade-value g) ...))

(define EX-GRADE-1 (make-grade 1 "Fundies 1" 95))
(define EX-GRADE-2 (make-grade 1 "Psychoceramics" 65))
(define EX-GRADE-3 (make-grade 2 "Programming Languages" 85))
(define EX-GRADE-4 (make-grade 2 "Fundies 1" 75))
(define EX-GRADE-5 (make-grade 3 "Fundies 1" 68))
(define EX-GRADE-6 (make-grade 3 "Cybernetics" 82))
(define EX-GRADE-7 (make-grade 3 "Phonology" 89))
(define EX-GRADE-8 (make-grade 4 "Fundies 1" 55))

(define-struct student-grades [name grades])
;; A StudentGrades is a (make-student-grades String [List-of Number]).
;; Interpretation: (make-student-grades name grades) represents the grades
;; that a student has received in all courses.

(define (student-grades-template sg)
  (... (student-grades-name sg) ... 
       ; i.e., template for [List-of Number]
       (lon-template (student-grades-grades sg)) ...))

(define EX-STUDENT-GRADES-1 (make-student-grades "Alice" (list 95 65)))
(define EX-STUDENT-GRADES-2 (make-student-grades "Bob" (list 85 75)))
(define EX-STUDENT-GRADES-3 (make-student-grades "Carol" (list 68 82 89)))

;; Design a function called students->student-grades that receives a list of 
;; Students and list of Grades, and produces a list of StudentGrades, where each
;; StudentGrade in the result is the list of all the grades received by a
;; student in all courses takes by that student.

;; NOTE: The list produced by students->student-grades should have an item for
;; every student in the input list, even if there are no grades for that 
;; student.

;; <solution>
;; NOTE: This is a cross-product traversal.

;; student-grades: [List-of Student] [List-of Grade] -> [List-of StudentGrades]
;; Produces a StudentGrade for each student, with the list of grades that
;; student received.

(check-expect
 (students->student-grades '() '())
 '())

(check-expect
 (students->student-grades '() (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4))
 '())

(check-expect
 (students->student-grades (list EX-STUDENT-1 EX-STUDENT-2) '())
 (list (make-student-grades "Alice" '()) (make-student-grades "Bob" '())))

(check-expect
 (students->student-grades (list EX-STUDENT-1)
                           (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4))
 (list EX-STUDENT-GRADES-1))

(check-expect
 (students->student-grades (list EX-STUDENT-1 EX-STUDENT-2 EX-STUDENT-3)
                           (list EX-GRADE-1 EX-GRADE-2 EX-GRADE-3 EX-GRADE-4
                                 EX-GRADE-5 EX-GRADE-6 EX-GRADE-7 EX-GRADE-8))
 (list EX-STUDENT-GRADES-1 EX-STUDENT-GRADES-2 EX-STUDENT-GRADES-3))

(define (students->student-grades students grades)
  (local [;;; grades-for-student : Student -> StudentGrades
          ;;; looks up all the grades for the supplied student
          (define (grades-for-student student)
            (make-student-grades
             (student-name student)
             (map grade-value
                  (filter
                   (λ (g) (= (grade-nuid g) (student-nuid student)))
                   grades))))]
    (map grades-for-student students)))

;; </solution>
