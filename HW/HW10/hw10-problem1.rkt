;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 1

;; In most organizations, employeers are arranged in a hierarchy called an
;; "organization chart" or org-chart. For example, here is a portion of the 
;; org-chart for Northeastern:
;;
;;                      Joseph E. Aoun
;;                      (President)
;;                           |
;;                      The Cabinet                                   
;;                           |
;;       +-------------------+-------------------+
;;   Karl Reid      Madeleine Estabrook    David Madigan
;; (Chief Inclusion  (Student Affairs)    (Academic Affairs)
;;     Officer)                                 |
;;                    +-------------------------+---------+
;;                    |                                   |
;;              Administration                      Academic Deans
;;                    |                                   |
;;                    |                  +----------------+---------------+
;;                    |                  |                |               | 
;;              Thomas Sheahan      Alan Mislove    Carmen Sceppa    Uta Poiger
;;         (Curriculum & Programs)    (Khoury)         (Bouve)     (Social Sciences &
;;                                                                      Humanities)
;;
;; The people in this chart have a name (obviously) and a title. Each also
;; has a number of "direct reports" (possible no direct reports). In addition,
;; each set of direct reports is grouped together with a label. E.g., the three
;; groups above are "The Cabinet", "Administration", and "Academic Affairs".
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part A
;;
;; Design data called OrgChart to represent an org-chart.
;; One of your examples must have *all* the information in the org-chart picture
;; shown above.

;; [TODO] Data design

(define-struct employee [name title direct-reports])
; A Employee is a (make-employee String String [List-of Groups])
; - name is the name of an employee
; - title is the title of an employee
; - direct-reports is a list of employees who are direct reports

(define-struct org [employees])
; A Group is a (make-group Employee [List-of Employees])
; - leader is the top employee of the group in the hierarchy tree 
; - employees is a list of employees who are direct reports of a the leader


(define NO-DIR-REPORTS '())

(define EMPLOYEE-MISLOVE (make-employee "Alan Mislove" "Khoury" NO-DIR-REPORTS)) 
(define EMPLOYEE-SCEPPA (make-employee "Carmen Sceppa" "Bouve" NO-DIR-REPORTS))
(define EMPLOYEE-POIGER (make-employee "Uta Poiger" "Social Sciences & Humanities" NO-DIR-REPORTS))
(define ORG-ACADEMIC (make-org (list EMPLOYEE-MISLOVE EMPLOYEE-SCEPPA EMPLOYEE-POIGER)))

(define EMPLOYEE-SHEAHAN (make-employee "Thomas Sheahan" "Curriculum & Programs" NO-DIR-REPORTS))
(define ORG-ADMIN (make-org (list EMPLOYEE-SHEAHAN)))


(define EMPLOYEE-REID (make-employee "Karl Reid" "Chief Inclusion Officer" NO-DIR-REPORTS))
(define EMPLOYEE-ESTABROOK (make-employee "Madeleine Estabrook" "Student Affairs" NO-DIR-REPORTS))
(define EMPLOYEE-MADIGAN
  (make-employee "David Madigan" "Academic Affairs" (list ORG-ADMIN ORG-ACADEMIC)))
(define ORG-CABINET (make-org (list EMPLOYEE-REID EMPLOYEE-ESTABROOK EMPLOYEE-MADIGAN)))

(define EMPLOYEE-AOUN (make-employee "Joseph E. Aoun" "President" (list ORG-CABINET)))
(define ORG-NU (make-org (list EMPLOYEE-AOUN)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function num-peeps that counts how many people are in an OrgChart.

;; [TODO] Function design

(check-expect (num-peeps ORG-ADMIN) 1)
(check-expect (num-peeps ORG-ACADEMIC) 3)
(check-expect (num-peeps ORG-CABINET) 7)
(check-expect (num-peeps ORG-NU) 8)

(define (num-peeps o)
  (local [; num-peeps/lodr : [List-of Employee-direct-reports] -> NatNum
          ; how many employees are inside the group of direct reports?
          (define (num-peeps/lodr lodr)
            (cond
              [(empty? lodr) 0] 
              [(cons? lodr)
               (+  
                (num-peeps (first lodr))
                (num-peeps/lodr (rest lodr)))]))]  
    (+ (foldr + 0 (map num-peeps/lodr (map employee-direct-reports (org-employees o))))
       (length (org-employees o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function full-title that consumes an OrgChart and the name of
;; an organization, and produces an OrgChart that adds the "(organization name)"
;; to the title for each person in the OrgChart.

(define FULL-TITLE-MISLOVE (make-employee "Alan Mislove" "Northeastern Khoury" NO-DIR-REPORTS)) 
(define FULL-TITLE-SCEPPA (make-employee "Carmen Sceppa" "Northeastern Bouve" NO-DIR-REPORTS))
(define FULL-TITLE-POIGER
  (make-employee "Uta Poiger" "Northeastern Social Sciences & Humanities" NO-DIR-REPORTS))
(define FULL-TITLE-ACADEMIC (make-org (list FULL-TITLE-MISLOVE FULL-TITLE-SCEPPA FULL-TITLE-POIGER)))
(define FULL-TITLE-SHEAHAN
  (make-employee "Thomas Sheahan" "Northeastern Curriculum & Programs" NO-DIR-REPORTS))
(define FULL-TITLE-ADMIN (make-org (list FULL-TITLE-SHEAHAN)))

(check-expect (full-title ORG-ACADEMIC "Northeastern ") FULL-TITLE-ACADEMIC)
(check-expect (full-title ORG-ADMIN "Northeastern ") FULL-TITLE-ADMIN)

(define (full-title o org-name)
  (map (make-employee
        employee-name 
        (map employee-title (org-employees o)
             employee-direct-reports)
        (org-employees o)


                                           

        #|
  (map (λ (title) (string-append name " " title))
             (map employee-title (org-employees o)))) 


|#




        