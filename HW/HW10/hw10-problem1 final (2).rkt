;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10-problem1 final (2)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; A Employee is a (make-employee String String [List-of OrgCharts])
; - name is the name of an employee
; - title is the title of an employee
; - direct-reports is a list of org charts who are direct reports of an employee 
;; represents an employee 

(define (employee-temp e)
  (... (employee-name e) ...
       (employee-title e) ...
       (group-temp (employee-direct-reports e)) ...))

(define-struct group [title employees])
; A Group is a (make-group String [List-of OrgCharts])
; - title is the label of the group
; - employees is a list of org charts who are a part of the group
;; represents a group of employees

(define (group-temp g)
  (... (group-title g) ...
       (employee-temp (group-employees g)) ...))

(define NO-DIR-REPORTS '())

(define EMPLOYEE-MISLOVE (make-employee "Alan Mislove" "Khoury" NO-DIR-REPORTS)) 
(define EMPLOYEE-SCEPPA (make-employee "Carmen Sceppa" "Bouve" NO-DIR-REPORTS))
(define EMPLOYEE-POIGER (make-employee "Uta Poiger" "Social Sciences & Humanities" NO-DIR-REPORTS))
(define GROUP-ACADEMIC (make-group "Academic Affairs" (list EMPLOYEE-MISLOVE
                                                            EMPLOYEE-SCEPPA
                                                            EMPLOYEE-POIGER)))

(define EMPLOYEE-SHEAHAN (make-employee "Thomas Sheahan" "Curriculum & Programs" NO-DIR-REPORTS))
(define GROUP-ADMIN (make-group "Administration" (list EMPLOYEE-SHEAHAN)))

(define EMPLOYEE-REID (make-employee "Karl Reid" "Chief Inclusion Officer" NO-DIR-REPORTS))
(define EMPLOYEE-ESTABROOK (make-employee "Madeleine Estabrook" "Student Affairs" NO-DIR-REPORTS))
(define EMPLOYEE-MADIGAN (make-employee "David Madigan" "Academic Affairs"
                                        (list GROUP-ADMIN
                                              GROUP-ACADEMIC)))
(define GROUP-CABINET (make-group "The Cabinet" (list EMPLOYEE-REID
                                                      EMPLOYEE-ESTABROOK
                                                      EMPLOYEE-MADIGAN)))

(define EMPLOYEE-AOUN (make-employee "Joseph E. Aoun" "President"
                                     (list GROUP-CABINET)))

; An OrgChart is one of:
; - (make-employee String String [List-of OrgCharts])
; - (make-group String [List-of OrgCharts])
; Represents employeers arranged in a hierarchy

(define NU-ORG-CHART-1 (make-group "Organization Chart" (list EMPLOYEE-AOUN
                                                              GROUP-ACADEMIC
                                                              GROUP-CABINET
                                                              GROUP-ADMIN)))
(define NU-ORG-CHART-2 EMPLOYEE-REID)
(define NU-ORG-CHART-3 GROUP-CABINET)

(define (org-chart-temp oc)
  (...
   (cond
     [(employee? oc) (employee-temp oc) ...]
     [(group? oc) (group-temp oc) ...])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part B

;; Design the function num-peeps that counts how many people are in an OrgChart.

;; [TODO] Function design

; num-peeps : OrgChart -> NatNumber
; counts how many people are in an OrgChart
(check-expect (num-peeps EMPLOYEE-AOUN) 8)
(check-expect (num-peeps EMPLOYEE-REID) 1)
(check-expect (num-peeps EMPLOYEE-MADIGAN) 5)

(define (num-peeps o)
  (cond
    [(employee? o) (+ 1 (foldr (lambda (d e) (+ (num-peeps d) e)) 0 (employee-direct-reports o)))]
    [(group? o) (foldr (lambda (d e) (+ (num-peeps d) e)) 0 (group-employees o))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part C

;; Design the function full-title that consumes an OrgChart and the name of
;; an organization, and produces an OrgChart that adds the "(organization name)"
;; to the title for each person in the OrgChart.
(define EMPLOYEE-REID-2 (make-employee "Karl Reid" "Khoury Chief Inclusion Officer" NO-DIR-REPORTS))
(define EMPLOYEE-ESTABROOK-2 (make-employee "Madeleine Estabrook" "Khoury Student Affairs"
                                            NO-DIR-REPORTS))
(define EMPLOYEE-SHEAHAN-2 (make-employee "Thomas Sheahan" "Khoury Curriculum & Programs"
                                          NO-DIR-REPORTS))
(define GROUP-ADMIN-2 (make-group "Administration" (list EMPLOYEE-SHEAHAN-2)))
(define EMPLOYEE-MISLOVE-2 (make-employee "Alan Mislove" "Khoury Khoury" NO-DIR-REPORTS)) 
(define EMPLOYEE-SCEPPA-2 (make-employee "Carmen Sceppa" "Khoury Bouve" NO-DIR-REPORTS))
(define EMPLOYEE-POIGER-2 (make-employee "Uta Poiger" "Khoury Social Sciences & Humanities"
                                         NO-DIR-REPORTS))
(define GROUP-ACADEMIC-2 (make-group "Academic Affairs" (list EMPLOYEE-MISLOVE-2
                                                              EMPLOYEE-SCEPPA-2
                                                              EMPLOYEE-POIGER-2)))
(define EMPLOYEE-MADIGAN-2 (make-employee "David Madigan" "Khoury Academic Affairs"
                                          (list GROUP-ADMIN-2
                                                GROUP-ACADEMIC-2)))

; full-title : OrgChart String -> OrgChart
; Given an OrgChart and the name of an organization,
; adds the organization name to each employee's title
(check-expect (full-title EMPLOYEE-MISLOVE "Northeastern") (make-employee "Alan Mislove"
                                                                          "Northeastern Khoury"
                                                                          NO-DIR-REPORTS))
(check-expect (full-title NU-ORG-CHART-2 "Northeastern")
              (make-employee "Karl Reid"
                             "Northeastern Chief Inclusion Officer"
                             NO-DIR-REPORTS))
(check-expect (full-title NU-ORG-CHART-3 "Khoury") (make-group "The Cabinet"
                                                               (list EMPLOYEE-REID-2
                                                                     EMPLOYEE-ESTABROOK-2
                                                                     EMPLOYEE-MADIGAN-2)))

(define (full-title org name)
  (local
    ; title-changer-e : Employee String -> Employee
    ; takes an employee and adds a given name to the title
    [(define (title-changer-e e name)
       (make-employee (employee-name e)
                      (string-append name " " (employee-title e))
                      (map (lambda (e) (full-title e name)) (employee-direct-reports e))))]
    (cond
      [(employee? org) (title-changer-e org name)]
      [(group? org) (make-group (group-title org)
                                (map (lambda (e) (full-title e name)) (group-employees org)))])))











