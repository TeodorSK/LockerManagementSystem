#lang racket/base
(require db)
(require racket/list)
(require csv-reading)



;-----== structs ==------
;These are just local references
;TODO: maybe useless, along with db-lockers
(struct locker
  (id
   db))

(struct student
  (id
   db))

;-----== definitions ==------



;-----== aux functions ==------

;init db returns db
(define (init-db! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (unless (table-exists? db "lockers")
    (query-exec db
                (string-append
                 "CREATE TABLE lockers (id INTEGER PRIMARY KEY, location TEXT, lockid INTEGER)"))
    
    (insert-locker db 14 "near here (sample locker)" 0))
  (unless (table-exists? db "students")
    (print "Creating students table")
    (query-exec db
                (string-append
                 "CREATE TABLE students (id INTEGER PRIMARY KEY, firstname TEXT, lastname TEXT, program TEXT, email TEXT)"))
    (insert-student db 1 "john" "doe" "compsci" "abc@abc.com"))
  db)

(define (db-import-locker-csv! a-db file)
  (define (extract-locker-data-from-row a-row)
    (define locker-num (list-ref a-row 1))
    (define locker-location (list-ref a-row 3))
    (if (and
         (string->number locker-num) ;check if unique      
         (string? locker-location))
        (insert-locker a-db locker-num locker-location 0)
        ;(list locker-num locker-location)
        "incorrect format (not unique or not int, str)"))
  (csv-map extract-locker-data-from-row file))

(define (db-import-student-csv! a-db file)
  (define (extract-student-data-from-row a-row)
    (define student-id (list-ref a-row 0))
    (define student-firstname (list-ref a-row 1))
    (define student-lastname (list-ref a-row 2))
    (define student-program (list-ref a-row 3))
    (define student-email (list-ref a-row 4))
    (if (and
         (string->number student-id) ;check if unique      
         (string? student-firstname)
         (string? student-lastname)
         (string? student-program)
         (string? student-email))
        (insert-student a-db student-id student-firstname student-lastname student-program student-email)
        "incorrect format (not unique or not int, str)"))
  (csv-map extract-student-data-from-row file))


(define (extract-locker-data-from-row a-db a-row)
  (define locker-num (list-ref a-row 1))
  (define locker-location (list-ref a-row 3))
  (if (and
       (string->number locker-num) ;check if unique      
       (string? locker-location))
      (insert-locker a-db (string->number locker-num) locker-location 0)
      "incorrect format (not unique or not int, str)"))


;Next two functions may be very redundant
;lists all id-s of students
(define (db-students a-db)
  (define (id->student an-id)
    (student an-id a-db))
  (map id->student
       (query-list a-db "SELECT id FROM students")))

;lists all id-s of lockers
(define (db-lockers a-db)
  (define (id->locker an-id)
    (locker an-id a-db))
  (map id->locker
       (query-list a-db "SELECT id FROM lockers")))

(define (student-firstname a-student)
  (query-value
   (student-db a-student)
   "SELECT firstname FROM students WHERE id = ?"
   (student-id a-student)))

(define (student-lastname a-student)
  (query-value
   (student-db a-student)
   "SELECT lastname FROM students WHERE id = ?"
   (student-id a-student)))

(define (student-program a-student)
  (query-value
   (student-db a-student)
   "SELECT program FROM students WHERE id = ?"
   (student-id a-student)))

(define (student-email a-student)
  (query-value
   (student-db a-student)
   "SELECT email FROM students WHERE id = ?"
   (student-id a-student)))

(define (locker-location a-locker)
  (query-value
   (locker-db a-locker)
   "SELECT location FROM lockers WHERE id = ?"
   (locker-id a-locker)))


;this is how they should all be, instead of relying on local locker object
(define (temp-locker-location a-db id)
  (query-value
   a-db
   "SELECT location FROM lockers WHERE id = ?"
   id))

(define (insert-student a-db id firstname lastname program email)
  (query-exec
   a-db
   "INSERT INTO students (id, firstname, lastname, program, email) VALUES (?, ?, ?, ? ,?)"
   id firstname lastname program email))

(define (insert-locker a-db id location lockid)
  (query-exec
   a-db
   "INSERT INTO lockers (id, location, lockid) VALUES (?, ?, ?)"
   id location lockid))

(define (clear-selected-lockers! a-db locker-ids)
  (define (clear-db! an-id)
    (query-exec
     a-db
     "DELETE FROM lockers WHERE id = ?"
     an-id))
  (print locker-ids)
  (print "^^locker ids")
  (map clear-db! locker-ids))




(provide (all-defined-out))

