#lang racket/base
(require db)
(require racket/list)
(require csv-reading)
(require web-server/servlet)

;init db returns db
(define (init-db! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (unless (table-exists? db "lockers")
    (query-exec db
                (string-append
                 "CREATE TABLE lockers (id INTEGER PRIMARY KEY, location TEXT, lock_id INTEGER, is_broken BIT)"))
    
    (insert-locker db 1 "near here (sample locker)" 0 0))
  (unless (table-exists? db "students")
    (query-exec db
                (string-append
                 "CREATE TABLE students (id INTEGER PRIMARY KEY, firstname TEXT, lastname TEXT, program TEXT, email TEXT)"))
    (insert-student db 1 "john" "doe" "compsci" "abc@abc.com"))
  (unless (table-exists? db "student_locker")
    (query-exec db
                (string-append
                 "CREATE TABLE student_locker (
        	student_locker_id	INTEGER,
        	student_id      	INTEGER UNIQUE,
        	locker_id       	INTEGER UNIQUE,
        	valid_until     	TEXT,
        	PRIMARY KEY(student_locker_id AUTOINCREMENT),
        	FOREIGN KEY(student_id) REFERENCES students(id),
        	FOREIGN KEY(locker_id) REFERENCES lockers(id))")) ;database7.db has valid_until as integer, beware
    (insert-student-locker db 1 1 "forever"))
  db)

(define (db-import-locker-csv! a-db file)
  (define (extract-locker-data-from-row a-row)
    (define locker-num (list-ref a-row 1))
    (define locker-location (list-ref a-row 3))
    (if (and
         (string->number locker-num) ;check if unique      
         (string? locker-location))
        (insert-locker a-db locker-num locker-location 0 0)
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
         (string->number student-id) ;check if unique ; Actually unique check is in the CREATE TABLE statement, just add exception catcher         
         (string? student-firstname)
         (string? student-lastname)
         (string? student-program)
         (string? student-email))
        (insert-student a-db student-id student-firstname student-lastname student-program student-email)
        "incorrect format (not unique or not int, str)")) ; this dont work
  (csv-map extract-student-data-from-row file))

(define (extract-locker-lock-columns a-db file) ;file -> (list lockers locks)
  (define lockers (list))
  (define locks (list))
  (define (extract-lock-row a-row)
  
    (cond [(and (string->number (list-ref a-row 1)) (string->number (list-ref a-row 2)))
           (set! lockers (append (list (list-ref a-row 1)) lockers))
           (set! locks (append (list (list-ref a-row 2)) locks))]))       
  (csv-map extract-lock-row file)
  (values lockers locks))

(define (assign-locks! a-db file)    
  (define (extract-lock-row a-row)
    (define locker-id (list-ref a-row 1))
    (define lock-id (list-ref a-row 2))
    (insert-locker-lock a-db locker-id lock-id))
  (csv-map extract-lock-row file))

(define (insert-locker-lock a-db locker-id lock-id)
  (query-exec
   a-db
   "UPDATE lockers SET lock_id = ? WHERE id = ?"
   lock-id locker-id))


(define (extract-locker-data-from-row a-db a-row)
  (define locker-num (list-ref a-row 1))
  (define locker-location (list-ref a-row 3))
  (if (and
       (string->number locker-num) ;check if unique      
       (string? locker-location))
      (insert-locker a-db (string->number locker-num) locker-location 0)
      "incorrect format (not unique or not int, str)"))

(define (filter-students a-db [student-id "%"][firstname "%"][lastname "%"][program "%"][email "%"])
  (query-list
   a-db
   "SELECT id FROM students
    WHERE id LIKE ? AND firstname LIKE ? AND lastname LIKE ? AND program LIKE ? AND email LIKE ?" student-id firstname lastname program email))

(define (filter-lockers a-db locker-id location owned broken)
  (filter (λ (locker) (or (string=? broken "all")(string=? broken (if(locker-broken? a-db locker)"yes""no"))))
  (filter (λ (locker) (or (string=? owned "all")(string=? owned (if(locker-owned? a-db locker)"yes""no"))))
          (query-list
           a-db
           "SELECT id FROM lockers
           WHERE id LIKE ? AND location LIKE ?" locker-id (string-append "%" location "%")))))

;Lists all IDs
(define (all-students a-db)
  (query-list a-db "SELECT id FROM students"))

(define (all-lockers a-db)
  (query-list a-db "SELECT id FROM lockers"))

(define (all-programs a-db)
  (query-list a-db "SELECT DISTINCT program FROM students"))

(define (student-firstname a-db id)
  (query-value
   a-db
   "SELECT firstname FROM students WHERE id = ?"
   id))

(define (student-lastname a-db id)
  (query-value
   a-db
   "SELECT lastname FROM students WHERE id = ?"
   id))

(define (student-program a-db id)
  (query-value
   a-db
   "SELECT program FROM students WHERE id = ?"
   id))

(define (student-email a-db id)
  (query-value
   a-db
   "SELECT email FROM students WHERE id = ?"
   id))

(define (locker-location a-db id)
  (query-value
   a-db
   "SELECT location FROM lockers WHERE id = ?"
   id))

(define (locker-owner-id a-db id)
  (define (exn-handler exn)
    -1)
  (with-handlers ([exn? exn-handler])
    (query-value
     a-db
     "SELECT id FROM students WHERE id =
(SELECT student_id FROM student_locker WHERE locker_id = ?)"
     id)))

(define (lock-id a-db id) ;locker-id -> lock-id
  (define (exn-handler exn)
    -1)
  (with-handlers ([exn? exn-handler])
    (query-value
     a-db
     "SELECT lock_id FROM lockers WHERE id = ?"
     id)))
  
(define (locker-has-lock? a-db locker-id)
  (define (exn-handler exn)
    #f)
  (with-handlers ([exn? exn-handler])
    (if (query-value
         a-db
         "SELECT 1 FROM lockers WHERE id = ? AND lock_id <> '' AND lock_id <> 0"
         locker-id)
        #t
        #f)))

(define (locker-broken? a-db locker-id)
  (define (exn-handler exn)
    #f)
  (with-handlers ([exn? exn-handler])
    (if (not (equal? (query-value
                      a-db
                      "SELECT is_broken FROM lockers WHERE id = ?"
                      locker-id) 0)) #t #f)))

(define (set-locker-broken! a-db locker-id)
  (query-exec
   a-db
   "UPDATE lockers SET is_broken = 1 WHERE id = ?"
   locker-id))

(define (set-locker-fixed! a-db locker-id)
  (query-exec
   a-db
   "UPDATE lockers SET is_broken = 0 WHERE id = ?"
   locker-id))

(define (locker-owner-name a-db id) ;locker-id -> student-name
  (define (exn-handler exn)
    "No owner")
  (with-handlers ([exn? exn-handler])
    (string-append
     (query-value
      a-db
      "SELECT firstname FROM students WHERE id =
(SELECT student_id FROM student_locker WHERE locker_id = ?)"
      id)
     " "
     (query-value
      a-db
      "SELECT lastname FROM students WHERE id =
(SELECT student_id FROM student_locker WHERE locker_id = ?)"
      id))))

(define (student-name a-db id)
  (string-append
   ;(student-firstname a-db id)
   (query-value
    a-db
    "SELECT firstname FROM students WHERE id = ?"
    id)
   " "
   ;(student-lastname a-db id)
   (query-value
    a-db
    "SELECT lastname FROM students WHERE id = ?"
    id)))

;returns #t/#f
;TODO: does it??? clean it up
(define (student-owns-locker? a-db id)
  (define (exn-handler exn)
    #f) ;if query null, return #f
  (with-handlers ([exn? exn-handler])
    (if (query-value
         a-db
         "SELECT 1 FROM student_locker WHERE student_id = ?"
         id)
        #t
        #f
        )))

(define (locker-owned? a-db id)
  (define (exn-handler exn)
    #f)
  (with-handlers ([exn? exn-handler])
    (if (query-value
         a-db
         "SELECT 1 FROM student_locker WHERE locker_id = ?"
         id)
        #t
        #f
        )))

(define (insert-student a-db id firstname lastname program email)
  (query-exec
   a-db
   "INSERT INTO students (id, firstname, lastname, program, email) VALUES (?, ?, ?, ? ,?)"  
   id firstname lastname program email))

;students - listof student-id
;returns - a list of available lockers for each student
(define (mass-assign-get-lockers a-db students) ;just returns a list of locker ids
  (take (query-list
         a-db
         "SELECT id FROM lockers as locker1 WHERE NOT EXISTS (SELECT * FROM student_locker AS locker2 WHERE locker2.[locker_id] = locker1.[id])")
        (length students)))

(define (mass-assign a-db students lockers)
  (map (λ (student locker) (insert-student-locker a-db student locker "mass-assigned")) students lockers))

(define (insert-locker a-db id location lock-id is-broken)
  (query-exec
   a-db
   "INSERT INTO lockers (id, location, lock_id, is_broken) VALUES (?, ?, ?, ?)"
   id location lock-id is-broken))

(define (insert-student-locker a-db student-id locker-id valid-until)
  (query-exec
   a-db
   "INSERT INTO student_locker (student_id, locker_id, valid_until) VALUES (?, ?, ?)"
   student-id locker-id valid-until))

(define (students-locker-id a-db id) ;student-id -> locker-id
  (query-value
   a-db
   "SELECT id FROM lockers WHERE id =
 (SELECT locker_id FROM student_locker WHERE student_id = ?)"
   id))

(define (clear-locker! a-db id)
  (query-exec
   a-db
   "DELETE FROM lockers WHERE id = ?"
   id))

(define (clear-student! a-db id)
  (query-exec
   a-db
   "DELETE FROM students WHERE id = ?"
   id))

(provide (all-defined-out))

