#lang racket/base
(require db)
(require racket/list)
(require csv-reading)



;-----== structs ==------
(struct locker
  (id
   location
   [lockid #:mutable]
   db))
;need reference to db?

(struct student
  (name))

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
  db)

(define (db-import-csv! a-db file)
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

;(open-input-file "htdocs/lockers.csv")

(define (extract-locker-data-from-row a-db a-row)
  (define locker-num (list-ref a-row 1))
  (define locker-location (list-ref a-row 3))
  (if (and
       (string->number locker-num) ;check if unique      
       (string? locker-location))
      (insert-locker a-db (string->number locker-num) locker-location 0)
      ;(list locker-num locker-location)
      "incorrect format (not unique or not int, str)"))
  

(define (db-lockers a-db)
  (define (id->locker an-id)
    (locker an-id "nowhere" 0 a-db))
  (map id->locker
       (query-list a-db "SELECT id FROM lockers")))


(define (my-locker-location a-locker)
  (query-value
   (locker-db a-locker)
   "SELECT location FROM lockers WHERE id = ?"
   (locker-id a-locker)))

(define (insert-locker a-db id location lockid)
  (query-exec
   a-db
   "INSERT INTO lockers (id, location, lockid) VALUES (?, ?, ?)"
   id location lockid))

(define (clear-db! a-db)
  (query-exec
   a-db
   "DELETE FROM lockers"))


(provide (all-defined-out))

