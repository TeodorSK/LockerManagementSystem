#lang racket/base
(require db)
(require csv-reading)
(require racket/list)

;-----== structs ==------
(struct post (id title body comments) #:mutable #:prefab)
(struct DB (home posts) #:mutable #:prefab)

;-----== definitions ==------
(define first-locker (post 1 "firsttitle" "bodyey" (list)))

;-----== aux functions ==------

(define (post-insert-comment! a-db a-post new-comment)
  (query-exec
   a-db
   "INSERT INTO comments (pid, content) VALUES (?, ?)"
   (post-id a-post)
   new-comment))
;  (set-post-comments! a-post (append (list new-comment) (post-comments a-post))))

(define (db-insert-post! a-db new-post)
  (query-exec
   a-db
    "INSERT INTO posts (title, body) VALUES (?, ?)"
    (post-title new-post)
    (post-body new-post)))

;  (set-DB-posts! a-db (cons new-post
;                                (DB-posts a-db))))

;In the tutorial, they use
;(struct blog (db))
;to encapsulate a database. we don't need that

;We do, since 
(define (initialize-db! home)
  (define the-DB (sqlite3-connect #:database home #:mode 'create))
  (unless (table-exists? the-DB "posts")
    (query-exec the-DB
       (string-append
        "CREATE TABLE posts "
        "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)"))
    (db-insert-post!
     the-DB
     (post 2 "locker1#" "location1" (list))))
  (unless (table-exists? the-DB "comments")
    (query-exec the-DB
      "CREATE TABLE comments (pid INTEGER, content TEXT)")
    (post-insert-comment!
      the-DB (first (DB-posts the-DB)) "firstcomment"))
  the-DB)

;TODO: finish database
;  (define (log-missing-exn-handler exn)
;    (DB
;     (path->string home)
;     (list (post "Post 1" "Body 1" (list "heyy!!!" "whaaat???"))
;                         (post "Post 2" "Body 2" (list)))))
;  (define the-db
;    (with-handlers ([exn? log-missing-exn-handler])
;      (with-input-from-file home read)))
;  (set-DB-home! the-db (path->string home))
;  the-db)

(define (save-db! joney)
  (define (write-to-db)
    (write joney))
  (with-output-to-file (DB-home joney)
    write-to-db
    #:exists 'replace))


(provide (all-defined-out))