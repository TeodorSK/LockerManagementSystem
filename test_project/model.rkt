#lang racket/base


;-----== structs ==------
(struct post (title body comments) #:mutable)
(struct blog (posts) #:mutable)

;-----== definitions ==------
(define first-locker (post "firsttitle" "bodyey" (list)))
(define BLOG (blog (list (post "Post 1" "Body 1" (list "heyy!!!" "whaaat???"))
                         (post "Post 2" "Body 2" (list)))))


;-----== aux functions ==------
(define (post-insert-comment! a-post new-comment)
  (set-post-comments! a-post (append (list new-comment) (post-comments a-post))))

(define (blog-insert-post! a-blog new-post)
  (set-blog-posts! a-blog (cons new-post
                                (blog-posts a-blog))))


(provide (all-defined-out))