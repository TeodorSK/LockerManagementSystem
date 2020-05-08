#lang web-server/insta
;-----== structs ==------
(struct post (title body comments) #:mutable)
(struct blog (posts) #:mutable)

;-----== definitions ==------
(define first-locker (post "firsttitle" "bodyey" empty))
(define BLOG (blog (list (post "Post 1" "Body 1" (list "heyy!!!" "whaaat???"))
                         (post "Post 2" "Body 2" empty))))


;-----== handlers ==------
(define (start request)
  (render-home-page request))

;Home page
;render-home-page: request -> void
(define (render-home-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System"))
            (body
             (h1 "It's the blog")
             ,(render-posts embed/url)

             (form ((action
                     ,(embed/url insert-post-handler)))
                   (input  ((type "text") (name "title")))
                   (input  ((type "text") (name "body")))
                   (input ((type "submit"))))))))
  (define (insert-post-handler request)
    (blog-insert-post! BLOG (parse-post (request-bindings request)))
    (render-home-page request))
  (send/suspend/dispatch response-generator))

;Post detail page
;render-post-detail-page: request -> void
(define (render-post-detail-page a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System"))
            (body
             ,(render-post-details a-post)
             
             (form ((action
                     ,(embed/url insert-comment-handler)))
                   (input  ((type "text") (name "comment")))
                   (input ((type "submit"))))
             (form ((action
                     ,(embed/url render-home-page)))
                   (button ((type "submit")) "Back"))))))

  (define (insert-comment-handler a-request)
    (post-insert-comment! a-post (parse-comment (request-bindings a-request)))
    (render-post-detail-page a-post a-request))
  
  (send/suspend/dispatch response-generator))
  

;-----== aux functions ==------
(define (post-insert-comment! a-post new-comment)
  (set-post-comments! a-post (cons new-comment
                                   (post-comments a-post))))

(define (blog-insert-post! a-blog new-post)
  (set-blog-posts! a-blog (cons new-post
                                (blog-posts a-blog))))

(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

;parse user submitted post with empty comment list
(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)
        (list)))

(define (parse-comment bindings)
    (extract-binding/single 'comment bindings))

(define (render-posts embed/url)
  (define (render-post/embed/url a-post)
    (render-post a-post embed/url))
  `(div ((class "posts"))
        ,@(map render-post/embed/url (blog-posts BLOG))))

;this renders post details
(define (render-post-details a-post)
  `(div ((class "post"))
        (h1 "Post details:")
        (h2 ,(post-title a-post))
        (p ,(post-body a-post))
        ,(render-as-itemized-list (post-comments a-post))
        ))


;this renders a post in the home page
(define (render-post a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-post request))
  `(div ((class "post"))
        (a ((href ,(embed/url view-post-handler)))
           ,(post-title a-post))
        ;add first few lines of post
        ;add # of comments 
        ))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))