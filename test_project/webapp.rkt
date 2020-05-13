#lang racket
 
(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))

(require "model-2.rkt")

;-----== handlers ==------
(define (start request)
  (render-home-page
   (init-db!
    (build-path (current-directory)
                "database4.db"))
   request))

;Home page
;render-home-page: request -> void
(define (render-home-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  (link ((rel "stylesheet")
                       (href "/blog.css")
                       (type "text/css"))))
            (body
             (h1 "CURRENT DIR:")
             (path->str current-directory)
             (h1 "Lockers:")
             ,(render-lockers a-db embed/url))
             (form ((action
                     ,(embed/url import-csv-handler)))
                   (input ((type "submit")))))))
             
  (define (import-csv-handler request)
    (db-import-csv! a-db (open-input-file (build-path (current-directory) "lockers.csv")))
    (render-home-page a-db (redirect/get)))

  (send/suspend/dispatch response-generator))


(define (render-lockers a-db embed/url)
  `(div ((class "lockers"))
        ,@(map render-locker (db-lockers a-db))))


(define (render-locker a-locker)
  `(div ((class "locker"))
     ,(number->string (locker-id a-locker))
     ,(my-locker-location a-locker)))


(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))

;(static-files-path "htdocs")

(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list (build-path "/Users/tsk/Documents/Work/Lockers/test_project" "htdocs"))
               #:servlet-path
               "/servlets/webapp.rkt")


            