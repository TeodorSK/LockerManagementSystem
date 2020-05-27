#lang racket
 
(require web-server/servlet)
(require web-server/formlets)
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
             (h1 "Lockers:")
              (a ((href ,(embed/url clear-db-handler))) "Clear Lockers from DB")
              (form 
               ([action ,(embed/url import-csv-handler)]
                [method "POST"]
                [enctype "multipart/form-data"])
               ,@(formlet-display file-upload-formlet)
               (input ([type "submit"] [value "Upload"])))
              
              ,(render-lockers a-db embed/url)))))

  (define (clear-db-handler request)
    (clear-db! a-db)
    (render-home-page a-db (redirect/get)))
  
  (define (import-csv-handler request)
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname));Maybe add timestamp?
    (display-to-file fcontents save-name #:exists 'replace);Limit the number of saved local files?
    (db-import-csv! a-db (open-input-file (build-path (current-directory) save-name)))
    (render-upload-successful-page a-db (redirect/get)))

  (send/suspend/dispatch response-generator))


;Upload success page
(define (render-upload-successful-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
    `(html (head (title "Locker Management System")
                  (link ((rel "stylesheet")
                       (href "/blog.css")
                       (type "text/css"))))
            (body
             (p "File upload successful!")
             (a ((href ,(embed/url back-to-homepage-handler))) "Back to homepage")))))

  (define (back-to-homepage-handler request)
    (render-home-page a-db (redirect/get)))

  
  (send/suspend/dispatch response-generator))

;(define file-upload-formlet
;    (formlet
;     (div ,{(file-upload) . => . binds})
;     (define
;       inputfile
;       [fcontents (binding:file-content binds)]
;       (values fname fcontents))))

(define file-upload-formlet
  (formlet
   (div ,{(file-upload) . => . binds})
   ; (formlet-process file-upload-formlet request)
   ; returns the file name and contents:
   (let
       ([fname (bytes->string/utf-8 (binding:file-filename binds))]
        [fcontents (binding:file-content binds)])
     (values fname fcontents))))


(define (render-lockers a-db embed/url)
  `(div ((class "lockers"))
        (table (th "Locker number") (th "Location")
        ,@(map render-locker (db-lockers a-db)))))


(define (render-locker a-locker)
  `(tr (td ,(number->string (locker-id a-locker)))
       (td ,(my-locker-location a-locker))))


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


            