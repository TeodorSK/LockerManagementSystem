#lang racket
 
(require web-server/servlet)
(require web-server/formlets)
(provide/contract (start (request? . -> . response?)))

(require "model-2.rkt")

;-----== handlers ==------
(define (start request)
  (render-admin-dashboard
   (init-db!
    (build-path (current-directory)
                "database4.db"))
   request))

(define (render-login-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))

                            (div ((class "w3-card-4 w3-white w3-center"))
                                 (h1 "Ryerson Locker Management System")
                                 (h2 "Description")
                                 (form 
                                  ([action ,(embed/url login-handler)]
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  ,@(formlet-display login-formlet)
                                  (input ([type "submit"] [value "Log in"]))))))))))

  (define (login-handler request) ;todo admin student split
    (render-admin-dashboard a-db request))
  
  (send/suspend/dispatch response-generator))

;Admin dashboard
(define (render-admin-dashboard a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (h3 "Admin name")
                                 (h2 "Actions:")
                            
                                 (form ([action ,(embed/url view-locker-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "View Lockers"])))
                                 (form ([action ,(embed/url view-locker-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "Students"])))
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue")(type "submit")(value "View Students"))) (br)
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue")(type "submit")(value "Import Lockers"))) (br)
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue")(type "submit")(value "Export Lockers"))) (br)
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue")(type "submit")(value "Import Lockers"))) (br)
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue")(type "submit")(value "Export Lockers"))) (br)
                                 )
                            (div ((class "w3-twothird w3-card-4"))
                                 (h2 "Locker details:")
                            
                                 )))))))

  (define (view-locker-handler request)
    (render-view-lockers a-db request))
  
  (send/suspend/dispatch response-generator))

;View Lockers
(define (render-view-lockers a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (h3 "Admin name")
                                 (h2 "Actions:")                           
                                 (h3 "Upload locker CSV file:")
                                 (form 
                                  ([action ,(embed/url import-csv-handler)]
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  ,@(formlet-display file-upload-formlet)
                                  (input ([type "submit"] [value "Upload"]))))

                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Lockers:")
                                 (form
                                  ([action ,(embed/url clear-db-handler)]
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  (input ([type "submit"] [value "Clear Selected Lockers from DB"])) ; Replace with formlet
                                  ,(render-lockers a-db embed/url)))))))))


  (define (view-locker-handler request)
    (render-view-lockers a-db request))
  
  (define (render-lockers a-db embed/url)   
    `(div ((class "lockers"))
          (table (th "select all") (th "Locker number") (th "Location") (th "Details")
                 ,@(map (λ (locker)(render-locker-row locker embed/url)) (db-lockers a-db)))))

  (define (render-locker-row a-locker embed/url)
    (define (id-value) (number->string (locker-id a-locker)))
    `(tr
      (td (input ((type "checkbox")(id ,(id-value))(value ,(id-value))))) ;TODO: fix checkbox
      (td ,(id-value))
      (td ,(my-locker-location a-locker))
      (td (form ([action ,(embed/url view-locker-handler)])
                (input ((type "submit")(id ,(id-value))(name ,(id-value))(value "Details")))))))

  (define (clear-db-handler request)
    (clear-selected-lockers! a-db (list (request-bindings request)))
    (render-view-lockers a-db (redirect/get)))
  
  (define (import-csv-handler request)
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname));Maybe add timestamp?
    (display-to-file fcontents save-name #:exists 'replace);Limit the number of saved local files?
    (db-import-csv! a-db (open-input-file (build-path (current-directory) save-name)))
    (render-upload-successful-page a-db (redirect/get)))

  (send/suspend/dispatch response-generator))

;Locker details
(define (render-locker-details a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (h3 "Admin name")
                                 (h2 "Actions:"))
                            
                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Locker Details:")
                                 ;Perform SQL query based on bindings and desplay details
                                 )))))))

  (send/suspend/dispatch response-generator))
  

;Upload success page
(define (render-upload-successful-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body
             (p "File upload successful!")
             (a ((href ,(embed/url back-to-homepage-handler))) "Back to homepage")))))

  (define (back-to-homepage-handler request)
    (render-view-lockers a-db (redirect/get)))
  
  (send/suspend/dispatch response-generator))

(define (style-link)
  `((link ((rel "stylesheet")
           (href "https://www.w3schools.com/w3css/4/w3.css")))))

(define login-formlet
  (formlet
   ,(=> (radio-group (list "Admin" "Student"))
                     usertype)
        usertype))

(define file-upload-formlet
  (formlet
   (div ,{(file-upload) . => . binds})
   ; (formlet-process file-upload-formlet request)
   (let
       ([fname (bytes->string/utf-8 (binding:file-filename binds))]
        [fcontents (binding:file-content binds)])
     (values fname fcontents))))


(require web-server/servlet-env)
(serve/servlet start
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list (build-path "/c/Users/Teodor/Documents/Lockers/LockerManagementSystem/test_project/" "htdocs"))
               #:servlet-path
               "/servlets/webapp.rkt")


