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
                "database7.db"))
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
                            
                                 (form ([action ,(embed/url view-lockers-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "View Lockers"])))
                                 (form ([action ,(embed/url view-students-handler)])
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

  (define (view-lockers-handler request)
    (render-view-lockers a-db request))

  (define (view-students-handler request)
    (render-view-students a-db request))
  
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
                                  (input ([type "submit"] [value "Upload"])))
                                 (form ([action ,(embed/url dashboard-handler)])
                                       (input ((type "submit")(value "Back")))))

                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Lockers:")
                                 (form
                                  ([action ,(embed/url clear-db-handler)]
                                   [method "POST"]
                                   (name "id"))
                                  (input ([type "submit"] [value "Clear Selected Lockers from DB"])) ; Replace with formlet
                                  ,(render-lockers a-db embed/url)))))))))


  (define (view-locker-handler request)
    (render-view-lockers a-db request))
  
  (define (clear-db-handler request)
    ;(print (map cdr (request-bindings request)))
    (clear-selected-lockers! a-db (map cdr (request-bindings request)))
    (render-view-lockers a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))
  
  (define (view-locker-details-handler request)
    (render-locker-details a-db request))
  
  (define (import-csv-handler request)
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname));Maybe add timestamp?
    (display-to-file fcontents save-name #:exists 'replace);Limit the number of saved local files?
    (db-import-locker-csv! a-db (open-input-file (build-path (current-directory) save-name)))
    (render-upload-successful-page a-db (redirect/get)))

  (define (render-lockers a-db embed/url)   
    `(div ((class "lockers"))
          (table (th "select all") (th "Locker number") (th "Location") (th "Details")
                 ,@(map (λ (locker)(render-locker-row locker embed/url)) (db-lockers a-db)))))

  (define (render-locker-row a-locker embed/url)
    (define (id-value) (number->string (locker-id a-locker)))
    `(tr
      (td (input ((type "checkbox")(name "id")(value ,(id-value))))) ;TODO: fix checkbox
      (td ,(id-value))
      (td ,(locker-location a-locker))
      (td (form ([action ,(embed/url view-locker-details-handler)])
                (button ((type "submit")(name "details-id")(value ,(id-value))) "Details")
                ))))


  (send/suspend/dispatch response-generator))

;View Students
(define (render-view-students a-db request)

  ;TODO: import students  or make file upload system
  ;(db-import-student-csv! a-db (open-input-file (build-path (current-directory) "students.csv")))
  
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
                                  ([action ,(embed/url import-csv-handler)] ; handler fix
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  ,@(formlet-display file-upload-formlet)
                                  (input ([type "submit"] [value "Upload"])))
                                 (form ([action ,(embed/url dashboard-handler)])
                                       (input ((type "submit")(value "Back")))))

                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Students:")
                                 (form
                                  ([action ,(embed/url temp-handler)] ;handler fix
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  (input ([type "submit"] [value "Assign lockers to selected students"]))
                                  ,(render-students a-db embed/url)))))))))

  (define (temp-handler request)
    (render-view-students a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))

  (define (import-csv-handler request)
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname));Maybe add timestamp?
    (display-to-file fcontents save-name #:exists 'replace);Limit the number of saved local files?
    (db-import-locker-csv! a-db (open-input-file (build-path (current-directory) save-name)))
    (render-upload-successful-page a-db (redirect/get)))

  (define (render-students a-db embed/url)   
    `(div ((class "students"))
          (table (th "select all") (th "First Name") (th "Last Name") (th "Program") (th "Email")
                 ,@(map (λ (student)(render-student-row student embed/url)) (db-students a-db)))))

  (define (render-student-row a-student embed/url)
    (define (id-value) (number->string (student-id a-student)))
    `(tr
      (td (input ((type "checkbox")(id ,(id-value))(value ,(id-value))))) ;TODO: fix checkbox
      ;(td ,(id-value))
      (td ,(student-firstname a-student))
      (td ,(student-lastname a-student))
      l;(td ,(student-program a-student))
      (td ,(student-email a-student))
      (td (form ([action ,(embed/url temp-handler)])
                (input ((type "submit")(id ,(id-value))(name ,(id-value))(value "Details")))))))

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

                                ,(display-locker-info (extract-binding/single 'details-id (request-bindings request)))
                                
                                 )))))))

  (define (display-locker-info id)
    `(div ((class "w3-white"))
          (h2 "Locker ID:")
          (h3 ,id)
          (h3 "Locker Location:") ,(temp-locker-location a-db id)
          (h3 "Locker owner:")
          (p ,(locker-owner a-db id))
          (h3 "Notes:")))

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
               #:launch-browser? #t
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list (build-path "/c/Users/Teodor/Documents/Lockers/LockerManagementSystem/test_project/" "htdocs"))
               #:servlet-path
               "/servlets/webapp.rkt")


