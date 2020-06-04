#lang racket
 
(require web-server/servlet)
(require web-server/formlets)
(provide/contract (start (request? . -> . response?)))

(require "model-2.rkt")

;-----== handlers ==------
(define (start request)
  (render-login-page
   (init-db!
    (build-path (current-directory)
                "database8.db"))
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
                                 (form ([action ,(embed/url login-handler)])                                   
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
                                 (p "Admin name")
                                 (h2 "Actions:")
                            
                                 (form ([action ,(embed/url view-lockers-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue "][type "submit"] [value "View Lockers"])))
                                 (form ([action ,(embed/url view-students-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "View Students"])))                                 
                                 (form ([action ,(embed/url upload-lockers-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "Import Lockers"])))
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled")(type "submit")(value "Export Lockers"))) (br) ;placeholders
                                 (form ([action ,(embed/url upload-students-handler)])
                                       (input ([class "w3-block w3-btn w3-ripple w3-blue"][type "submit"] [value "Import Students"])))
                                 (input ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled")(type "submit")(value "Export Students"))) (br) ;placeholders
                                 )
                            (div ((class "w3-twothird w3-card-4"))
                                 (h2 "Select an option")
                            
                                 )))))))

  (define (view-lockers-handler request)
    (render-view-lockers a-db request))

  (define (view-students-handler request)
    (render-view-students a-db request))

  (define (upload-lockers-handler request)
    (render-upload-lockers a-db request))

  (define (upload-students-handler request)
    (render-upload-students a-db request))
  
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
                                 (p "Admin name")                       
                                 
                                 (form ([action ,(embed/url dashboard-handler)])
                                       (input ([type "submit"][value "Back"])))
                                 (button ([form "delete"][type "submit"][name "id"])"Delete selected lockers!"))

                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Lockers:")
                                 (form ([id "delete"][action ,(embed/url clear-db-handler)])                                       
                                       ,(render-lockers a-db embed/url)))))))))


  (define (view-locker-handler request)
    (render-view-lockers a-db request))
  
  (define (clear-db-handler request) ;unused here
    (define lockers (map cdr (request-bindings request)))
    ;(print lockers)
    (map (λ (locker) (clear-locker! a-db locker)) lockers)
    (render-view-lockers a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))
  
  (define (view-locker-details-handler request)
    (render-locker-details a-db request))
  
  

  (define (render-lockers a-db embed/url)   
    `(div ((class "lockers"))
          (table (th "select all") (th "Locker number") (th "Location") (th "Details")
                 ,@(map (λ (locker)(render-locker-row locker embed/url)) (all-lockers a-db)))))

  (define (render-locker-row an-id embed/url)
    (define (id-value) (number->string an-id))
    `(tr
      (td (input ([type "checkbox"][name "id"][value ,(id-value)]))) ;TODO: fix checkbox
      (td ,(id-value))
      (td ,(locker-location a-db an-id))
      (td (form ([action ,(embed/url view-locker-details-handler)])
                (button ([type "submit"][name "details-id"][value ,(id-value)]) "Details")
                ))))


  (send/suspend/dispatch response-generator))

;View Students
(define (render-view-students a-db request)
  
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (p "Admin name")
                                 
                                 (form ([action ,(embed/url dashboard-handler)])
                                       (input ([type "submit"][value "Back"])))
                                 (button ([form "assign"][type "submit"][name "id"])"Mass assign lockers to selected students"))

                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Students:")
                                 (form ([id "assign"][action ,(embed/url mass-assign-handler)])
                                       ,(render-students a-db embed/url)))))))))

  ;placeholder
  (define (temp-handler request)
    (print "Handled!")
    (render-view-students a-db (redirect/get)))

  (define (mass-assign-handler request)
    (define students (map cdr (request-bindings request)))
    (print students)
    ;TODO: take user to separate confirm-assign page, list out locker-student pairs
    (mass-assign a-db students)
    (render-view-students a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))

  (define (view-student-details-handler request)
    (render-student-details a-db request))

  (define (render-students a-db embed/url)   
    `(div ((class "students"))
          (table (th "select all") (th "First Name") (th "Last Name") (th "Program") (th "Email")
                 ,@(map (λ (student)(render-student-row student embed/url)) (all-students a-db)))))


  (define is-checked? false)
  (define (render-student-row an-id embed/url)
    (define (id-value) (number->string an-id))
    `(tr
      ;(td (input ,(formlet-display (checkbox (id-value) is-checked?)))) DIDN'T WORK
      (td (input ([type "checkbox"][id ,(id-value)][name "id"][value ,(id-value)])))
      ;(td ,(id-value))
      (td ,(student-firstname a-db an-id))
      (td ,(student-lastname a-db an-id))
      (td ,(student-program a-db an-id))
      (td (form ([action ,(embed/url view-student-details-handler)])
                (button ([type "submit"][name "details-id"][value ,(id-value)]) "Details")))))

  (send/suspend/dispatch response-generator))

(define (confirm-mass-assign a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Confirm locker assignment")
                                
                                 (form ([action ,(embed/url temp-handler)])
                                       (input ([type "submit"][value "Confirm"])))
                                 (form ([action ,(embed/url temp-handler)])
                                       (input ([type "submit"][value "Cancel"]))))
                            
                            (div ((class "w3-twothird w3-card-4"))                                 
                                
                                 )))))))

  (define (temp-handler request)
    (print "handled!!!"))
  
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
                                 (p "Admin name")
                                 (h2 "Actions:")
                                 (form ([action ,(embed/url view-lockers-handler)])
                                       (input ([type "submit"][value "Back"]))))
                            
                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Locker Details:")

                                 ,(display-locker-info (extract-binding/single 'details-id (request-bindings request)))
                                
                                 )))))))

  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))

  (define (display-locker-info id)
    `(div ((class "w3-white"))
          (h2 "Locker ID:")
          (h3 ,id)
          (h3 "Locker Location:") ,(locker-location a-db id)
          (h3 "Locker owner:")
          (p ,(locker-owner a-db id))
          (h3 "Notes:")))

  (send/suspend/dispatch response-generator))

;Student details
(define (render-student-details a-db request)
  (define details-id (extract-binding/single 'details-id (request-bindings request)))
  
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (p "Admin name")
                                 (h2 "Actions:")
                                 (form ([action ,(embed/url view-students-handler)])
                                       (input ([type "submit"][value "Back"]))))
                            
                            
                            (div ((class "w3-twothird w3-card-4"))
                                 (h1 "Student Details:")

                                 ,(display-student-info details-id embed/url)
                                
                                 )))))))

  (define (view-students-handler request)
    (render-view-students a-db (redirect/get)))

  (define (add-student-locker-handler request)
    (confirm-add-student-locker a-db request))
  
  (define (display-student-info id embed/url)
    (define owns
      (if (student-owns-locker? a-db id)
          "Yeah!"
          "Nope!"))
    
    
    `(div ((class "w3-white"))
          (h2 "Student ID:")
          (h3 ,id)
          (h3 "Student Name:") ,(student-name a-db id)
          (h3 "Owns locker?")
          (p ,(if (student-owns-locker? a-db id)
                  "Yeah!"
                  "Nope!"))
          ;TODO: make button disabled if student-owns-locker? returns false
          (form ([action ,(embed/url add-student-locker-handler)])
                (input ([type "text"][id "locker-id"][name "locker-id"]))
                (input ([type "hidden"][id "student-id"][name "student-id"][value ,id]))
                (input ([type "submit"][value "Add locker"])))
          
          
          (h3 "Notes:")))

  (send/suspend/dispatch response-generator))

;Confirm new student-locker
(define (confirm-add-student-locker a-db request)
  
  (define locker-id (extract-binding/single 'locker-id (request-bindings request)))
  (define student-id (extract-binding/single 'student-id (request-bindings request)))
  
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Confirm new locker assignment?")
                                 (p ,locker-id) ;also make it display student name. ;)                  
                                 (form ([action ,(embed/url confirm-handler)])
                                       (input ([type "hidden"][id "details-id"][name "details-id"][value ,student-id]))
                                       (input ([type "submit"][value "Confirm"])))
                                 (form ([action ,(embed/url cancel-handler)])
                                       (input ([type "submit"][value "Cancel"]))))))))))
 
  (define (confirm-handler request)
    ;extract bindings here and exec sql
    ;TODO: reconstruct student id back into post request
    ;so student detail page can be rendered
    (insert-student-locker a-db student-id locker-id "until tuesday")
    (render-student-details a-db request))

  (define (cancel-handler request)
    ;TODO: reconstruct student id back into post request
    ;so student detail page can be rendered
    (render-view-students a-db (redirect/get)))
  
  (send/suspend/dispatch response-generator))
  
;Upload lockers
(define (render-upload-lockers a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (p "Admin name")
                                 (h2 "Actions:")
                                 (h3 "Upload locker CSV file:")
                                 (form 
                                  ([action ,(embed/url import-csv-handler)]
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  ,@(formlet-display file-upload-formlet)
                                  (input ([type "submit"] [value "Upload"])))
                                 
                                 (form ([action ,(embed/url dashboard-handler)])
                                       
                                       (input ([type "submit"][value "Back"])))
                                 (form ([action ,(embed/url view-lockers-handler)])
                                       (input ([type "submit"][value "View Lockers"]))))))))))


  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))

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
  
  
  (send/suspend/dispatch response-generator))

;Upload students
(define (render-upload-students a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body ((class "w3-container"))
                  (div ((class "w3-content w3-margin-top"))
                       (div ((class "w3-row-padding"))
                            (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                                 (h2 "Welcome! Logged in as:")
                                 (p "Admin name")
                                 (h2 "Actions:")
                                 (h3 "Upload student CSV file:")
                                 (form 
                                  ([action ,(embed/url import-csv-handler)]
                                   [method "POST"]
                                   [enctype "multipart/form-data"])
                                  ,@(formlet-display file-upload-formlet)
                                  (input ([type "submit"] [value "Upload"])))
                                 
                                 (form ([action ,(embed/url dashboard-handler)])
                                       (input ([type "submit"][value "Back"])))
                                 (form ([action ,(embed/url view-students-handler)])
                                       (input ([type "submit"][value "View students"]))))))))))


  (define (view-students-handler request)
    (render-view-students a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))

  (define (import-csv-handler request)
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname));Maybe add timestamp?
    (display-to-file fcontents save-name #:exists 'replace);Limit the number of saved local files?
    (db-import-student-csv! a-db (open-input-file (build-path (current-directory) save-name)))
    (render-upload-successful-page a-db (redirect/get)))
  
  
  (send/suspend/dispatch response-generator))


;Upload success page
(define (render-upload-successful-page a-db request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Locker Management System")
                  ,@(style-link))
            (body
             (p "File upload successful!")             
             (form ([action ,(embed/url dashboard-handler)])
                   (input ([type "submit"][value "Back to Dashboard"])))))))

  (define (dashboard-handler request)
    (render-admin-dashboard a-db (redirect/get)))
  
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


