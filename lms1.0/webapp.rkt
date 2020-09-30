#lang racket
(require web-server/servlet)
(require web-server/formlets)
(require web-server/stuffers/gzip)
(require web-server/safety-limits)
(require web-server/managers/timeouts)
(require string-util)
(require web-server/servlet-env)
(require racket/runtime-path)
(require net/smtp)
(require net/head)

(require "model.rkt")
(require "student.rkt")

(define interface-version 'stateless)
(provide interface-version gzip-stuffer start)

(define-runtime-path files-path "htdocs")

(define admin-firstname "None")

(define (start request) (start-page request))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Start=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (start-page request)    
  (define activeclasses (string-split (extract-binding/single 'cas-activeclasses (request-headers request)) ","))  
  (set! admin-firstname (extract-binding/single 'cas-firstname (request-headers request))) 
  (if (or (member "instructor" activeclasses) (member "staff" activeclasses))      
      (if (is-admin? (extract-binding/single 'cas-employeenumber (request-headers request)) (open-input-file (build-path files-path "auth_admins")))
          (student-start request)                
          (admin-unauth-page request))            
       (student-start request)))
  

(define (admin-unauth-page request)
  (response/xexpr
   `(html (p "Error: You are trying to access the admin dashboard, but you are not an authorized administrator. If you believe this is an error please contact tsandelkonjevic [at] ryerson [dot] ca"))))
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Admin dashboard=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-admin-dashboard request [a-db (init-db! (build-path files-path "database.db"))]) 
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, " ,admin-firstname))
                ;userinfo.rkt test

                (h4 "View")                                 
                ,(nav-button (embed/url view-lockers-handler) "View Lockers")                
                ,(nav-button (embed/url view-students-handler) "View Students")
                (p ((style "border-top: 3px dashed #bbb")))
                (h4 "Import")                
                ,(nav-button (embed/url upload-lockers-handler) "Import Lockers")
                ,(nav-button (embed/url upload-students-handler) "Import Students")                
                (p ((style "border-top: 3px dashed #bbb")))
                (h4 "Other")
                (input ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled")(type "submit")(value "Export Lock sheet")))
                (br)
                )
           (div ((class "w3-twothird w3-card-4"))
                (h3 "Select an option from the menu")
                            
                ))))

  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (view-lockers-handler request)
    (render-view-lockers a-db request))

  (define (view-students-handler request)
    (render-view-students a-db request))

  (define (upload-lockers-handler request)
    (render-upload-lockers a-db request))

  (define (upload-students-handler request)
    (render-upload-students a-db request))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-View Lockers=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-view-lockers a-db request)

  (define locker-id (extract-bindings-safely 'locker-id "%" request))
  (define location (extract-bindings-safely 'location "" request))
  (define assigned (extract-bindings-safely 'assigned "" request))
  (define unassigned (extract-bindings-safely 'unassigned "" request))  
  (define broken (extract-bindings-safely 'broken "" request))
  (define fixed (extract-bindings-safely 'fixed "" request))

  (define filtered? (extract-bindings-safely 'filtered #f request)) ;only #t if user filters, #f if just navigating to page
  
  (define filtered-lockers (filter-lockers a-db locker-id location assigned unassigned broken fixed))
  (define locker-count (length filtered-lockers))  

  (define all-checked (extract-bindings-safely 'all-checked #f request))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, " ,admin-firstname))

                (h3 "Filters:")
                (form ([id "filters"][action ,(embed/url filter-handler)][method "GET"])                      
                      (input ([style "width: 100%"][type "text"][name "locker-id"][id "locker-id"][placeholder "Locker ID"])) (br)(br)                      
                      (input ([style "width: 100%"][type "text"][name "location"][id "location"][placeholder "Location"])) (br) (br)                                                                                       
                      ,(checkmark "assigned" "assigned" "Assigned" (if (and (null-string? assigned) filtered?) #f #t))                      
                      ,(checkmark "unassigned" "unassigned" "Unassigned" (if (and (null-string? unassigned) filtered?) #f #t))
                      ,(checkmark "broken" "broken" "Broken" (if (and (null-string? broken) filtered?) #f #t))                      
                      ,(checkmark "fixed" "fixed" "Fixed" (if (and (null-string? fixed) filtered?) #f #t))
                      (input ([type "hidden"][name "filtered"][value "yes"]))
                      (input ([class ,(button-style-class)][type "submit"][value "Filter"])))

                (p ((style "border-top: 3px dashed #bbb")))                
                ,(nav-button (embed/url assign-locks-handler) "Assign locks to lockers")
                (button ([class ,(button-style-class)][form "table"][type "submit"][name "work-order"])"Issue work orders") (br)                                              
                (button ([class ,(button-style-class)][form "table"][type "submit"][name "release"])"Release selected lockers")                
                (p ((style "border-top: 3px dashed #bbb")))                
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard")
                (br))           

           (div ((class "w3-twothird w3-card-4"))
                (table ((style "width:100%"))
                       (tr (td ([style "width: 30%"])(h1 "Lockers:"))
                           (td (span ([style "background-color:#86E660; height: 25px; width: 25px; border-radius: 50%; display: inline-block; "])) " - Assigned & Operational" (br)
                               (span ([style "background-color:#FFBA49; height: 25px; width: 25px; border-radius: 50%; display: inline-block; "])) " - Broken" (br)
                               (span ([style "background-color:#2196F3; height: 25px; width: 25px; border-radius: 50%; display: inline-block; "])) " - Available")                           
                           (td ([style "width: 30%"])(img ((style "max-width:40%;height:auto;float:right")(src "lockers.jpg"))))))

                ,(if (and (zero? locker-count) filtered?)                                             
                     `(p "No results found according to filter settings.")
                     `(p "Total results:" ,(if filtered? (number->string locker-count) (number->string (length (all-lockers a-db))))))
                (form ([id "table"][action ,(embed/url table-handler)])
                      ,(render-lockers a-db embed/url))))))

  (define (render-lockers a-db embed/url)   
    `(div ((class "lockers"))
          (table ([class "w3-table w3-striped w3-bordered w3-hoverable"])                                  
                 (th (button ([form "filters"][type "submit"][name "all-checked"][value ,(if all-checked "" "#t")])(img ([src ,(if all-checked "checked.png" "unchecked.png")]))))
                 (th "Locker ID")
                 (th "Location")
                 (th "Assigned to")
                 (th "Status")
                 (th "Details")
                 ,@(map (λ (locker)(render-locker-row locker embed/url)) (if filtered? filtered-lockers (all-lockers a-db))))))

  (define (render-locker-row an-id embed/url)
    (define id-value (number->string an-id))
    `(tr 
      (td ,(checkmark "id" id-value "" all-checked))
      (td ,id-value)
      (td ,(locker-location a-db an-id))
      (td ,(if (locker-assigned? a-db an-id) (locker-owner-name a-db an-id) "---"))
      (td (span ([style ,(string-append (cond [(locker-broken? a-db an-id) "background-color:#FFBA49;"]
                                              [(locker-assigned? a-db an-id) "background-color:#86E660;"]                                              
                                              [else "background-color:#2196F3;"])
                                        " height: 25px; width: 25px; border-radius: 50%; display: inline-block; ")])))      
      (td (button ([class ,(button-style-class)][form "table"][type "submit"][name "locker-details-id"][value ,id-value])"Details"))))

  ;=-=-Handlers-=-=
  (define (table-handler request)
    (cond ((exists-binding? 'release (request-bindings request))
           ((map (λ (locker) (release-student-locker a-db locker)) (extract-bindings 'id (request-bindings request)))
            (render-view-lockers a-db request)))
          ((exists-binding? 'locker-details-id (request-bindings request))
           (view-locker-details-handler request))
          ((exists-binding? 'work-order (request-bindings request))
           (send-email-page a-db request))
          (else (render-view-lockers a-db request))))
  
  (define (assign-locks-handler request)
    (assign-locks-page a-db request))

  (define (filter-handler request)    
    (render-view-lockers a-db request))
  
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))
  
  (define (view-locker-details-handler request)
    (render-locker-details a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-View Students=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-view-students a-db request)

  ;Filters
  (define student-id (extract-bindings-safely 'student-id "%" request))
  (define firstname (extract-bindings-safely 'firstname "%" request))
  (define lastname (extract-bindings-safely 'lastname "%" request))
  (define program (extract-bindings-safely 'program "%" request))
  (define email (extract-bindings-safely 'email "%" request))
  (define has-locker (extract-bindings-safely 'has-locker "" request))
  (define awaiting (extract-bindings-safely 'awaiting "" request))
  (define no-locker (extract-bindings-safely 'no-locker "" request))
  

  (define filtered? (extract-bindings-safely 'filtered #f request)) ;only #t if user filters, #f if just navigating to page

  (define filtered-students (filter-students a-db student-id firstname lastname program email has-locker awaiting no-locker))
  
  (define student-count (length filtered-students))

  (define all-checked (extract-bindings-safely 'all-checked #f request))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, " ,admin-firstname))


                (h3 "Filters:")
                (form ([id "filters"][action ,(embed/url filter-handler)][method "GET"])
                      
                      (input ([style "width: 100%"][type "text"][name "student-id"][id "student-id"][placeholder "Student ID"])) (br) (br)                      
                      (input ([style "width: 100%"][type "text"][name "firstname"][id "firstname"][placeholder "First Name"])) (br) (br)                      
                      (input ([style "width: 100%"][type "text"][name "lastname"][id "lastname"][placeholder "Last name"])) (br) (br)                      
                      (select ([style "max-width:100%"][name "program"][id "program"])                              
                              (option ([value ,(if (exists-binding? 'program (request-bindings request)) program "")])"Select Program")
                              ,@(map (λ (str) `(option ([value ,str]) ,str)) (all-programs a-db))) (br) (br)
                                                                                                   (input ([style "width: 100%"][type "text"][name "email"][id "email"][placeholder "Email"])) (br) (br)
                                                                                                   ,(checkmark "has-locker" "has-locker" "Has locker" (if (and (null-string? has-locker) filtered?) #f #t))                                                                                                   
                                                                                                   ,(checkmark "awaiting" "awaiting" "Awaiting locker" (if (and (null-string? awaiting) filtered?) #f #t))
                                                                                                   ,(checkmark "no-locker" "no-locker" "No locker" (if (and (null-string? no-locker) filtered?) #f #t))                      
                                                                                                   (input ([type "hidden"][name "filtered"][value "yes"]))
                                                                                                   (input ([class ,(button-style-class)][type "submit"][value "Filter"])))
                                 

                (p ((style "border-top: 3px dashed #bbb")))
                (button ([class ,(button-style-class)][form "table"][type "submit"][name "mass-assign"])"Mass assign lockers") (br)
                (button ([class ,(button-style-class)][form "table"][type "submit"][name "mass-email"])"Mass email") (br)
                (p ((style "border-top: 3px dashed #bbb")))                
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard")
                (br))

           (div ((class "w3-twothird w3-card-4"))
                (table ((style "width:100%"))
                       (tr (td ([style "width: 20%"])(h1 "Students:"))
                           (td ([style "width: 30%"])(span ([style "background-color:#86E660; height: 25px; width: 25px; border-radius: 50%; display: inline-block; "])) " - Has locker" (br)
                               (span ([style "background-color:#2196F3; height: 25px; width: 25px; border-radius: 50%; display: inline-block; "])) " - Awaiting")
                           (td ([style "width: 20%"]),(if (any-outstanding-requests? a-db) "❕ New requests awaiting" "✓ No new requests"))
                           (td ([style "width: 30%"])(img ((style "max-width:100%;height:auto;float:right")(src "students.jpg"))))))
                ,(if (and (zero? student-count) filtered?)                                             
                     `(p "No results found according to filter settings.")
                     `(p "Total results:" ,(if filtered? (number->string student-count) (number->string (length (all-students a-db))))))
                (form ([id "table"][action ,(embed/url table-handler)][method "POST"])
                      ,(render-students a-db embed/url))))))

  (define (render-students a-db embed/url)           
    `(div ((class "students"))
          (table ([class "w3-table w3-striped w3-bordered w3-hoverable"][style "max-width:100%"])
                 (th (button ([form "filters"][type "submit"][name "all-checked"][value ,(if all-checked "" "#t")])(img ([src ,(if all-checked "checked.png" "unchecked.png")]))))
                 (th "Student ID")
                 (th "Name")
                 (th "Status")
                 (th "Details")
                 ,@(map (λ (student)(render-student-row student embed/url)) (if filtered? filtered-students (all-students a-db))))))
  
  (define (render-student-row an-id embed/url)
    (define id-value (number->string an-id))
    `(tr      
      (td ,(checkmark "id" id-value "" all-checked))     
      (td ,(number->string an-id))
      (td ,(student-name a-db an-id) (br)
          (p ([style "font-size: smaller"])
                          ,(student-email a-db an-id)))
;             ,(substring (student-email a-db an-id) 0 10) "...") ;sample emails too long, mess up css
      (td (span ([style ,(string-append (cond [(student-assigned-locker? a-db an-id) "background-color:#86E660;"]
                                              [(student-awaiting-locker? a-db an-id) "background-color:#2196F3;"]
                                              [else ""])
                                        " height: 25px; width: 25px; border-radius: 50%; display: inline-block; ")])))      
      (td (form ([id "details"][action ,(embed/url view-student-details-handler)])
                (button ([class ,(button-style-class)][form "table"][type "submit"][name "student-details-id"][value ,id-value]) "Details")))))

  ;=-=-Handlers-=-=
  (define (table-handler request)
    (cond ((exists-binding? 'mass-assign (request-bindings request)) (mass-assign-handler request))
          ((exists-binding? 'student-details-id (request-bindings request)) (view-student-details-handler request))
          ((exists-binding? 'mass-email (request-bindings request)) (send-email-page a-db request))
          (else (render-view-students a-db request))))
  
  (define (filter-handler request)    
    (render-view-students a-db request))
  
  (define (mass-assign-handler request)    
    (confirm-mass-assign a-db request))

  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (view-student-details-handler request)
    (render-student-details a-db request))  

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Confirm mass assign student-locker=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (confirm-mass-assign a-db request)

  ;Remove extra row from students list
  (define students (list-tail (map cdr (request-bindings request)) 1))

  (define enough-lockers? (< (length students) (length (all-lockers a-db))))
  
  (define students-with-lockers
    (if enough-lockers? (filter (λ (student) (student-assigned-locker? a-db student)) students) (list)))
  
  (define students-without-lockers
    (if enough-lockers? (filter (λ (student) (not (student-assigned-locker? a-db student))) students) (list)))
  
  (define lockers
    (if enough-lockers? (map number->string (mass-assign-get-lockers a-db students-without-lockers)) (list)))
  
  (define assigned-lockers
    (if enough-lockers? (map (λ (student) (students-locker-id a-db student)) students-with-lockers) (list)))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (h2 "Confirm locker assignment")
                                
                ,(if (not (empty? students-without-lockers))                     
                     (nav-button (embed/url confirm-handler) "Confirm")
                     `(button ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled"))"Confirm"))
                     
                ,(nav-button (embed/url cancel-handler) "< Cancel"))
                            
           (div ((class "w3-twothird w3-card-4"))

                ,(if (not (empty? students-without-lockers))
                     `(p ,(render-table students-without-lockers lockers))       
                     (if enough-lockers? `(p
                                           (img ((style "max-width:10%;height:auto;")(src "warning.png")))
                                           "Error! No valid students selected. Please go back and select students you wish to assign lockers to.")
                         `(p
                           (img ((style "max-width:10%;height:auto;")(src "warning.png")))
                           "Error! More students selected than available lockers.")))



                ,(if (not (empty? students-with-lockers))
                     `(p
                       (h3 "Warning: following students already have lockers")
                       ,(render-table students-with-lockers (map number->string assigned-lockers)))
                     `(p))
                                
                ))))
 
  (define (render-row student locker)
    `(tr (td ,(string-append (student-firstname a-db student) " " (student-lastname a-db student)))
         (td ,locker)))
  
  (define (render-table students lockers)
    `(table ([class "w3-table w3-striped w3-bordered w3-hoverable"])(th "Student Ids")(th "Locker #s")
            ,@(map render-row students lockers)))

  ;=-=-Handlers-=-=
  (define (confirm-handler request)
    (mass-assign a-db students-without-lockers lockers)
    (mass-assign-successful-page a-db request))

  (define (cancel-handler request)
    (render-view-students a-db (redirect/get)))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Mass assign successful=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (mass-assign-successful-page a-db request)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h2 ((class "w3-text-green")) "Assignment successful!")
                                                                
                ,(nav-button (embed/url view-students-handler) "< View Students")                
                ,(nav-button (embed/url view-lockers-handler) "< View Lockers")                
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard"))
                
                            
           (div ((class "w3-twothird w3-card-4"))                                 
                (img ((style "max-width:100%")(src "https://i.pinimg.com/originals/35/f3/23/35f323bc5b41dc4269001529e3ff1278.gif")))
                ))))

  ;=-=-Handlers-=-=
  (define (view-students-handler request)
    (render-view-students a-db (redirect/get)))

  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))
  
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Assign Locks=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (assign-locks-page a-db request)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (h2 "Upload locker-lock csv file")
                (form 
                 ([action ,(embed/url import-csv-handler)]
                  [method "POST"]
                  [enctype "multipart/form-data"])
                 ,@(formlet-display file-upload-formlet) (br)
                 (input ([class ,(button-style-class)][type "submit"] [value "Upload"])))
                
                ,(nav-button (embed/url view-lockers-handler)  "< View Lockers")                
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard"))
                 
                            
           (div ((class "w3-twothird w3-card-4"))                 
                                
                ))))

  ;=-=-Handlers-=-=
  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (import-csv-handler request)         
    (render-confirm-assign-locks a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Confirm assign locks=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-confirm-assign-locks a-db request)
  
  (define-values (fname fcontents)
    (formlet-process file-upload-formlet request))
  ;Saves a local copy of the upload file under name "!uploaded-filename"
  (define save-name (string-append "!uploaded-" fname))
  (display-to-file fcontents (build-path files-path save-name) #:exists 'replace)

  (define-values (locker-ids lock-ids) (extract-locker-lock-columns a-db (open-input-file (build-path files-path save-name))))
    
  (define lockers-without-locks
    (filter (λ (locker) (not (locker-has-lock? a-db locker))) locker-ids))
  
  (define lockers-with-locks
    (filter (λ (locker) (locker-has-lock? a-db locker)) locker-ids))

  (define assigned-locks
    (map (λ (locker) (number->string (lock-id a-db locker))) lockers-with-locks))

  (define new-locks
    (map (λ (index) (list-ref lock-ids index))(map (λ (locker) (index-of locker-ids locker)) lockers-without-locks)))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (h2 "Confirm lock assignment")
                
                ,(nav-button (embed/url confirm-handler) "Confirm")                
                ,(nav-button (embed/url cancel-handler) "< Cancel"))
                            
           (div ((class "w3-twothird w3-card-4"))

                ,(if (not (empty? lockers-without-locks))
                     `(p ,(render-table lockers-without-locks new-locks))
                     `(p
                       (img ((style "max-width:10%;height:auto;")(src "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/OOjs_UI_icon_error-destructive.svg/1200px-OOjs_UI_icon_error-destructive.svg.png")))
                       "Error! File contains no new locker-lock relationships"))

                ,(if (not (empty? lockers-with-locks))
                     `(p (h3 "Warning: following lockers already have locks")
                         ,(render-table lockers-with-locks assigned-locks))
                     `(p))
                 
                ))))


  (define (render-row locker lock)
    `(tr (td ,locker) (td ,lock)))
  
  (define (render-table lockers locks)
    `(table ([class "w3-table w3-striped w3-bordered"])(th "Locker IDs")(th "Lock IDs")
            ,@(map render-row lockers locks)))

  ;=-=-Handlers-=-=
  (define (confirm-handler request)    
    (assign-locks! a-db (open-input-file (build-path files-path save-name)))   
    (mass-assign-successful-page a-db request))
  
  (define (cancel-handler request)
    (render-view-lockers a-db (redirect/get)))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Locker details=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-locker-details a-db request)
  
  (define locker-details-id (extract-binding/single 'locker-details-id (request-bindings request)))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, " ,admin-firstname))
                                 
                ,(nav-button (embed/url view-lockers-handler) "< Locker view")
                )
                            
           (div ((class "w3-twothird w3-card-4"))
                (h1 "Locker Details:")

                ,(display-locker-info locker-details-id embed/url)
                                
                ))))

  
  (define (display-locker-info id embed/url)
    (define student-id (number->string (locker-owner-id a-db id)))
    
    `(div ((class "w3-white"))
          (table ([class "w3-table w3-striped w3-bordered"])
                 (tr (td (h3 "Locker ID:"))(td ((class "w3-right-align")) (p ,id)))                 
                 (tr (td (h3 "Locker Location:"))(td ((class "w3-right-align")) ,(locker-location a-db id)))
                 (tr (td (h3 "Lock #:"))(td ((class "w3-right-align"))
                                            ,(if (locker-has-lock? a-db id)                             
                                                 `(div ,(number->string (lock-id a-db id)))
                                                 `(div "No Lock Assigned!"))))
                 (tr (td (h3 "Locker assigned to:"))(td ((class "w3-right-align"))
                                                        ,(if (locker-assigned? a-db id)
                                                             `(div ,(locker-owner-name a-db id)                                                           
                                                                   (form ([action ,(embed/url student-details-handler)][method "PUT"])
                                                                         (input ([type "hidden"][id "student-details-id"][name "student-details-id"][value ,student-id]))
                                                                         (input ([class ,(button-style-class)][type "submit"][value "Student Details"])))
                                                                   (form ([action ,(embed/url release-locker-handler)][method "DELETE"])
                                                                         (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,id]))
                                                                         (input ([class ,(button-style-class)][type "submit"][value "Release locker"]))
                                                                         ))
                                                             `(div
                                                               "None"
                                                               (form ([action ,(embed/url add-student-locker-handler)])
                                                                     (label ((for "student-id")) "Student ID:")
                                                                     (input ([required "required"][type "text"][id "student-id"][name "student-id"]))
                                                                     (input ([type "hidden"][id "locker-id"][name "locker-id"][value ,id]))
                                                                     (input ([class ,(button-style-class)][type "submit"][value "Assign"])))))))
                 (tr (td (h3 "Locker status: ")) (td ((class "w3-right-align")) (form ([action ,(embed/url (if (locker-broken? a-db id) set-fixed-handler set-broken-handler))][method "PUT"])
                                                                                      (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,id]))
                                                                                      ,(if (locker-broken? a-db id)
                                                                                           `(div
                                                                                             (div ([class "w3-orange w3-center"]) "Broken")
                                                                                             (input ([class ,(button-style-class)][type "submit"][value "Set fixed"])))
                                                                                           `(div
                                                                                             (div ([class "w3-blue w3-center"]) "Fixed")
                                                                                             (input ([class ,(button-style-class)][type "submit"][value "Set broken"]))))))) 
                 (tr (td (h3 "Notes:")) (td ((class "w3-right-align"))
                                            ,@(map (λ (a-note-id) (format-notes a-note-id embed/url)) (locker-notes a-db id))
                                            (form ([method "POST"][action ,(embed/url add-note-handler)])
                                                  (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,id]))
                                                  (input ([type "hidden"][id "note-author"][name "note-author"][value ,admin-firstname]))
                                                  (textarea ([required "required"][style "resize: none;"][rows "4"][cols "40"][id "new-note"][name "new-note"]))
                                                  (input ([class ,(button-style-class)][type "submit"][value "Add new note"])))))
                 )))

  (define (format-notes note-id embed/url)
    `(div ([class "w3-border w3-padding w3-pale-yellow"])
          (div ([class "note-user"]) ,(locker-note-author a-db note-id))
          (div ([class "note-content"]) ,(locker-note-content a-db note-id))
          (form ([action ,(embed/url remove-note-handler)])                
                (input ([type "hidden"][id "note-id"][name "note-id"][value ,(number->string note-id)]))
                (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,locker-details-id]))
                (input ([class "w3-button w3-large w3-padding-small w3-text-red"][type "submit"][value "x"])))))          

  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))

  (define (student-details-handler request)
    (render-student-details a-db request))

  (define (set-broken-handler request)
    (set-locker-broken! a-db locker-details-id)
    (render-locker-details a-db request))

  (define (set-fixed-handler request)
    (set-locker-fixed! a-db locker-details-id)
    (render-locker-details a-db request))

  (define (release-locker-handler request)
    (release-student-locker a-db locker-details-id)
    (render-locker-details a-db request))

  (define (add-note-handler request)
    (define id (extract-binding/single 'locker-details-id (request-bindings request)))
    (define note-author (extract-binding/single 'note-author (request-bindings request)))           
    (define note (extract-binding/single 'new-note (request-bindings request)))           
    (add-locker-note a-db id note note-author)
    (render-locker-details a-db request))

  (define (remove-note-handler request)    
    (define note-id (extract-binding/single 'note-id (request-bindings request)))
    (remove-locker-note a-db note-id)
    (render-locker-details a-db request))
  
  (define (add-student-locker-handler request)
    (confirm-add-student-locker a-db request))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Student details=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-student-details a-db request)
  
  (define student-details-id (extract-binding/single 'student-details-id (request-bindings request)))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, " ,admin-firstname))
                
                ,(nav-button (embed/url view-students-handler) "< Student view"))
                                                        
           (div ((class "w3-twothird w3-card-4"))
                (h1 "Student Details:")

                ,(display-student-info student-details-id embed/url)
                                
                ))))
   
  (define (display-student-info id embed/url)            
    `(div ((class "w3-white"))
          (table ([class "w3-table w3-striped w3-bordered"])
                 (tr (td  (h2 "Student ID:"))(td ((class "w3-right-align")) (h3 ,id)))  
                 (tr (td (h3 "Student Name:"))(td ((class "w3-right-align")) ,(student-name a-db id)))
                 (tr (td (h3 "Program:"))(td ((class "w3-right-align")) ,(student-program a-db id)))
                 (tr (td  (h3 "Locker:"))(td ((class "w3-right-align"))
                                             ,(if (student-assigned-locker? a-db id)
                                                  `(div                 
                                                    ,(number->string (students-locker-id a-db id))
                                                    (form ([action ,(embed/url locker-details-handler)])
                                                          (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,(number->string (students-locker-id a-db id))]))
                                                          (input ([class ,(button-style-class)][type "submit"][value "Locker details"]))))
                                                  `(div
                                                    "No locker assigned"
                                                    (form ([action ,(embed/url add-student-locker-handler)])
                                                          (input ([required "required"][type "text"][id "locker-id"][name "locker-id"]))
                                                          (input ([type "hidden"][id "student-id"][name "student-id"][value ,id]))
                                                          (input ([class ,(button-style-class)][type "submit"][value "Add locker"])))))))
                 (tr (td(h3 "Email:"))(td ((class "w3-right-align")) ,(student-email a-db id)))
                 (tr (td (h3 "Notes:")) (td ((class "w3-right-align"))
                                            ,@(map (λ (a-note-id) (format-notes a-note-id embed/url)) (student-notes a-db id))
                                            (form ([method "POST"][action ,(embed/url add-note-handler)])
                                                  (input ([type "hidden"][id "student-details-id"][name "student-details-id"][value ,id]))
                                                  (input ([type "hidden"][id "note-author"][name "note-author"][value ,admin-firstname]))
                                                  (textarea ([style "resize: none;"][rows "4"][cols "40"][id "new-note"][name "new-note"]))
                                                  (input ([class ,(button-style-class)][type "submit"][value "Add new note"]))))))))

  (define (format-notes note-id embed/url)
    `(div ([class "w3-border w3-padding w3-pale-yellow"])
          (div ([class "note-user"]) ,(student-note-author a-db note-id))
          (div ([class "note-content"]) ,(student-note-content a-db note-id))
          (form ([action ,(embed/url remove-note-handler)])                
                (input ([type "hidden"][id "note-id"][name "note-id"][value ,(number->string note-id)]))
                (input ([type "hidden"][id "student-details-id"][name "student-details-id"][value ,student-details-id]))
                (input ([class "w3-button w3-large w3-padding-small w3-text-red"][type "submit"][value "x"])))))

  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (add-note-handler request)
    (define id (extract-binding/single 'student-details-id (request-bindings request)))
    (define note-author (extract-binding/single 'note-author (request-bindings request)))           
    (define note (extract-binding/single 'new-note (request-bindings request)))           
    (add-student-note a-db id note note-author)
    (render-student-details a-db request))

  (define (remove-note-handler request)    
    (define note-id (extract-binding/single 'note-id (request-bindings request)))
    (remove-student-note a-db note-id)
    (render-student-details a-db request))
  
  (define (view-students-handler request)
    (render-view-students a-db (redirect/get)))

  (define (add-student-locker-handler request)
    (confirm-add-student-locker a-db request))

  (define (locker-details-handler request)
    (render-locker-details a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Confirm new student-locker=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (confirm-add-student-locker a-db request)

  
  (define locker-id (extract-binding/single 'locker-id (request-bindings request)))
  (define student-id (extract-binding/single 'student-id (request-bindings request)))

  (define error-msg (cond ((not (locker-exists? a-db locker-id)) (string-append "Warning! The locker ID you entered doesn't exist in the database: " locker-id))
                          ((not (student-exists? a-db student-id)) (string-append "Warning! The student ID you entered doesn't exist in the database: " student-id))
                          ((student-assigned-locker? a-db student-id) (string-append "Warning! Selected student already has assigned locker: " student-id))
                          ((locker-assigned? a-db locker-id) (string-append "Warning! Selected locker is already assigned: " locker-id))))

  (define all-ok? (and (locker-exists? a-db locker-id)
                       (student-exists? a-db student-id)
                       (not (student-assigned-locker? a-db student-id))
                       (not (locker-assigned? a-db locker-id))))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (h2 "Confirm locker assignment")

                ,(if all-ok?
                     (nav-button (embed/url confirm-handler) "Confirm"
                                 #:hidden-fields `(input ([type "hidden"][id "student-details-id"][name "student-details-id"][value ,student-id])))
                     `(input ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled")(type "submit")(value "Confirm"))))
                ,(nav-button (embed/url cancel-handler) "< Cancel"
                             #:hidden-fields `(input ([type "hidden"][id "student-details-id"][name "student-details-id"][value ,student-id]))))
           (div ((class "w3-twothird w3-card-4"))                
                ,(if all-ok?
                     `(table ([class "w3-table w3-striped w3-bordered"])
                             (th (h2 "Student Name"))(th (h2 "Locker ID"))
                             (tr (td ,(student-name a-db student-id))(td ,locker-id)))
                     `(div (img ((style "max-width:10%;height:auto;")(src "warning.png")))
                           ,error-msg
                           ))
                                
                ))))

  ;=-=-Handlers-=-=
  (define (confirm-handler request)   
    (insert-student-locker a-db student-id locker-id "single-assigned")
    (render-view-students a-db (redirect/get)))

  (define (cancel-handler request)
    (render-view-students a-db (redirect/get)))
  
  (send/suspend/dispatch response-generator))
  
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Upload lockers=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-upload-lockers a-db request)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))                
                (h3 "Upload locker CSV file:")
                (form 
                 ([action ,(embed/url import-csv-handler)]
                  [method "POST"]
                  [enctype "multipart/form-data"])
                 ,@(formlet-display file-upload-formlet)(br)
                 (input ([class ,(button-style-class)][type "submit"] [value "Upload"])))
                
                ,(nav-button (embed/url view-lockers-handler) "< View Lockers")
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard")))))
                 
  ;=-=-Handlers-=-=
  (define (view-lockers-handler request)
    (render-view-lockers a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (import-csv-handler request)
    (define num-lockers-before (length (all-lockers a-db)))
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename", in order to read
    (define save-name (string-append "!uploaded-" fname))    
    (display-to-file fcontents (build-path files-path save-name) #:exists 'replace)
    (db-import-locker-csv! a-db (open-input-file (build-path files-path save-name)))
    (define num-lockers-after (length (all-lockers a-db)))
    (define num-new-lockers (- num-lockers-after num-lockers-before))
    (render-upload-successful-page a-db (redirect/get) num-new-lockers))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Upload students=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-upload-students a-db request)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (h3 "Upload student CSV file:")
                (form 
                 ([action ,(embed/url import-csv-handler)]
                  [method "POST"]
                  [enctype "multipart/form-data"])
                 ,@(formlet-display file-upload-formlet)(br)
                 (input ([class ,(button-style-class)][type "submit"] [value "Upload"])))
                                                                 
                ,(nav-button (embed/url view-students-handler) "< View Students")                
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard")))))

  ;=-=-Handlers-=-=
  (define (view-students-handler request)
    (render-view-students a-db (redirect/get)))

  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (import-csv-handler request)
    (define num-students-before (length (all-students a-db)))
    (define-values (fname fcontents)
      (formlet-process file-upload-formlet request))
    ;Saves a local copy of the upload file under name "!uploaded-filename"
    (define save-name (string-append "!uploaded-" fname))
    (display-to-file fcontents (build-path files-path save-name) #:exists 'replace)    
    (db-import-student-csv! a-db (open-input-file (build-path files-path save-name)))
    (define num-students-after (length (all-students a-db)))
    (define num-new-students (- num-students-after num-students-before))
    
    (render-upload-successful-page a-db (redirect/get) num-new-students))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Upload success page=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-upload-successful-page a-db request num-new-rows)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h2 ((class "w3-text-green")) "File upload successful!")
                (p ,(number->string num-new-rows) " new rows imported.")
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard"))
           
           (div ((class "w3-twothird w3-card-4"))
                (img ((style "max-width:100%")(src "success.gif")))
                ))))
           
  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Send Email=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (send-email-page a-db request)
  
  (define email-type (cond ((exists-binding? 'mass-email (request-bindings request)) "Mass Student Email")
                           ((exists-binding? 'work-order (request-bindings request)) "Work Order")))  

  (define recipients (cond ((exists-binding? 'mass-email (request-bindings request)) (map (λ (student-id) (student-email a-db student-id)) (extract-bindings 'id (request-bindings request))))
                           ((exists-binding? 'work-order (request-bindings request)) (list "tsandelkonjevic@ryerson.ca")))) ;TODO: replace with fixit@ryerson.ca
  
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h2 "Send email")
                
                ,(if (empty? (extract-bindings 'id (request-bindings request)))
                     `(input ((class "w3-block w3-btn w3-ripple w3-blue w3-disabled")(value "Send")))
                     `(button ([class ,(button-style-class)][form "email"][type "submit"][name "send"]) "Send")) (br)
                                                                                                                 ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard"))
           
           (div ((class "w3-twothird w3-card-4"))
                (h2 ,(string-append "New message - " email-type))
                ,(if (empty? (extract-bindings 'id (request-bindings request))) `(p
                                                                                  (img ((style "max-width:10%;height:auto;")(src "warning.png")))
                                                                                  "Error! No students/lockers selected. Please go back and select students/lockers you wish to include in message.") "")
                (form ([id "email"][action ,(embed/url send-handler)][method "POST"])
                      (table ((class "w3-table w3-bordered"))
                             (tr (td "To: " ,@(map (λ (r) (string-append r ", ")) recipients)))
                             (tr (td "Subject: "
                                     (input ([type "text"]
                                             [name "subject"]
                                             [id "subject"]
                                             [value ,(if (exists-binding? 'work-order (request-bindings request))
                                                         "Locker repairs"
                                                         "Mass email")]))))
                             (tr (td (textarea ([name "body"][id "body"][rows "10"][cols "60"])
                                               ,email-body)))))))))
                                                         

  (define email-body
    (cond ((exists-binding? 'work-order (request-bindings request))
           (string-append* (map (λ (s) (string-append s ", ")) (extract-bindings 'id (request-bindings request)))))
          ((exists-binding? 'mass-email (request-bindings request)) "Dear Students,")))
  
  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-admin-dashboard (redirect/get) a-db))

  (define (send-handler request)
    (smtp-send-message "localhost"
                       "noreply@racket.cs.ryerson.ca"
                       recipients
                       (standard-message-header "noreply@racket.cs.ryerson.ca"
                                                (list) ;to
                                                (list) ;cc
                                                recipients ;bcc
                                                (extract-binding/single 'subject (request-bindings request)))
                       (list (extract-binding/single 'body (request-bindings request))
                             "------
This message was automatically generated by the locker management system. For any questions/concerns please email tsandelkonjevic [at] ryerson [dot] ca")
                       #:port-no 25)
    
    (render-admin-dashboard (redirect/get) a-db))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Other=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(define (checkmark name value text [checked #f])
  `(label ([class "container"]) ,text
          ,(if checked
               `(input ([type "checkbox"][name ,name][value ,value][checked "checked"]))
               `(input ([type "checkbox"][name ,name][value ,value])))
          (span ([class "checkmark"]))))

(define (extract-bindings-safely name default request)
  (if
   (and (exists-binding? name (request-bindings request)) (non-empty-string? (extract-binding/single name (request-bindings request))))
   (extract-binding/single name (request-bindings request))
   default))

(define (extract-checkmark-bindings-safely name default request)
  (if (exists-binding? name (request-bindings request))
      (extract-binding/single name (request-bindings request))
      default))

(define (style-link)
  `((link ((rel "stylesheet")
           (href "style.css")))))

(define (button-style-class)
  "w3-block w3-btn w3-ripple w3-blue w3-round-large")

(define login-formlet
  (formlet
   ,(=> (radio-group (list "Admin" "Student"))
        usertype)
   usertype))

(define file-upload-formlet
  (formlet
   (div ,{(file-upload) . => . binds})
   (let
       ([fname (bytes->string/utf-8 (binding:file-filename binds))]
        [fcontents (binding:file-content binds)])
     (values fname fcontents))))

(define (html-wrap content)
  (response/xexpr
   `(html (head (title "Locker Management System")
                ,@(style-link))
          (body ((class "w3-container"))
                (div ((class "w3-content w3-margin-top"))                       
                     ,content)))))

(define (nav-button handler text #:hidden-fields [hidden-fields ""] #:post [post #f])
  `(form ([action ,handler][method ,(if post "post" "get")])
         (input ([class ,(button-style-class)][type "submit"][value ,text]))
         ,hidden-fields))

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8005))

(define timeout-manager (create-timeout-manager
                         (λ (r) (response/xexpr `(html (p "Session expired. Don't worry, your work has been saved. Please go back to main page."))))
                         3600
                         3600))

(serve/servlet start               
               #:quit? #f
               #:launch-browser? #f
               #:listen-ip #f
               #:port port
               #:server-root-path files-path               
               #:manager timeout-manager
               #:extra-files-paths (list files-path)                              
               #:servlet-path "/webapp.rkt")
               


