#lang racket

(require web-server/servlet)
(require racket/runtime-path)
(require net/smtp)
(require net/head)


(require "model.rkt")
(provide (all-defined-out))

;(define student-firstname "NoneS")
(define student-id "0")
(define student-fname "NoneS")

(define-runtime-path files-path "htdocs")


;(define (admin-unauth-page request)
;  (response/xexpr
;   `(html (p "Error: You are trying to access the admin dashboard, but you are not an authorized administrator. If you believe this is an error please contact tsandelkonjevic [at] ryerson [dot] ca"))))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Student start=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (student-start request [a-db (init-db! (build-path files-path "database.db"))])
;  (print (string->number (extract-binding/single 'cas-studentnumber (request-headers request))))
;  (print (all-students a-db))

  (set! student-id (extract-binding/single 'cas-studentnumber (request-headers request)))
  
  (if (not (member (string->number student-id) (all-students a-db)))
      (student-unauth-page request)      
      (render-student-dashboard request a-db))
      
  )

(define (student-unauth-page request)
  (response/xexpr
   `(html (p "Error: You are trying to access the student dashboard, but you are not in the db. If you believe this is an error please contact alina [at] ryerson [dot] ca"))))
  
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Student dashboard=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-student-dashboard request [a-db (init-db! (build-path files-path "database.db"))])

  (set! student-id (extract-binding/single 'cas-studentnumber (request-headers request)))        

  (set! student-fname (student-firstname a-db student-id))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))                
                (h3 (string-append "Welcome back, " ,student-fname))

                ,(nav-button (embed/url my-locker-handler) "My Locker")
                ,(nav-button (embed/url my-profile-handler) "My Profile")                
                )
           
           (div ((class "w3-twothird w3-card-4"))
                (h3 "Select an option from the menu")
                            
                ))))

  ;=-=-Handlers-=-=
  (define (my-locker-handler request)
    (render-my-locker a-db request))

  (define (my-profile-handler request)
    (render-my-profile a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-My Locker=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-my-locker a-db request)

  ;TODO replace with queries
  ;  (define id (number->string 1093)) ;students locker
  (define locker-id (if (student-assigned-locker? a-db student-id) (students-locker-id a-db student-id) "0"))
  ;  (define student-details-id (number->string 3974133))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))                
                (h3 (string-append "Welcome back, " ,student-fname))

                ,(nav-button (embed/url my-locker-handler) "My Locker")
                ,(nav-button (embed/url my-profile-handler) "My Profile")                
                )
           
           (div ((class "w3-twothird w3-card-4"))
                (h1 "My Locker Details:")

                ,(display-locker-info locker-id embed/url)
                                
                ))))

  (define (display-locker-info id embed/url)    
    
    `(div ((class "w3-white"))           

          ;Only render this if no locker assigned
          ,(if (not (student-assigned-locker? a-db student-id))
               `(form ([action ,(embed/url request-locker-handler)])
                      ;                (input ([type "hidden"][name "student-id"][value ,student-id]))
                      (button ([class ,(button-style-class)][type "submit"][name "locker-request"]) "Request new locker"))

               ;Otherwise render this
               `(table ([class "w3-table w3-striped w3-bordered"])
                       (tr (td (h3 "Locker ID:"))(td ((class "w3-right-align")) (p ,(number->string locker-id))))                 
                       (tr (td (h3 "Locker Location:"))(td ((class "w3-right-align")) ,(locker-location a-db locker-id)))
                       (tr (td (h3 "Lock #:"))
                           (td ((class "w3-right-align"))
                               ,(if (locker-has-lock? a-db locker-id)                             
                                    `(div ,(number->string (lock-id a-db locker-id)))
                                    `(div "No Lock Assigned!"))))                 
                       (tr (td (h3 "Locker status: "))
                           (td ((class "w3-right-align")) (form ([action ,(embed/url report-issue-handler)][method "PUT"])
                                                                ;                                                                                          (input ([type "hidden"][id "locker-details-id"][name "locker-details-id"][value ,locker-id]))
                                                                (input ([class ,(button-style-class)][type "submit"][name "report-issue"][value "Report issue"])))))
                                                                                           
                       (tr (td (h3 "Notes:")) (td ((class "w3-right-align"))
                                                  ,@(map (λ (a-note-id) (format-notes a-note-id embed/url)) (locker-notes a-db locker-id))))))))
                                            

  (define (format-notes note-id embed/url)
    `(div ([class "w3-border w3-padding w3-pale-yellow"])
          (div ([class "note-user"]) ,(locker-note-author a-db note-id))
          (div ([class "note-content"]) ,(locker-note-content a-db note-id))))
                          
  ;=-=-Handlers-=-=
  (define (request-locker-handler request)
    (student-send-mail-page a-db request))
  
  (define (report-issue-handler request)    
    (student-send-mail-page a-db request))
  
  (define (my-locker-handler request)
    (render-my-locker a-db request))

  (define (my-profile-handler request)
    (render-my-profile a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-My Profile=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (render-my-profile a-db request)
  
  ;  (define student-id (number->string 3974133))
  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))                
                (h3 (string-append "Welcome back, " ,student-fname))

                ,(nav-button (embed/url my-locker-handler) "My Locker")
                ,(nav-button (embed/url my-profile-handler) "My Profile")                
                )
           
           (div ((class "w3-twothird w3-card-4"))
                (h1 "My Profile Details:")

                ,(display-student-info student-id embed/url)
                                
                ))))

  (define (display-student-info id embed/url)            
    `(div ((class "w3-white"))
          (table ([class "w3-table w3-striped w3-bordered"])
                 (tr (td  (h2 "Student ID:"))(td ((class "w3-right-align")) (h3 ,student-id)))  
                 (tr (td (h3 "Student Name:"))(td ((class "w3-right-align")) ,(student-name a-db student-id)))                 
                 (tr (td(h3 "Email:"))(td ((class "w3-right-align")) ,(student-email a-db student-id)))
                 (tr (td(h3 "Notes:"))(td ((class "w3-right-align")) "")))))
          

  ;=-=-Handlers-=-=
  (define (my-locker-handler request)
    (render-my-locker a-db request))

  (define (my-profile-handler request)
    (render-my-profile a-db request))

  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Send mail=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
(define (student-send-mail-page a-db request)

  ;  (define locker-details-id (extract-bindings-safely 'locker-details-id "" request))
  (define locker-id (if (student-assigned-locker? a-db student-id) (students-locker-id a-db student-id) "0"))
  ;  (define student-id (extract-bindings-safely 'student-id "" request))

  (define email-type (cond ((exists-binding? 'locker-request (request-bindings request)) "Request for new locker")
                           ((exists-binding? 'report-issue (request-bindings request)) "Report an issue")))

  
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                (a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h2 "Send mail")
                
                (button ([class ,(button-style-class)][form "mail"][type "submit"][name "send"]) "Send") (br)
                ,(nav-button (embed/url dashboard-handler) "< Back to Dashboard"))
           
           (div ((class "w3-twothird w3-card-4"))
                (h2 ,(string-append "New message - " email-type))
                (form ([id "mail"][action ,(embed/url send-handler)][method "POST"])
                      (input ([type "hidden"][name ,(cond ((exists-binding? 'locker-request (request-bindings request)) "locker-request")
                                                          ((exists-binding? 'report-issue (request-bindings request)) "report-issue"))][value "1"]))
                      (table ((class "w3-table w3-bordered"))
                             ;                       (tr (td "To: " ,@(map (λ (r) (string-append r ", ")) recipients)))
                             (tr (td "Subject: " (input ([type "text"][name "subject"][id "subject"][value ,(if (exists-binding? 'locker-request (request-bindings request)) "New locker request" "Issue with locker")]))))
                             (tr (td (textarea ([name "body"][id "body"][rows "10"][cols "60"])
                                               ,email-body)))))))))

  (define email-body
    (cond ((exists-binding? 'locker-request (request-bindings request)) (string-append "I want a new locker. My student # is " student-id))
          ((exists-binding? 'report-issue (request-bindings request)) (string-append "Issue with my locker. My locker # is " locker-id))))
                
           
  ;=-=-Handlers-=-=
  (define (dashboard-handler request)
    (render-student-dashboard (redirect/get) a-db))

  (define (send-handler request)
    
    (cond ((exists-binding? 'locker-request (request-bindings request))
           (print "requesting locker")
           (student-request-locker a-db student-id)))
    
    (smtp-send-message "localhost"
                       (student-email a-db student-id) ;Current students email - TODO: is this ok to be @ryerson? allows alina to reply easily, gmail complains abt security
                       (list "tsandelkonjevic@ryerson.ca") ;Admin email TODO: change to alinas
                       (standard-message-header (student-email a-db student-id) ;Current student email
                                                (list "tsandelkonjevic@ryerson.ca") ;to
                                                (list) ;cc
                                                (list) ;bcc
                                                ;                                            (cond ((exists-binding? 'locker-request (request-bindings request)) "New locker request")
                                                ;                                                  ((exists-binding? 'report-issue (request-bindings request)) "Locker repair req")) ;don't hardcode this, resolve in 
                                                (extract-binding/single 'subject (request-bindings request)))
                       (list (extract-binding/single 'body (request-bindings request)))
                       #:port-no 25)

    
    
    (render-student-dashboard (redirect/get) a-db))
  
  (send/suspend/dispatch response-generator))

;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-Other=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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

(define (extract-bindings-safely name default request)
  (if
   (and (exists-binding? name (request-bindings request)) (non-empty-string? (extract-binding/single name (request-bindings request))))
   (extract-binding/single name (request-bindings request))
   default))

(define (button-style-class)
  "w3-block w3-btn w3-ripple w3-blue w3-round-large")

(define (style-link)
  `((link ((rel "stylesheet")
           (href "style.css")))))


