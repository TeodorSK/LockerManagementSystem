#lang racket/base

(require web-server/servlet)
(require web-server/formlets)
(require xml)
(require string-util)

;(require "model.rkt")
(provide (all-defined-out))

(define (render-student-dashboard request)
  (define (response-generator embed/url)
    (html-wrap
     `(div ((class "w3-row-padding"))
           (div ((class "w3-third w3-white w3-text-grey w3-card-4"))
                ;(a ([href ,(embed/url dashboard-handler)])(img ([style "max-width:20%;"][src "home_btn.svg"])))
                (h3 (string-append "Welcome back, Student"))                
                )
           
           (div ((class "w3-twothird w3-card-4"))
                (h3 "Select an option from the menu")
                            
                ))))

  (send/suspend/dispatch response-generator))




(define (html-wrap content)
  (response/xexpr
   `(html (head (title "Locker Management System")
                ,@(style-link))
          (body ((class "w3-container"))
                (div ((class "w3-content w3-margin-top"))                       
                     ,content)))))

(define (style-link)
  `((link ((rel "stylesheet")
           (href "style.css")))))


     