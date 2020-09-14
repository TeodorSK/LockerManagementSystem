#lang racket
(require net/smtp)
(require net/head)
(smtp-send-message "localhost"
                   "test@mail.test.com"
                   (build-list 100 (λ (x) "mrtheo1000@gmail.com"))
                   (standard-message-header "test@mail.test.com"
                                            (list) ;to
                                            (list) ;cc
                                            (build-list 100 (λ (x) "mrtheo1000@gmail.com")) ;bcc
                                            "spam test")
                   "spam test!!!"
                   #:port-no 25)

