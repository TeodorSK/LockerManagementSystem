#lang racket
(require net/smtp)
(require net/head)
(smtp-send-message "localhost"
                   "test@mail.test.com"
                   (list "tsandelkonjevic@ryerson.ca")
                   (standard-message-header "test@mail.test.com"
                                            (list "tsandelkonjevic@ryerson.ca") ;to
                                            (list) ;cc
                                            (list) ;bcc
                                            "Test subject")
                   "It works!"
                   #:port-no 25)