#lang racket
(require net/smtp)
(require net/head)
(smtp-send-message "localhost"
                   "test@mail.test.com"
                   "tsandelkonjevic@ryerson.ca"
                   (standard-message-header "test@mail.test.com"
                                            (list "tsandelkonjevic@ryerson.ca")
                                            (list) (list) "Test subject")
                   "It works!"
                   #:port-no 25)