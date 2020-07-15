#lang racket

(require net/smtp)
(require net/dns)
(require net/head)
(require openssl/mzssl)


(smtp-send-message
  (dns-get-address (dns-find-nameserver) "smtp.gmail.com")
  "mrtheo1000@gmail.com"
  '("mrtheo1000@gmail.com")
  (standard-message-header
   "X Sender <mrtheo1000@gmail.com>"
   '("X Recipient <mrtheo1000@gmail.com>")
   '() ; CC
   '() ; BCCa
   "saying hello")
  '("hello world!")
  #:port-no 465
  #:auth-user "mrtheo1000"
  #:auth-passwd "gtagta777mr"
  #:tcp-connect ssl-connect)