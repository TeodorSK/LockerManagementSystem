#lang racket/base
(provide (all-defined-out))

(define username "None")
(define user-type "None")

(define (set-username! new-username)
  (set! username new-username))

(define (set-user-type! type)
  (set! user-type type))