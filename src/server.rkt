#lang racket/base

(require racket/tcp)

(define (handle in out)
  (write-bytes #"HELLO, CLIENT :)\n" out))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (serve [port-no 11211])
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))
