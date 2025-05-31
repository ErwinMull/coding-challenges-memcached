#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp)

;;; =============================== Exports ====================================
(provide serve)

;;; =========================== Request handling ===============================

(define (handle in out)
  (let* ([buf (make-bytes 1024 0)])
    (define nread (read-bytes-avail! buf in))
    (write-bytes (subbytes buf 0 nread) out)))

;;; ========================= Server & connections =============================

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (serve [port-no 11211])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (λ ()
    (custodian-shutdown-all cust)))
