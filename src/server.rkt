#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp
         racket/date)

;;; =============================== Exports ====================================

(provide serve)

;;; =============================== Logging ====================================

(define lg (make-logger))
(define nrc (make-log-receiver lg 'info))

(define exit-log-loop-channel (make-channel))

(define (log-loop)
  (sync
   (handle-evt nrc
                 (λ (v)
                   (printf "[~a] ~a: ~a\n"
                           (date->string (current-date) #t)
                           (vector-ref v 0)
                           (vector-ref v 1))
                   (log-loop)))
   exit-log-loop-channel))

;;; =========================== Request handling ===============================

(define (handle in out)
  (let* ([buf (make-bytes 1024 0)])
    (define nread (read-bytes-avail! buf in))
    (write-bytes (subbytes buf 0 nread) out)))

;;; ========================= Server & connections =============================

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))

  (log-info "Accepted")

  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (serve [port-no 11211])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-logger lg]
                 [date-display-format 'iso-8601])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (define log-thread (thread log-loop))
    (thread loop)
    (log-info "Server started")
    (with-handlers ([exn:break? (lambda (e)
                                  (log-info "Server shutdown")
                                  (channel-put exit-log-loop-channel #t)
                                  (thread-wait log-thread)
                                  (custodian-shutdown-all cust))])
      (sync/enable-break never-evt))))
