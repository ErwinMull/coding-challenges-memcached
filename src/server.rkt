#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp
         racket/date)

;;; =============================== Exports ====================================

(provide serve/test)

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

(define (serve/private port-no
                       #:test? [test? #f])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-logger lg]
                 [date-display-format 'iso-8601])
    (define listener (tcp-listen port-no 5 #t))

    (thread
     (λ ()
       (let loop ()
         (accept-and-handle listener)
         (loop))))

    (cond
      [test?
       (λ () (custodian-shutdown-all cust))]
      [else
       (define shutdown-log-thread
         (let ([t (thread log-loop)])
           (λ ()
             (channel-put exit-log-loop-channel #t)
             (thread-wait t))))
       (log-info "Server started")
       (with-handlers ([exn:break? (lambda (e)
                                     (log-info "Server shutdown")
                                     (shutdown-log-thread)
                                     (custodian-shutdown-all cust))])
         (sync/enable-break never-evt))])))

(define (serve/test [port-no 11211])
  (define stop (serve/private port-no
                              #:test? #t))
  stop)

(define (serve [port-no 11211])
  (serve/private port-no))
