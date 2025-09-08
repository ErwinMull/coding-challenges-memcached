#lang racket/base

;;; =============================== IMPORTS ====================================

(require racket/tcp
         racket/date
         racket/logging

         "handler.rkt"
         "connection.rkt"
         "thread-pool.rkt")

;;; =============================== EXPORTS ====================================

(provide serve)

;;; ================================ CONFIG ====================================

(define LOG-LEVEL 'debug)
(define WORKER-THREADS-AMOUNT 16)

;;; =============================== LOGGING ====================================

(define (print-log-message port level message)
  (fprintf port "[~a] ~a: ~a\n" (date->string (current-date)) level message))

;;; ================================ SERVER ====================================

(define (serve [port-no 11211]
               #:log? [log? #f]
               #:log-file [log-file (current-output-port)])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [date-display-format 'iso-8601])
    (with-intercepted-logging
      (if log?
          (λ (vec)
            (define level (vector-ref vec 0))
            (define message (vector-ref vec 1))
            (print-log-message log-file level message))
          (λ _ (void)))
      (λ ()
        (log-info (format "Starting server on port ~a" port-no))
        (define listener (tcp-listen port-no 5 #t))
        (define thread-pool
          (make-worker-thread-pool WORKER-THREADS-AMOUNT
                                   (λ (vec)
                                     (define-values (header in out)
                                       (unpack-header&ports vec))
                                     (handle header in out))))
        (event-loop listener
                    (λ (header in out)
                      (enqueue (pack-header&ports header in out))))
        (log-info "Shutting down server")
        (kill-worker-thread-pool thread-pool)
        (tcp-close listener))
      LOG-LEVEL))
  (custodian-shutdown-all cust))

;;; ================================= MAIN =====================================

(module+ main
  (serve #:log? #t))
