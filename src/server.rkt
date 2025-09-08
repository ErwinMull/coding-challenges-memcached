#lang racket/base

;;; =============================== IMPORTS ====================================

(require racket/tcp
         racket/function
         racket/vector
         racket/date
         racket/port

         "protocol.rkt")

;;; =============================== EXPORTS ====================================

(provide serve)

;;; ================================ CONFIG ====================================

(define LOG-LEVEL 'debug)
(define WORKER-THREADS-AMOUNT 16)

;;; =============================== LOGGING ====================================

(define (print-log-message port date-str level message)
  (fprintf port "[~a] ~a: ~a\n" date-str level message))

(define (setup-log-thread [nrc (make-log-receiver (current-logger) LOG-LEVEL)]
                          [port (current-output-port)])
  (define t
    (thread
     (thunk
      (let loop ()
        (sync/enable-break (handle-evt
                            nrc
                            (λ (vec)
                              (define level (vector-ref vec 0))
                              (define message (vector-ref vec 1))
                              (print-log-message port
                                                 (date->string (current-date))
                                                 level
                                                 message)
                              (loop)))
                           (thread-receive-evt))))))
  (thunk
   (thread-send t 1 (thunk (void)))
   (thread-wait t)))

;;; ================================ SERVER ====================================

(define (serve [port-no 11211]
               #:log? [log? #f]
               #:log-file [log-file (current-output-port)])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-logger (make-logger)]
                 [current-output-port log-file]
                 [date-display-format 'iso-8601])
    (define kill-log-thread
      (if log?
          (setup-log-thread)
          (thunk (void))))
    (log-info (format "Starting server on port ~a" port-no))
    (define listener (tcp-listen port-no 5 #t))
    (define conn-tab (initialize-connection-table))
    (define thread-pool
      (make-worker-thread-pool WORKER-THREADS-AMOUNT
                               (thunk
                                (define-values (header in out)
                                  (dequeue/header&ports))
                                (log-info (format "Received: ~a" header))
                                (handle-memcached-command header in out))))
    (with-handlers ([exn:break? (λ (e)
                                  (log-info "Shutting down server")
                                  (connection-table-clear! conn-tab)
                                  (kill-worker-thread-pool thread-pool)
                                  (tcp-close listener)
                                  (kill-log-thread)
                                  (custodian-shutdown-all cust))])
      (let loop ()
        (sync/enable-break
         (handle-evt (tcp-accept-evt listener)
                     (λ (in-out-lst)
                       (connection-table-set! conn-tab
                                              (car in-out-lst)
                                              (cadr in-out-lst))
                       (loop)))
         (handle-evt (connection-table-evt conn-tab)
                     (λ _
                       (loop))))))))

;;; ================================= MAIN =====================================

(module+ main
  (serve #:log? #t))
