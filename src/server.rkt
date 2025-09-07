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

;;; ============================= CONNECTIONS ==================================

(define (tcp-connection-evt in out eof-return-value)
  (choice-evt (handle-evt (eof-evt in)
                          (λ _
                            eof-return-value))
              (handle-evt (read-bytes-line-evt in 'return-linefeed)
                          (λ (line)
                            (void
                             (when (not (eof-object? line))
                               (enqueue/header&ports line in out)))))))

(struct connection-table (ht [evt #:mutable])
  #:property prop:evt (λ (conn-tab)
                        (connection-table-evt conn-tab)))

(define (initialize-connection-table)
  (connection-table (make-hasheq) (choice-evt)))

(define (connection-table-generate-event! conn-tab)
  (define table (connection-table-ht conn-tab))
  (set-connection-table-evt! conn-tab
                             (handle-evt
                              (apply choice-evt
                                     (map (λ (item)
                                            (define-values (id in out)
                                              (apply values item))
                                            (tcp-connection-evt in out id))
                                          (hash->list table)))
                              (λ (res)
                                (when (and (not (void? res))
                                           (hash-has-key? table res))
                                  (connection-table-remove! conn-tab res))))))

(define (connection-table-set! conn-tab in out)
  (define id (gensym))
  (define lst `(,in ,out))
  (hash-set! (connection-table-ht conn-tab) id lst)
  (connection-table-generate-event! conn-tab))

(define (connection-table-remove! conn-tab id)
  (define ht (connection-table-ht conn-tab))
  (define ports (hash-ref ht id))
  (hash-remove! ht id)
  (for-each tcp-abandon-port ports)
  (connection-table-generate-event! conn-tab))

(define (connection-table-clear! conn-tab)
  (define table (connection-table-ht conn-tab))
  (for-each (curry for-each tcp-abandon-port) (hash-values table))
  (hash-clear! table)
  (set-connection-table-evt! conn-tab (choice-evt)))

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
