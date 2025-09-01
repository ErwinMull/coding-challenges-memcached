#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp
         racket/function
         racket/vector
         racket/date)

;;; ================================ CONFIG ====================================

(define LOG-LEVEL 'debug)
(define WORKER-THREADS-AMOUNT 16)

;;; ============================ WORKER THREADS ================================

(define QUEUE (make-channel))

(define (enqueue val)
  (channel-put QUEUE val))

(define (dequeue)
  (channel-get QUEUE))

(define (make-worker-thread-pool n thnk)
  (for/list ([_ (in-range n)])
    (thread
     (thunk (let loop ()
              (thnk)
              (loop))))))

(define (kill-worker-thread-pool thp)
  (for-each kill-thread thp))

;;; =============================== LOGGING ====================================

(define (print-log-message port date-str level message)
  (fprintf port "[~a] ~a: ~a\n" date-str level message))

(define (setup-log-thread nrc port)
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
                            (loop))))))))

;;; ============================= CONNECTIONS ==================================

(define (tcp-connection-evt in out eof-return-value)
  (choice-evt (handle-evt (eof-evt in)
                          (λ _
                            eof-return-value))
              (handle-evt (read-bytes-line-evt in 'return-linefeed)
                          (λ (line)
                            (void
                             (when (not (eof-object? line))
                               (enqueue b)))))))

(struct connection-table (ht [evt #:mutable])
  #:property prop:evt (λ (conn-tab)
                        (handle-evt
                         (connection-table-evt conn-tab)
                         (λ (res)
                           (when (and (not (void? res))
                                      (hash-has-key? table res))
                             (connection-pool-remove! conn-pool res))))))

(define (initialize-connection-table)
  (connection-table (make-hasheq) (choice-evt)))

(define (connection-table-generate-event! conn-tab)
  (set-connection-table-evt! conn-tab
                             (handle-evt
                              (apply choice-evt
                                     (map (λ (item)
                                            (define-values (id in out)
                                              (apply values item))
                                            (tcp-connection-evt in out id))))
                              (λ (res)
                                (when (and (not (void? res))
                                           (hash-has-key? table res))
                                  (connection-tab-remove! conn-tab res))))))

(define (connection-table-set! conn-tab in out)
  (define id (gensym))
  (define lst `(,in ,out))
  (hash-set! (connection-table-ht conn-pool) id lst)
  (connection-table-generate-event! conn-tab))

(define (connection-table-remove! conn-tab id)
  (define ht (connection-table-ht conn-tab))
  (define ports (hash-ref ht id))
  (hash-remove! ht id)
  (for-each tcp-abandon-port ports)
  (connection-table-generate-event! conn-tab))

(define (connection-pool-clear! conn-pool)
  (define table (connection-pool-table conn-pool))
  (for-each (curry for-each tcp-abandon-port) (hash-values table))
  (hash-clear! table)
  (set-connection-table-evt! (choice-evt)))

;;; ========================= Server & connections =============================

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (λ ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  (void (thread (λ ()
                  (sleep 10)
                  (custodian-shutdown-all cust)))))

(define (serve/private port-no
                       #:test? [test? #f]
                       #:log-file [log-file (current-output-port)])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust]
                 [current-logger lg]
                 [date-display-format 'iso-8601])
    (define listener (tcp-listen port-no 5 #t))

    (void (thread (λ ()
                    (let loop ()
                      (accept-and-handle listener)
                      (loop)))))

    (cond
      [test?
       (λ () (custodian-shutdown-all cust))]
      [else
       (define shutdown-log-thread
         (let* ([chan (make-channel)]
                [log-loop (log-setup chan log-file)]
                [t (thread log-loop)])
           (λ ()
             (channel-put chan #t)
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

;;; ================================= Main =====================================

(module+ main
  (serve))
