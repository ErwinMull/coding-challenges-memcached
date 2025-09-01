#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp
         racket/date)

;;; =============================== Exports ====================================

(provide serve/test)

;;; ============================ WORKER THREADS ================================

(define queue-channel (make-channel))

(define (enqueue val)
  (channel-put queue-channel val))

(define (dequeue)
  (channel-get queue-channel))

(define (make-worker-thread-pool n thnk)
  (for/vector ([_ (in-range n)])
    (thread thnk)))

(define (kill-worker-thread-pool thp)
  (vector-map kill-thread thp))

;;; =============================== Logging ====================================

(define lg (make-logger))
(define nrc (make-log-receiver lg 'info))

(define exit-log-loop-channel (make-channel))

(define (log-setup exit-channel
                   log-port)
  (λ ()
    (let loop ()
      (sync
       (handle-evt nrc
                   (λ (v)
                     (fprintf log-port
                              "[~a] ~a: ~a\n"
                              (date->string (current-date) #t)
                              (vector-ref v 0)
                              (vector-ref v 1))
                     (loop)))
       exit-channel))))

;;; =========================== Request handling ===============================

(define (handle in out)
  (let* ([buf (make-bytes (expt 2 16) 0)])
    (define nread (read-bytes-avail! buf in))
    (define message (subbytes buf 0 nread))
    (log-info (format "Received ~a bytes: ~v"
                      nread
                      message))
    (write-bytes message out)))

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
