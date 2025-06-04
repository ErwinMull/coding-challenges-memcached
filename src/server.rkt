#lang racket/base

;;; =============================== Imports ====================================

(require racket/tcp
         racket/date
         racket/port

         "key-val-store.rkt"
         "protocol.rkt")

;;; =============================== Exports ====================================

(provide serve/test)

;;; ============================== Constants ===================================

(define timeout-secs (make-parameter #f))

;;; ============================== Exceptions ==================================

(struct exn:timeout exn ()
  #:extra-constructor-name make-exn:timeout
  #:transparent)

(define (raise-timeout message)
  (raise (make-exn:timeout message (current-continuation-marks))))

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

(define (drain-input-port in)
  (define buf (make-bytes (expt 2 14)))
  (let loop ()
    (define try-read (read-bytes-avail!* buf in))
    (unless (or (eof-object? try-read)
                (and (number? try-read) (= try-read 0)))
      (loop))))

(define (make-timeout-alarm secs)
  (alarm-evt (+ (current-inexact-milliseconds) (* secs 1000))))

(define (sync/timeout-alarm event)
  (sync event
        (if (timeout-secs)
            (wrap-evt (make-timeout-alarm (timeout-secs))
                      (λ (v) (raise-timeout "IDLE_TIMEOUT")))
            never-evt)))

(define (read-bytes-line/timeout [in (current-input-port)]
                                 [mode 'return-linefeed])
  (sync/timeout-alarm (handle-evt (read-bytes-line-evt in mode)
                                  (λ (v) v))))

(define (read-bytes/timeout amt [in (current-input-port)])
  (sync/timeout-alarm (handle-evt (read-bytes-evt amt in)
                                  (λ (v) v))))

(define (write-bytes/flush b out)
  (write-bytes b out)
  (flush-output out))

(define (handle in out)
  (let loop ()
    (with-handlers ([exn:timeout?
                     (λ (e)
                       (log-info "Timeout")
                       (write-bytes/flush (bytes-append
                                           (string->bytes/utf-8
                                            (exn-message e))
                                           #"\r\n")
                                          out))]
                    [exn:fail:network?
                     (λ (e)
                       (void))]
                    [exn:command-parse?
                     (λ (e)
                       (drain-input-port in)
                       (log-error "COMMAND PARSE")
                       (loop))])
      (define first-line (read-bytes-line/timeout in))
      (unless (eof-object? first-line)
        (define com (bytes->command first-line))
        (define data (and (storage-command? com)
                          (read-bytes/timeout (storage-command-bytecount com)
                                              in)))

        (log-info (format "Received: ~a ~a"
                          (command-name com)
                          (command-key com)))
        (define result
          (case (command-name com)
            [(set) (handle-set com data)]
            [(get) (handle-get com)]))

        (write-bytes/flush result out)
        (drain-input-port in)
        (loop)))))

;;; ========================= Server & connections =============================

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (log-info "Connection opened")
  (void
   (thread (λ ()
             (handle in out)
             (close-input-port in)
             (close-output-port out)
             (log-info "Connection terminated")))))

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
                      (loop))))
          (init-key-val-thread))

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
