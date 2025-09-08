#lang racket/base

;;; =============================== IMPORTS ====================================

(require racket/string

         "hashtable.rkt")

;;; =============================== EXPORTS ====================================

(provide pack-header&ports
         unpack-header&ports
         handle)

;;; ============================== PACKAGING ===================================

;; These two functions are meant for simplifying the interaction with the queue
;; of the worker threads.

(define (pack-header&ports header in out)
  (vector header in out))

(define (unpack-header&ports vec)
  (values (vector-ref vec 0) (vector-ref vec 1) (vector-ref vec 2)))

;;; ================================ ERRORS ====================================

(struct exn:fail:client-error exn ()
  #:extra-constructor-name make-exn:fail:client-error
  #:transparent)
(struct exn:fail:server-error exn ()
  #:extra-constructor-name make-exn:fail:server-error
  #:transparent)

(define (raise-exn:fail:client-error message)
  (raise (make-exn:fail:client-error message (current-continuation-marks))))

(define (raise-exn:fail:server-error message)
  (raise (make-exn:fail:server-error message (current-continuation-marks))))

;;; =============================== HANDLING ===================================

(define (header->name&key&arguments header)
  (define tmp (string-split (bytes->string/utf-8 header)))
  (unless (> (length tmp) 1)
    (raise-exn:fail:client-error "TODO"))
  (values (string->symbol (car tmp))
          (string->symbol (cadr tmp))
          (cddr tmp)))

(define (arguments->flags/exptime/bytes-count/noreply arguments)
  (define flags (string->number (car arguments)))
  (define exptime (string->number (cadr arguments)))
  (define bytes-count (string->number (caddr arguments)))
  (define noreply (and (not (null? (cdddr arguments)))
                       (cadddr arguments)))
  (unless (and (number? flags)
               (number? exptime)
               (number? bytes-count)
               (or (not noreply)
                   (string=? noreply "noreply")))
    (raise-exn:fail:client-error "TODO"))
  (values flags exptime bytes-count noreply))

(define (handle-storage-command name key arguments in out)
  (define-values (flags exptime bytes-count noreply)
    (arguments->flags/exptime/bytes-count/noreply arguments))
  (define data (read-bytes bytes-count in))
  (when (bytes=? #"\r\n" (peek-bytes 2 0 in))
    (read-bytes 2 in))
  (mem-set key flags data)
  (unless noreply
    (reply out "OK")))

(define (handle-retrieval-command name key arguments in out)
  (define-values (flags data) (mem-get key))
  (if (and flags data)
      (reply out "VALUE ~a ~a ~a\r\n~a" key flags (bytes-length data) data)
      (reply out "END")))

(define (handle-error name key arguments in out)
  (reply out "ERROR"))

(define (name->handler name)
  (case name
    [(set) handle-storage-command]
    [(get) handle-retrieval-command]
    [else handle-error]))

(define (handle header in out)
  (with-handlers ([exn:fail:client-error? (λ (e)
                                            (reply out
                                                   "CLIENT_ERROR ~a"
                                                   (exn-message e)))]
                  [exn:fail:server-error? (λ (e)
                                            (reply out
                                                   "SERVER_ERROR ~a"
                                                   (exn-message e)))])
    (define-values (name key arguments) (header->name&key&arguments header))
    ((name->handler name) name key arguments in out)))

(define (reply out form . args)
  (apply fprintf out (string-append form "\r\n") args)
  (flush-output out))
