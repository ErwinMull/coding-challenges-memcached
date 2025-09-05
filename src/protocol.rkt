#lang racket/base

;;; =============================== IMPORTS ====================================

(require racket/string
         racket/match

         "hashtable.rkt")

;;; ================================ export ====================================

(provide handle-memcached-command)

;;; ================================ STRUCT ====================================

(struct storage-command (name key flags exptime bytes-count)
  #:transparent)
(struct retrieval-command (name key)
  #:transparent)

;;; =============================== PARSING ====================================

(define (parse-storage-header byt)
  (define str (bytes->string/utf-8 byt))
  (define header-lst (string-split str))
  (define command-name (string->symbol (car header-lst)))
  (define key (string->symbol (cadr header-lst)))
  (define flags (string->number (caddr header-lst)))
  (define exptime (string->number (cadddr header-lst)))
  (define bytes-count (string->number (car (cddddr header-lst))))
  (storage-command command-name key flags exptime bytes-count))

(define (parse-retrieval-header byt)
  (define str (bytes->string/utf-8 byt))
  (define header-lst (string-split str))
  (define command-name (string->symbol (car header-lst)))
  (define key (string->symbol (cadr header-lst)))
  (retrieval-command command-name key))

;;; =============================== HANDLER ====================================

(define STORAGE-COMMAND-REGEXP #px"^(set)")
(define RETRIEVAL-COMMAND-REGEXP #px"^(get)")

(define (handle-memcached-command header in out)
  (match header
    [(regexp STORAGE-COMMAND-REGEXP) (handle-storage-command header in out)]
    [(regexp RETRIEVAL-COMMAND-REGEXP) (handle-retrieval-command header in out)]
    [_ (raise "TODO")]))

(define (handle-storage-command header in out)
  (define st (parse-storage-header header))
  (define data (read-bytes (storage-command-bytes-count st) in))
  (mem-set (storage-command-key st) (storage-command-flags st) data)
  (write-bytes #"OK" out)
  (flush-output out))

(define (handle-retrieval-command header in out)
  (define re (parse-retrieval-header header))
  (define-values (flags data) (mem-get (retrieval-command-key re)))
  (define bytes-count (bytes-length data))
  (fprintf out
           "VALUE ~a ~a ~a\r\n~a"
           (retrieval-command-key re)
           flags
           bytes-count
           data)
  (flush-output out))
