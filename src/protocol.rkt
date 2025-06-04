#lang racket/base

;;; =============================== Imports ====================================

(require racket/list
         racket/match)

;;; =============================== Exports ====================================

(provide (struct-out command)
         (struct-out storage-command)
         (struct-out retrieval-command)
         (struct-out storage-unit)
         bytes->command
         storage-command+data->storage-unit
         key+storage-unit->bytes)

;;; =========================== Command structs ================================

(struct command (name key)
  #:extra-constructor-name make-command
  #:transparent)

(struct storage-command command (flags exptime bytecount noreply)
  #:extra-constructor-name make-storage-command
  #:transparent)

(struct retrieval-command command ()
  #:extra-constructor-name make-retrieval-command
  #:transparent)

;;; =========================== Bytes -> command ===============================

(struct exn:command-parse exn ()
  #:extra-constructor-name make-exn:command-parse
  #:transparent)

(define (raise-command-parse message)
  (raise (make-exn:command-parse message (current-continuation-marks))))

(define (bytes->symbol b)
  (string->symbol (bytes->string/utf-8 b)))

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

(define (bytes->command b)
  (match b
    [(regexp #px"set \\S{1,1024} \\d{1,5} \\d+ \\d+( noreply)?")
     (bytes->storage-command b)]
    [(regexp #px"get \\S{1,1024}")
     (bytes->retrieval-command b)]
    [else (raise-command-parse (format "Invalid syntax: ~a"
                                       b))]))

(define (bytes->storage-command b)
  (define parts (regexp-split #rx" " b))
  (define len (length parts))
  (define name (bytes->symbol (first parts)))
  (define key (bytes->symbol (second parts)))
  (define flags (bytes->number (third parts)))
  (define exptime (bytes->number (fourth parts)))
  (define bytecount (bytes->number (fifth parts)))
  (define noreply (= len 6))
  (make-storage-command name key flags exptime bytecount noreply))

(define (bytes->retrieval-command b)
  (define parts (regexp-split #rx" " b))
  (define name (bytes->symbol (first parts)))
  (define key (bytes->symbol (second parts)))
  (make-retrieval-command name key))

;;; ========================= Storage unit struct ==============================

(struct storage-unit
  (flags bytecount data)
  #:extra-constructor-name make-storage-unit
  #:transparent)

;;; ======================= Command -> storage unit ============================

(define (storage-command+data->storage-unit command data)
  (make-storage-unit (storage-command-flags command)
                     (storage-command-bytecount command)
                     data))

;;; ======================== Storage unit -> bytes =============================

(define (key+storage-unit->bytes key storage-unit)
  (bytes-append (string->bytes/utf-8
                 (format "VALUES ~a ~a ~a"
                         key
                         (storage-unit-flags storage-unit)
                         (storage-unit-bytecount storage-unit)))
                #"\r\n"
                (storage-unit-data storage-unit)
                #"\r\n"))
