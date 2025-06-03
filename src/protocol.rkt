#lang racket/base

;;; =============================== Imports ====================================

(require racket/list)

;;; =============================== Exports ====================================

(provide (struct-out command)
         (struct-out storage-unit)
         bytes->command
         command+data->storage-unit
         key+storage-unit->bytes)

;;; ============================== Constants ===================================

(define VALID-COMMANDS (list 'set 'get))

;;; =============================== Structs ====================================

(struct command
  (name key flags exptime byte-count noreply)
  #:extra-constructor-name make-command
  #:transparent)

(struct storage-unit
  (flags byte-count data-block)
  #:extra-constructor-name make-storage-unit
  #:transparent)

;;; =========================== Bytes -> command ===============================

(define (bytes->symbol b)
  (string->symbol (bytes->string/utf-8 b)))

(define (bytes->number b)
  (string->number (bytes->string/utf-8 b)))

(define (bytes->command b)
  (define parts (regexp-split #rx" " b))
  (define len (length parts))
  (when (not (or (= len 5) (= len 6)))
    (error "TODO: custom exception? 1"))
  (define name (bytes->symbol (first parts)))
  (when (not (member name VALID-COMMANDS))
    (error "TODO: custom exception? 2"))
  (define key (bytes->symbol (second parts)))
  (define flags (bytes->number (third parts)))
  (define exptime #f)
  (define byte-count (bytes->number (fifth parts)))
  (define noreply (and (= len 6)
                       (or (bytes=? (sixth parts) #"noreply")
                           (error "TODO: custom exception? 3"))))
  (make-command name key flags exptime byte-count noreply))

;;; ======================= Command -> storage unit ============================

(define (command+data->storage-unit command data)
  (make-storage-unit (command-flags command)
                     (command-byte-count command)
                     data))

;;; ======================== Storage unit -> bytes =============================

(define (key+storage-unit->bytes key storage-unit)
  (bytes-append (string->bytes/utf-8
                 (format "VALUES ~a ~a ~a"
                         key
                         (storage-unit-flags storage-unit)
                         (storage-unit-byte-count storage-unit)))
                #"\r\n"
                 (storage-unit-data-block storage-unit)))
