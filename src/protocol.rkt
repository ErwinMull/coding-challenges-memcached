#lang racket/base

(struct protocol
  (command-name key flags exptime byte-count noreply data-block)
  #:exra-constructor-name make-protocol
  #:transparent)

(struct storage-unit
  (flags bytecount data-block)
  #:extra-constructor-name make-storage-unit
  #:transparent)
