#lang racket/base

(require "protocol.rkt")

(provide handle-get
         handle-set
         init-key-val-thread)

(define set-channel (make-channel))
(define get-channel (make-channel))

(define (handle-set com data)
  (define key (command-key com))
  (define storage-unit (storage-command+data->storage-unit com data))
  (channel-put set-channel (vector key storage-unit))
  (void (channel-get set-channel))
  (if (storage-command-noreply com)
      #""
      #"STORED\r\n"))

(define (handle-get com)
  (define key (command-key com))
  (channel-put get-channel key)
  (define res (channel-get get-channel))
  (if res
      (key+storage-unit->bytes key res)
      #"NOT FOUND\r\n"))

(define (init-key-val-thread)
  (thread
   (λ ()
     (let loop ([ht (hasheq)])
       (void
        (sync (handle-evt set-channel
                            (λ (v)
                              (define key (vector-ref v 0))
                              (define storage-unit (vector-ref v 1))
                              (define new-ht (hash-set ht key storage-unit))
                              (channel-put set-channel (void))
                              (loop new-ht)))
              (handle-evt get-channel
                            (λ (key)
                              (channel-put get-channel (hash-ref ht key #f))
                              (loop ht)))))))))
