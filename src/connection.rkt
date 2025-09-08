#lang racket/base

;;; =============================== IMPORTS ====================================

(require racket/port
         racket/tcp
         racket/function)

;;; =============================== EXPORTS ====================================

(provide event-loop)

;;; ======================= SINGLE CONNECTION EVENT ============================

(define (tcp-connection-evt in out proc eof-return-value)
  (choice-evt (handle-evt (eof-evt in)
                          (λ _
                            eof-return-value))
              (handle-evt (read-bytes-line-evt in 'return-linefeed)
                          (λ (line)
                            (when (not (eof-object? line))
                              (void (proc line in out)))))))

;;; =========================== CONNECTION TABLE ===============================

(struct connection-table (ht proc [evt #:mutable])
  #:property prop:evt (λ (conn-tab)
                        (connection-table-evt conn-tab)))

(define (initialize-connection-table proc)
  (connection-table (make-hasheq) proc (choice-evt)))

(define (connection-table-generate-event! conn-tab)
  (define table (connection-table-ht conn-tab))
  (define proc (connection-table-proc conn-tab))
  (set-connection-table-evt! conn-tab
                             (handle-evt
                              (apply choice-evt
                                     (map (λ (item)
                                            (define-values (id in out)
                                              (apply values item))
                                            (tcp-connection-evt in out proc id))
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

;;; ============================== EVENT LOOP ==================================

(define (event-loop listener proc)
  (define conn-tab (initialize-connection-table proc))
  (with-handlers ([exn:break? (λ (e)
                                (connection-table-clear! conn-tab))])
    (let loop ()
      (sync/enable-break
       (handle-evt (tcp-accept-evt listener)
                   (λ (port-lst)
                     (define in (car port-lst))
                     (define out (cadr port-lst))
                     (connection-table-set! conn-tab in out)))
       conn-tab)
      (loop))))
