#lang racket/base

;;; =============================== EXPORTS ====================================

(provide mem-set
         mem-get)

;;; =============================== GLOBALS ====================================

(define MEM-SEM (make-semaphore 1))
(define MEM-HT (make-hasheq))

;;; ================================= ITEM =====================================

(struct mem-item (flags data)
  #:transparent)

;;; ============================== ACCESSING ===================================

(define (mem-access th)
  (call-with-semaphore MEM-SEM
                       th))

;;; =============================== COMMANDS ===================================

(define (mem-set key flags data)
  (mem-access (λ ()
                (hash-set! MEM-HT key (mem-item flags data)))))

(define (mem-get key)
  (mem-access (λ ()
                (define tmp (hash-ref MEM-HT key))
                (values (mem-item-flags tmp) (mem-item-data tmp)))))
