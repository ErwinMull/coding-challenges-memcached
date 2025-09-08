#lang racket/base

;;; =============================== EXPORTS ====================================

(provide enqueue
         make-worker-thread-pool
         kill-worker-thread-pool)

;;; ================================ QUEUE =====================================

(define QUEUE (make-channel))

(define (enqueue val)
  (channel-put QUEUE val))

(define (dequeue)
  (channel-get QUEUE))

;;; ============================ WORKER THREADS ================================

(define (make-worker-thread-pool n proc)
  (for/list ([_ (in-range n)])
    (thread
     (λ () (let loop ()
              (proc (dequeue))
              (loop))))))

(define (kill-worker-thread-pool thp)
  (for-each kill-thread thp))
