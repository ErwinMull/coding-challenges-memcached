#lang racket/base

;;; =============================== IMPORTS ====================================

(require rackunit
         rackunit/text-ui

         racket/tcp

         "../src/server.rkt")

;;; ============================== CONSTANTS ===================================

(define PORT 11211)
(define MEMCACHED-THREAD #f)

;;; ================================ UTILS =====================================

(define (tcp-can-connect? hostname port)
  (with-handlers ([exn:fail:network? (λ (e) #f)])
    (define-values (in out) (tcp-connect hostname port))
    (tcp-abandon-port in)
    (tcp-abandon-port out)
    #t))

;;; ============ TEST SUITE: CHECK IF SERVER IS RUNNING ON PORT ================

(define port-suite
  (test-suite

   "Suite for testing whether the server runs on a given port on localhost"

   #:before (λ ()
              (set! MEMCACHED-THREAD (thread (λ ()
                                               (serve PORT)))))
   #:after (λ ()
             (break-thread MEMCACHED-THREAD)
             (set! MEMCACHED-THREAD #f))

   (test-case
       "Connect to port"

     (let loop ([n 0])
       (cond
         [(= n 5)
          (with-check-info (['message (string-info
                                       (format "Server not running on port ~a!"
                                               PORT))])
            (fail-check))]
         [(tcp-can-connect? "localhost" PORT) #t]
         [else
          (sleep 1)
          (loop (add1 n))])))))

;;; ========================== RUNNING ALL SUITES ==============================

(define SUITES (list port-suite))

(for ([suite (in-list SUITES)])
  (run-tests suite))
