#lang racket/base

;;; =============================== IMPORTS ====================================

(require rackunit
         rackunit/text-ui

         racket/tcp

         "../src/server.rkt")

;;; ============================== CONSTANTS ===================================

(define PORT 11211)
(define MEMCACHED-THREAD #f)

;;; ============ TEST SUITE: CHECK IF SERVER IS RUNNING ON PORT ================

(define port-suite
  (test-suite

   "Suite for testing whether the server runs on a given port on localhost"

   #:before (λ ()
              (set! MEMCACHED-THREAD (thread (λ ()
                                               (serve PORT))))
              (sleep 1))
   #:after (λ ()
             (break-thread MEMCACHED-THREAD)
             (set! MEMCACHED-THREAD #f))

   (test-case
       "Connect to port"

     (let ([result (with-handlers ([exn:fail:network? (λ (exn) #f)])
                     (define-values (in out) (tcp-connect "localhost" PORT))
                     (close-input-port in)
                     (close-output-port out)
                     #t)])
       (unless result
         (with-check-info (['message (string-info
                                      (format "Server not running on port ~a!"
                                              PORT))])
           (fail-check)))))))

;;; ========================== RUNNING ALL SUITES ==============================

(define SUITES (list port-suite))

(for ([suite (in-list SUITES)])
  (run-tests suite))
