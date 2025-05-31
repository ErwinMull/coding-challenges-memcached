#lang racket/base

;;; =============================== Imports ====================================

(require rackunit
         rackunit/text-ui

         racket/tcp

         "../src/server.rkt")

;;; =========================== Global variables ===============================

(define PORTNO 11211)

;;; ============ Test suite: check if server is running on port ================

(define port-suite
  (test-suite

   "Suite for testing whether the server runs on a given port on localhost"

   (test-case
       "Connect to port"

     (let ([result (with-handlers ([exn:fail:network? (λ (exn) #f)])
                     (define-values (in out) (tcp-connect "localhost" PORTNO))
                     (close-input-port in)
                     (close-output-port out)
                     #t)])
       (unless result
         (with-check-info (['message (string-info
                                      (format "Server not running on port ~a!"
                                              PORTNO))])
           (fail-check)))))))

;;; ========================== Running all suites ==============================

(define SUITES (list port-suite))

(define (run-suites-with-server port-no suites)
  (define stop (serve port-no))
  (for ([suite (in-list suites)])
    (run-tests suite))
  (stop))

(run-suites-with-server PORTNO SUITES)
