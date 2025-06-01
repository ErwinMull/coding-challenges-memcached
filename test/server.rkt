#lang racket/base

;;; =============================== Imports ====================================

(require rackunit
         rackunit/text-ui

         racket/tcp

         "../src/server.rkt")

;;; =========================== Global variables ===============================

(define PORTNO 11211)
(define custodian #f)

;;; ============ Test suite: check if server is running on port ================

(define port-suite
  (test-suite

   "Suite for testing whether the server runs on a given port on localhost"

   #:before (λ ()
              (set! custodian (make-custodian))
              (parameterize ([current-custodian custodian])
                (thread (λ () (serve PORTNO)))
                ;; TODO: This is bad but works for now. Have to find a better
                ;; solution to wait for startup
                (sleep 1)))
   #:after (λ ()
             (custodian-shutdown-all custodian))

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

(for ([suite (in-list SUITES)])
  (run-tests suite))
