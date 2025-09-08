#lang racket/base

;;; =============================== IMPORTS ====================================

(require rackunit
         rackunit/text-ui

         "../src/handler.rkt")

;;; ========================= TEST SUITE: HANDLING =============================

(define handle-suite
  (test-suite

   "Suite for testing whether the handling provides correct behavior according to
client input"

   (test-case
       "Set and get key"

     (let ([out (open-output-bytes)])
       (handle #"set test 0 0 4\r\n" (open-input-bytes #"1234\r\n") out)
       (check-equal? #"OK\r\n"
                     (get-output-bytes out #t))
       (handle #"get test\r\n" (open-input-bytes #"") out)
       (check-equal? #"VALUE test 0 4\r\n1234\r\n"
                     (get-output-bytes out #t))))

   (test-case
       "Invalid command"

     (let ([out (open-output-bytes)])
       (handle #"nonsense test 0 0 4\r\n" (open-input-bytes #"") out)
       (check-equal? #"ERROR\r\n"
                     (get-output-bytes out #t))))

   (test-case
       "Malformed header"

     (let ([out (open-output-bytes)])
       (handle #"set test zero 0 4\r\n" (open-input-bytes #"") out)
       (check-equal? #"CLIENT_ERROR"
                     (subbytes (get-output-bytes out #t) 0 12))))

   (test-case
       "End"

     (let ([out (open-output-bytes)])
       (handle #"get test2\r\n" (open-input-bytes #"") out)
       (check-equal? #"END\r\n"
                     (get-output-bytes out #t))))))

;;; ========================== RUNNING ALL SUITES ==============================

(define SUITES (list handle-suite))

(for ([suite (in-list SUITES)])
  (run-tests suite))
