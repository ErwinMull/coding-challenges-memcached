#lang racket/base

;;; =============================== Imports ====================================

(require rackunit
         rackunit/text-ui

         "../src/protocol.rkt")

(void
 (run-tests
  (test-suite

   "Suite for testing relevant functions from 'protocol.rkt'"

   (test-case
       "bytes->command"
     (check-equal? (bytes->command #"set test 0 0 4")
                   (make-command 'set 'test 0 #f 4 #f))
     ;; TODO: add more tests!
     )

   (test-case
       "command+data->storage-unit"
     (check-equal? (command+data->storage-unit
                    (make-command 'set 'test 0 #f 4 #f)
                    #"hello")
                   (make-storage-unit '0 4 #"hello"))
     ;; TODO: add more tests!
     ))))
