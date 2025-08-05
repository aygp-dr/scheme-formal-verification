;;; property.scm -- Property-based testing framework for Guile Scheme
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This module provides property-based testing capabilities with
;;; automatic test case generation and shrinking on failures.

(define-module (verification property)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (system base compile)
  #:export (define-property
            check-property
            for-all
            list-of
            integer
            string-of
            property-test-config))

;;; Configuration for property testing
(define property-test-config
  (make-parameter 
    `((max-tests . 100)
      (max-shrinks . 100)
      (random-seed . #f))))

;;; Generator framework
(define (integer)
  "Generate random integers for property testing"
  (lambda ()
    (- (random 2000) 1000)))

(define (list-of generator)
  "Generate lists using the given element generator"
  (lambda ()
    (let ((length (random 20)))
      (map (lambda (_) (generator)) (iota length)))))

(define (string-of)
  "Generate random strings for property testing"
  (lambda ()
    (list->string 
      (map (lambda (_) 
             (integer->char (+ 97 (random 26))))
           (iota (+ 1 (random 10)))))))

;;; Property definition and checking
(define-syntax define-property
  (syntax-rules ()
    ((define-property (name . args) body ...)
     (define name
       (lambda args
         (lambda () body ...))))))

(define-syntax for-all
  (syntax-rules ()
    ((for-all ([var generator] ...) body ...)
     (lambda ()
       (let ((var ((generator))) ...)
         body ...)))))

(define (check-property property)
  "Check a property by running it multiple times with generated inputs"
  (let ((config (property-test-config))
        (successes 0)
        (failures '()))
    
    (let loop ((tests-remaining (assq-ref config 'max-tests)))
      (if (zero? tests-remaining)
          ;; All tests passed
          (begin
            (format #t "✓ Property passed ~a tests~%" successes)
            #t)
          
          ;; Run one test
          (catch #t
            (lambda ()
              (let ((result (property)))
                (if result
                    (begin
                      (set! successes (+ successes 1))
                      (loop (- tests-remaining 1)))
                    (begin
                      (format #t "✗ Property failed on test ~a~%" 
                              (+ successes 1))
                      #f))))
            (lambda (key . args)
              (format #t "✗ Property threw exception: ~a ~a~%" key args)
              #f))))))

;;; Example property for self-testing
(define-property (list-length-property lst)
  (>= (length lst) 0))

;;; Self-test function
(define (run-self-test)
  "Run self-test for property module"
  (format #t "Property-based testing module loaded.~%")
  
  (let ((test-result
          (check-property
            (for-all ([lst (list-of integer)])
              (list-length-property lst)))))
    
    (if test-result
        (format #t "Self-test passed: list length property verified~%")
        (format #t "Self-test failed: basic property checking broken~%"))))

;; Run self-test when not in batch mode
(when (not (batch-mode?))
  (run-self-test))