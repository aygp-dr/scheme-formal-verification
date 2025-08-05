#!/usr/bin/env guile
!#
;; Dependency Check for Guile Scheme Formal Verification Toolkit
;; Confirms Guile 3.0+ and required modules are available

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (srfi srfi-26)
             (system base compile))

(define (check-guile-version)
  "Check that we're running Guile 2.2+ or later"
  (let ((version (version)))
    (format #t "✓ Guile version: ~a~%" version)
    (cond 
      ((string-prefix? "3." version)
       (format #t "✓ Guile 3.x detected - fully compatible~%"))
      ((string-prefix? "2.2." version)
       (format #t "✓ Guile 2.2.x detected - compatible~%"))
      (else
       (format #t "⚠ Warning: Expected Guile 2.2+ or 3.x, got ~a~%" version)))))

(define (check-module module-name)
  "Check if a module can be loaded"
  (catch #t
    (lambda ()
      (resolve-module module-name)
      (format #t "✓ Module ~a: available~%" module-name)
      #t)
    (lambda (key . args)
      (format #t "✗ Module ~a: missing (~a)~%" module-name key)
      #f)))

(define required-modules
  '((ice-9 match)
    (ice-9 format)
    (ice-9 pretty-print)
    (ice-9 regex)
    (srfi srfi-1)
    (srfi srfi-26)
    (srfi srfi-64)
    (system base compile)))

(define (main)
  (format #t "=== Guile Scheme Formal Verification Toolkit - Dependency Check ===~%~%")
  
  ;; Check Guile version
  (check-guile-version)
  (newline)
  
  ;; Check required modules
  (format #t "Required modules:~%")
  (let ((missing (filter (lambda (mod) (not (check-module mod))) required-modules)))
    (if (null? missing)
        (format #t "✓ All required modules available~%")
        (format #t "✗ Missing required modules: ~a~%" missing)))
  
  (newline)
  (format #t "=== Dependency check complete ===~%"))

;; Run the check
(main)