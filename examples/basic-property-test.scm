#!/usr/bin/env guile
!#
;;; basic-property-test.scm -- Basic property-based testing example
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This example demonstrates basic property-based testing as shown
;;; in the README documentation.

;; Set load path to find our modules
(set! %load-path (cons "src" %load-path))

(use-modules (verification property)
             (verification shrink))

(format #t "=== Basic Property Testing Example ===~%~%")

;; Example from README: Basic Property Testing
(format #t "Testing list length property (from README)...~%")
(check-property 
  (lambda ()
    (let ((lst ((list-of integer))))
      (= (length lst) (length (reverse lst))))))

(format #t "~%")

;; Additional property: list reverse involution  
(format #t "Testing list reverse involution...~%")
(check-property
  (lambda ()
    (let ((lst ((list-of integer))))
      (equal? lst (reverse (reverse lst))))))

(format #t "~%")

;; Property with strings
(format #t "Testing string length property...~%")
(check-property
  (lambda ()
    (let ((str ((string-of))))
      (>= (string-length str) 0))))

(format #t "~%=== Property testing example complete ===~%")