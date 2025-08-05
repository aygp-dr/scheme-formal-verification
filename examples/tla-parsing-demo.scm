#!/usr/bin/env guile
!#
;;; tla-parsing-demo.scm -- TLA+ parsing demonstration
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This example demonstrates TLA+ specification parsing as shown
;;; in the README documentation.

;; Set load path to find our modules
(set! %load-path (cons "src" %load-path))

(use-modules (verification tla-parser))

(format #t "=== TLA+ Specification Parsing Example ===~%~%")

;; Example from README: TLA+ Specification Parsing
(format #t "Parsing Counter specification...~%")

;; Create sample Counter.tla content (as would be in specs/Counter.tla)
(define counter-tla-content "
MODULE Counter
VARIABLE count

Init == count = 0

Increment == count' = count + 1
Decrement == count' = count - 1

Next == Increment \\/ Decrement

INVARIANT count >= 0
")

;; Parse TLA+ specification
(define counter-spec 
  (parse-tla-string counter-tla-content))

(format #t "Successfully parsed TLA+ specification!~%")
(format #t "Module name: ~a~%" (tla-spec-name counter-spec))
(format #t "Variables: ~a~%" (tla-spec-variables counter-spec))
(format #t "Number of actions: ~a~%" (length (tla-spec-actions counter-spec)))

(format #t "~%Generating Scheme code...~%")
;; Generate Scheme implementation (placeholder)
(tla->scheme counter-spec)

(format #t "~%=== TLA+ parsing example complete ===~%")