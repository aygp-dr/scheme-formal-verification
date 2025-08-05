#!/usr/bin/env guile
!#
;;; shrinking-demo.scm -- Test case shrinking demonstration
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This example demonstrates the shrinking capabilities for
;;; finding minimal failing test cases.

;; Set load path to find our modules  
(set! %load-path (cons "src" %load-path))

(use-modules (verification property)
             (verification shrink)
             (srfi srfi-1))

(format #t "=== Test Case Shrinking Demonstration ===~%~%")

;; Demonstrate integer shrinking
(format #t "Integer shrinking examples:~%")
(let ((examples '(42 -17 0 100)))
  (for-each (lambda (n)
              (let ((shrunk (shrink-integer n)))
                (format #t "  ~a -> ~a~%" n 
                        (if (> (length shrunk) 5)
                            (append (take shrunk 5) '(...))
                            shrunk))))
            examples))

(format #t "~%List shrinking examples:~%")
(let ((examples '((1 2 3 4 5) (a b c) () (x))))
  (for-each (lambda (lst)
              (let ((shrunk (shrink-list lst)))
                (format #t "  ~a -> ~a candidates~%" lst (length shrunk))))
            examples))

(format #t "~%String shrinking examples:~%")
(let ((examples '("hello" "world" "a" "")))
  (for-each (lambda (str)
              (let ((shrunk (shrink-string str)))
                (format #t "  \"~a\" -> ~a candidates~%" str (length shrunk))))
            examples))

;; Demonstrate shrinking with a property that fails for large numbers
(format #t "~%Property that fails for numbers > 50:~%")
(define-property (small-number-property n)
  (<= n 50))

;; This would normally be done by the property testing framework
;; Here we demonstrate manually
(format #t "Original failing input: 87~%")
(let ((minimal (with-shrinking 
                 (lambda (n) (small-number-property n))
                 87)))
  (format #t "Found minimal failing case: ~a~%" minimal))

(format #t "~%=== Shrinking demonstration complete ===~%")