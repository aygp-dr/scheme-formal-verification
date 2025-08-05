;;; shrink.scm -- Test case shrinking for property-based testing
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This module provides shrinking capabilities to find minimal
;;; failing test cases when properties fail.

(define-module (verification shrink)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (shrink-integer
            shrink-list
            shrink-string
            shrink-value
            with-shrinking))

;;; Basic shrinking strategies
(define (shrink-integer n)
  "Generate smaller integers by halving towards zero"
  (cond
    ((= n 0) '())
    ((> n 0) (let loop ((candidates '()) (current n))
               (if (<= current 0)
                   (reverse candidates)
                   (loop (cons current candidates)
                         (quotient current 2)))))
    (else (map - (shrink-integer (- n))))))

(define (shrink-list lst)
  "Generate smaller lists by removing elements"
  (if (null? lst)
      '()
      (append
        ;; Remove elements from front
        (map (lambda (n) (drop lst n)) 
             (iota (min 5 (length lst))))
        ;; Remove elements from back  
        (map (lambda (n) (take lst n))
             (reverse (iota (min 5 (length lst)))))
        ;; Remove single elements
        (filter-map (lambda (i)
                      (let ((before (take lst i))
                            (after (drop lst (+ i 1))))
                        (if (< i (length lst))
                            (append before after)
                            #f)))
                    (iota (length lst))))))

(define (shrink-string str)
  "Generate smaller strings by removing characters"
  (let ((char-list (string->list str)))
    (map list->string (shrink-list char-list))))

(define (shrink-value value)
  "Generic shrinking dispatcher based on value type"
  (cond
    ((integer? value) (shrink-integer value))
    ((list? value) (shrink-list value))
    ((string? value) (shrink-string value))
    (else '())))

;;; Shrinking framework
(define (with-shrinking property original-input)
  "Apply shrinking to find minimal failing input"
  (define (shrink-loop input shrinks-remaining)
    (if (zero? shrinks-remaining)
        input
        (let ((candidates (shrink-value input)))
          (let loop ((remaining candidates))
            (if (null? remaining)
                input  ; No smaller failing case found
                (let ((candidate (car remaining)))
                  (catch #t
                    (lambda ()
                      (if (not (property candidate))
                          ;; Found smaller failing case
                          (shrink-loop candidate (- shrinks-remaining 1))
                          ;; This candidate passes, try next
                          (loop (cdr remaining))))
                    (lambda (key . args)
                      ;; Exception counts as failure
                      (shrink-loop candidate (- shrinks-remaining 1))))))))))
  
  (format #t "Shrinking failing input...~%")
  (let ((minimal-input (shrink-loop original-input 100)))
    (format #t "Minimal failing input: ~a~%" minimal-input)
    minimal-input))

;;; Example usage and self-test
(when (not (batch-mode?))
  (format #t "Shrinking module loaded.~%")
  
  ;; Test integer shrinking
  (let ((shrunk (shrink-integer 42)))
    (format #t "Shrinking 42: ~a~%" (take shrunk (min 5 (length shrunk)))))
  
  ;; Test list shrinking  
  (let ((shrunk (shrink-list '(1 2 3 4 5))))
    (format #t "Shrinking '(1 2 3 4 5): ~a candidates~%" (length shrunk))))