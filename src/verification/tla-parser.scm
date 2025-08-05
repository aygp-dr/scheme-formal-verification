;;; tla-parser.scm -- TLA+ specification parser and integration
;;; Part of the Guile Scheme Formal Verification Toolkit
;;;
;;; This module provides basic TLA+ parsing and conversion to Scheme
;;; data structures for verification purposes.

(define-module (verification tla-parser)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:export (parse-tla-file
            parse-tla-string
            tla->scheme
            tla-spec?
            tla-spec-name
            tla-spec-variables
            tla-spec-actions
            make-tla-spec))

;;; TLA+ specification data structure
(define-record-type tla-spec
  (make-tla-spec name variables init actions invariants)
  tla-spec?
  (name tla-spec-name)
  (variables tla-spec-variables) 
  (init tla-spec-init)
  (actions tla-spec-actions)
  (invariants tla-spec-invariants))

;;; Basic TLA+ token patterns
(define tla-patterns
  `((module . "^MODULE\\s+(\\w+)")
    (variable . "^VARIABLE[S]?\\s+(.+)")
    (init . "^Init\\s*==\\s*(.+)")
    (next . "^Next\\s*==\\s*(.+)")
    (action . "^(\\w+)\\s*==\\s*(.+)")
    (invariant . "^INVARIANT\\s+(.+)")
    (comment . "^\\(\\*.*\\*\\)")
    (end-module . "^={4,}")))

(define (tokenize-tla-line line)
  "Tokenize a single line of TLA+ specification"
  (let loop ((patterns tla-patterns))
    (if (null? patterns)
        `(unknown . ,line)
        (let* ((pattern-pair (car patterns))
               (pattern-name (car pattern-pair))
               (pattern-regex (cdr pattern-pair))
               (match-result (string-match pattern-regex line)))
          (if match-result
              `(,pattern-name . ,(match:substring match-result 1))
              (loop (cdr patterns)))))))

(define (parse-tla-string tla-content)
  "Parse TLA+ specification from string content"
  (let ((lines (string-split tla-content #\newline))
        (spec-name #f)
        (variables '())
        (init-expr #f)
        (actions '())
        (invariants '()))
    
    (for-each
      (lambda (line)
        (let ((trimmed (string-trim line)))
          (unless (string=? trimmed "")
            (let ((token (tokenize-tla-line trimmed)))
              (match token
                (('module . name)
                 (set! spec-name name))
                (('variable . vars)
                 (set! variables (append variables (string-split vars #\,))))
                (('init . expr)
                 (set! init-expr expr))
                (('action . expr)
                 (set! actions (cons expr actions)))
                (('invariant . expr)
                 (set! invariants (cons expr invariants)))
                (_ #f))))))
      lines)
    
    (make-tla-spec 
      spec-name
      (map string-trim variables)
      init-expr
      (reverse actions)
      (reverse invariants))))

(define (parse-tla-file filename)
  "Parse TLA+ specification from file"
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (parse-tla-string (get-string-all port))))
      (error "TLA+ file not found: " filename)))

(define (tla->scheme spec)
  "Convert TLA+ specification to executable Scheme code (placeholder)"
  (format #t ";; Generated Scheme code from TLA+ spec: ~a~%" 
          (tla-spec-name spec))
  (format #t ";; Variables: ~a~%" (tla-spec-variables spec))
  (format #t ";; Actions: ~a~%" (tla-spec-actions spec))
  (format #t ";; Full implementation planned for future releases~%")
  #t)

;;; Self-test function
(define (run-tla-self-test)
  "Run self-test for TLA+ parser module"
  (format #t "TLA+ parser module loaded.~%")
  
  ;; Test with simple counter specification
  (let ((sample-tla "MODULE Counter
VARIABLE count

Init == count = 0

Increment == count' = count + 1
Decrement == count' = count - 1

Next == Increment \\/ Decrement

INVARIANT count >= 0"))
    
    (let ((parsed (parse-tla-string sample-tla)))
      (if (tla-spec? parsed)
          (begin
            (format #t "Successfully parsed TLA+ spec: ~a~%" 
                    (tla-spec-name parsed))
            (format #t "Variables: ~a~%" (tla-spec-variables parsed))
            (format #t "Actions: ~a~%" (length (tla-spec-actions parsed))))
          (format #t "Failed to parse sample TLA+ specification~%")))))

;; Run self-test when not in batch mode
(when (not (batch-mode?))
  (run-tla-self-test))