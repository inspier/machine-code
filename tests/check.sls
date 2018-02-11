;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (machine-code tests check)
  (export
    check check-report check-passed?)
  (import
    (rnrs (6)))

  (define checks-passed 0)
  (define checks-ok? #t)

  (define (check-it test-expr test expected-expr expected)
    (let ((test-result (test))
          (expected-result (expected)))
      (cond ((equal? test-result expected-result)
             (set! checks-passed (+ checks-passed 1)))
            (else
             (newline)
             (write test-expr)
             (display "\n=>\n")
             (write test-result)
             (newline)
             (display "; Wrong! Expected:\n")
             (write expected-result)
             (newline)
             (set! checks-ok? #f)))))

  (define (check-report)
    (display "\n; ")
    (display checks-passed)
    (display " checks passed\n")
    (unless checks-ok?
      (display "; There are errors\n"))
    (flush-output-port (current-output-port)))

  (define (check-passed? n)
    (= checks-passed n))

  (define-syntax check
    (syntax-rules (=>)
      ((_ test => expected)
       (check-it 'test (lambda () test) 'expected (lambda () expected))))))
