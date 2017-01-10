#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2016, 2017 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(import (rnrs)
        (machine-code format elf)
        (machine-code tests check))

(check (let ((table (make-string-table '(""))))
         (string-table-size table))
       => 1)

(check (let ((table (make-string-table '(""))))
         (string-table-list-index table ""))
       => 0)

(check (let ((table (make-string-table '(""))))
         (string-table-byte-index table ""))
       => 0)

(check (let ((table (make-string-table '("" "foo"))))
         (string-table-list-index table "foo"))
       => 1)

(check (let ((table (make-string-table '("" "foo"))))
         (string-table-byte-index table "foo"))
       => 1)

(check (let ((table (make-string-table '("" "foo" "bar"))))
         (string-table-list-index table "bar"))
       => 2)

(check (let ((table (make-string-table '("" "foo" "bar"))))
         (string-table-byte-index table "bar"))
       => 5)

(check (let ((table (make-string-table '(""))))
         (string-table-bytes table))
       => (string->utf8 "\x0;"))

(check (let ((table (make-string-table '("a"))))
         (string-table-bytes table))
       => (string->utf8 "a\x0;"))

(check (let ((table (make-string-table '("" "b"))))
         (string-table-bytes table))
       => (string->utf8 "\x0;b\x0;"))

(check-report)

(exit (if (check-passed? 10) 0 1))
