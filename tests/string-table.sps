#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
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
