#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2016, 2017 Göran Weinholt <goran@weinholt.se>

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
        (machine-code tests check)
        (machine-code disassembler arm-a64))

(define (d instruction)
  (let ((bv (make-bytevector 4)))
    (bytevector-u32-set! bv 0 instruction (endianness little))
    (let ((p (open-bytevector-input-port bv)))
      (guard (con
              ((invalid-opcode? con)
               `(bad: ,(condition-message con)
                      ,@(condition-irritants con))))
        (let ((instr (get-instruction p #f #x4100)))
          (assert (port-eof? p))
          instr)))))

(check (d #x913FE210) => '(add x16 x16 #xff8))

(check (d #x54000040) => '(b.eq #x4108))

(check (d #xa9bf7bfd) => '(stp x29 x30 (mempre+ sp -16))) ;stp x29, x30, [sp, #-16]!
(check (d #xa9001c06) => '(stp x6 x7 (mem+ x0)))  ;stp x6, x7, [x0]
(check (d #xa900cc01) => '(stp x1 x19 (mem+ x0 8))) ;stp x1, x19, [x0, #8]
(check (d #xa8c17bfd) => '(ldp x29 x30 (mempost+ sp 16))) ;ldp x29, x30, [sp], #16

(check (d #x721E785F) => '(tst w2 #xfffffffd))
(check (d #x72001c1f) => '(tst w0 #xff))
(check (d #xf240057f) => '(tst x11 #x3))
(check (d #xf278001f) => '(tst x0 #x100))
(check (d #xf279001f) => '(tst x0 #x80))
(check (d #xf27a001f) => '(tst x0 #x40))
(check (d #xf27b001f) => '(tst x0 #x20))
(check (d #xf27c001f) => '(tst x0 #x10))
(check (d #xf27d001f) => '(tst x0 #x8))
(check (d #xf27e001f) => '(tst x0 #x4))
(check (d #xf27f001f) => '(tst x0 #x2))
(check (d #xf240001f) => '(tst x0 #x1))
(check (d #x721b783f) => '(tst w1 #xffffffef))
(check (d #xf240097f) => '(tst x11 #x7))
(check (d #xf251009f) => '(tst x4 #x800000000000))

(check (d #x90000141) => '(adrp x1 #x2c000))
(check (d #x10000061) => '(adr x1 #x410c))
(check (d #xb4000060) => '(cbz x0 #x410c))
(check (d #x1a9f17e2) => '(cset w2 eq))
(check (d #x8b02039c) => '(add x28 x28 x2))

;; (check (d #x9b017f39) => '(mul x25 x25 x1))

(check-report)
(exit (if (check-passed? 26) 0 1))
