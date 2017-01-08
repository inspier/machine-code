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
(check (d #x7f7fffff) => '(fcvtzu d31 d31 1))
(check (d #x295a17aa) => '(ldp w10 w5 (mem+ x29 #xD0)))
(check (d #x38401c01) => '(ldrb w1 (mempre+ x0 1)))
(check (d #xdac01001) => '(clz x1 x0))
(check (d #x1e2e1001) => '(fmov s1 1.0))
(check (d #x1e220800) => '(fmul s0 s0 s2))
(check (d #xfa401844) => '(ccmp x2 0 #x4 ne))
(check (d #x93cbd086) => '(extr x6 x4 x11 52))

;; (check (d #x9b017f39) => '(mul x25 x25 x1))

;; Complete check of the floating-point constant decoding
(check (do ((fp-constants
             '#(#(2.0   4.0  8.0  16.0 0.125     0.25     0.5     1.0)
                #(2.125 4.25 8.5  17.0 0.1328125 0.265625 0.53125 1.0625)
                #(2.25  4.5  9.0  18.0 0.140625  0.28125  0.5625  1.125)
                #(2.375 4.75 9.5  19.0 0.1484375 0.296875 0.59375 1.1875)
                #(2.5   5.0  10.0 20.0 0.15625   0.3125   0.625   1.25)
                #(2.625 5.25 10.5 21.0 0.1640625 0.328125 0.65625 1.3125)
                #(2.75  5.5  11.0 22.0 0.171875  0.34375  0.6875  1.375)
                #(2.875 5.75 11.5 23.0 0.1796875 0.359375 0.71875 1.4375)
                #(3.0   6.0  12.0 24.0 0.1875    0.375    0.75    1.5)
                #(3.125 6.25 12.5 25.0 0.1953125 0.390625 0.78125 1.5625)
                #(3.25  6.5  13.0 26.0 0.203125  0.40625  0.8125  1.625)
                #(3.375 6.75 13.5 27.0 0.2109375 0.421875 0.84375 1.6875)
                #(3.5   7.0  14.0 28.0 0.21875   0.4375   0.875   1.75)
                #(3.625 7.25 14.5 29.0 0.2265625 0.453125 0.90625 1.8125)
                #(3.75  7.5  15.0 30.0 0.234375  0.46875  0.9375  1.875)
                #(3.875 7.75 15.5 31.0 0.2421875 0.484375 0.96875 1.9375)))
            (imm8 0 (+ imm8 1))
            (errors '()
                    (let ((expect `(fmov s0 ,(let ((const (vector-ref (vector-ref fp-constants (fxbit-field imm8 0 4))
                                                                      (fxbit-field imm8 4 7))))
                                               (if (>= imm8 #x80) (- const) const))))
                          (result (d (bitwise-ior #x1e201000 (bitwise-arithmetic-shift-left imm8 13)))))
                      (if (equal? expect result)
                          errors
                          (cons (list imm8 expect result) errors)))))
           ((= imm8 #x100) errors))
       => '())


(check-report)
(exit (if (check-passed? 35) 0 1))
