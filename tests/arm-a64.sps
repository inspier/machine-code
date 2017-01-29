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

(check (d #x721E785F) => '(ands wzr w2 #xfffffffd))
(check (d #x72001c1f) => '(ands wzr w0 #xff))
(check (d #xf240057f) => '(ands xzr x11 #x3))
(check (d #xf278001f) => '(ands xzr x0 #x100))
(check (d #xf279001f) => '(ands xzr x0 #x80))
(check (d #xf27a001f) => '(ands xzr x0 #x40))
(check (d #xf27b001f) => '(ands xzr x0 #x20))
(check (d #xf27c001f) => '(ands xzr x0 #x10))
(check (d #xf27d001f) => '(ands xzr x0 #x8))
(check (d #xf27e001f) => '(ands xzr x0 #x4))
(check (d #xf27f001f) => '(ands xzr x0 #x2))
(check (d #xf240001f) => '(ands xzr x0 #x1))
(check (d #x721b783f) => '(ands wzr w1 #xffffffef))
(check (d #xf240097f) => '(ands xzr x11 #x7))
(check (d #xf251009f) => '(ands xzr x4 #x800000000000))
(check (d #x1e604008) => '(fmov d8 d0))

(check (d #x90000141) => '(adrp x1 #x2c000))
(check (d #x10000061) => '(adr x1 #x410c))
(check (d #xb4000060) => '(cbz x0 #x410c))
(check (d #x1a9f17e2) => '(csinc w2 wzr wzr ne))
(check (d #x8b02039c) => '(add x28 x28 x2))
(check (d #x7f7fffff) => '(fcvtzu d31 d31 1))
(check (d #x295a17aa) => '(ldp w10 w5 (mem+ x29 #xD0)))
(check (d #x38401c01) => '(ldrb w1 (mempre+ x0 1)))
(check (d #xdac01001) => '(clz x1 x0))
(check (d #x1e2e1001) => '(fmov s1 1.0))
(check (d #x1e220800) => '(fmul s0 s0 s2))
(check (d #xfa401844) => '(ccmp x2 0 #x4 ne))
(check (d #x93cbd086) => '(extr x6 x4 x11 52))
(check (d #x0f3d0420) => '(sshr v0.2s v1.2s 3))
(check (d #x1f422409) => '(fmadd d9 d0 d2 d9))
(check (d #x4f000402) => '(movi v2.4s 0))
(check (d #x4f0707e1) => '(movi v1.4s #xff))
(check (d #x2f00e7e1) => '(movi d1 #xffffffffff))
(check (d #x4e211ce5) => '(and v5.16b v7.16b v1.16b))
(check (d #x0e6128f0) => '(xtn v16.4h v7.4s))
(check (d #x4eb1b842) => '(addv s2 v2.4s))
(check (d #x3c8104e5) => '(str q5 (mempost+ x7 16)))
(check (d #x0e043c42) => '(umov w2 (ref v2.s 0)))
(check (d #x4e612870) => '(xtn2 v16.8h v3.4s))
(check (d #x4ea284a2) => '(add v2.4s v5.4s v2.4s))
(check (d #x2f20a422) => '(ushll v2.2d v1.2s 0))
(check (d #x6ee28400) => '(sub v0.2d v0.2d v2.2d))
(check (d #x6ea13000) => '(usubw2 v0.2d v0.2d v1.4s))
(check (d #x5ef1b800) => '(addp d0 v0.2d))
(check (d #x2e601d01) => '(bsl v1.8b v8.8b v0.8b))
(check (d #x7e37e43c) => '(fcmge s28 s1 s23))
(check (d #x1e58f920) => '(fcvtzs w0 d9 2))
(check (d #x9b017f39) => '(madd x25 x25 x1 xzr))

(check (d #x0f002420) => '(movi v0.2s (lsl #x1 8)))
(check (d #x4f046400) => '(movi v0.4s (lsl #x80 24)))
(check (d #x0f0737e0) => '(orr v0.2s (lsl #xff 8)))
(check (d #x4f005420) => '(orr v0.4s (lsl #x1 16)))
(check (d #x0f00a420) => '(movi v0.4h (lsl #x1 8)))
(check (d #x4f048400) => '(movi v0.8h #x80))
(check (d #x0f0797e0) => '(orr v0.4h #xff))
(check (d #x4f00b420) => '(orr v0.8h (lsl #x1 8)))
(check (d #x0f00c7e0) => '(movi v0.2s (msl #x1f 8)))
(check (d #x0f00d7e0) => '(movi v0.2s (msl #x1f 16)))
(check (d #x0f05e540) => '(movi v0.8b #xaa))
(check (d #x4f02e6a0) => '(movi v0.16b #x55))
(check (d #x2f05e540) => '(movi d0 #xff00ff00ff00ff00))
(check (d #x2f02e6a0) => '(movi d0 #xff00ff00ff00ff))
(check (d #x0f03f600) => '(fmov v0.2s 1.0))
(check (d #x4f00f400) => '(fmov v0.4s 2.0))
(check (d #x2f000600) => '(mvni v0.2s #x10))
(check (d #x6f0405e0) => '(mvni v0.4s #x8f))
(check (d #x2f001600) => '(bic v0.2s #x10))
(check (d #x6f0415e0) => '(bic v0.4s #x8f))
(check (d #x2f008600) => '(mvni v0.4h #x10))
(check (d #x6f0485e0) => '(mvni v0.8h #x8f))
(check (d #x2f009600) => '(bic v0.4h #x10))
(check (d #x6f0495e0) => '(bic v0.8h #x8f))
(check (d #x2f00c600) => '(mvni v0.2s (msl #x10 8)))
(check (d #x6f04d5e0) => '(mvni v0.4s (msl #x8f 16)))
(check (d #x6f05e540) => '(movi v0.2d #xff00ff00ff00ff00))
(check (d #x6f02e6a0) => '(movi v0.2d #xff00ff00ff00ff))
(check (d #x6f00f500) => '(fmov v0.2d 3.0))
(check (d #x4e011820) => '(uzp1 v0.16b v1.16b v1.16b))
(check (d #x5f523020) => '(sqdmlal s0 h1 (ref v2.h 1)))
(check (d #x5f73302f) => '(sqdmlal s15 h1 (ref v3.h 3)))
(check (d #x5f7f33de) => '(sqdmlal s30 h30 (ref v15.h 3)))
(check (d #x5f7f3bde) => '(sqdmlal s30 h30 (ref v15.h 7)))
(check (d #x5fb03020) => '(sqdmlal d0 s1 (ref v16.s 1)))
(check (d #x5fb03820) => '(sqdmlal d0 s1 (ref v16.s 3)))
(check (d #x5fb03bde) => '(sqdmlal d30 s30 (ref v16.s 3)))
(check (d #x5fa21020) => '(fmla s0 s1 (ref v2.s 1)))
(check (d #x5fa21820) => '(fmla s0 s1 (ref v2.s 3)))
(check (d #x5fc21820) => '(fmla d0 d1 (ref v2.d 1)))

(check (d #x0c002000) => '(st1 (list v0.8b - v3.8b) (mem+ x0)))
(check (d #x0c9f2000) => '(st1 (list v0.8b - v3.8b) (mempost+ x0 32)))
(check (d #x0c812000) => '(st1 (list v0.8b - v3.8b) (mempost+ x0 x1)))
(check (d #x0d0007e0) => '(st1 (ref (list v0.b) 1) (mem+ sp)))
(check (d #x0d000400) => '(st1 (ref (list v0.b) 1) (mem+ x0)))
(check (d #x0d002400) => '(st3 (ref (list v0.b - v2.b) 1) (mem+ x0)))
(check (d #x0d0063a2) => '(st3 (ref (list v2.h - v4.h) 0) (mem+ x29)))
(check (d #x4d007b82) => '(st3 (ref (list v2.h - v4.h) 7) (mem+ x28)))
(check (d #x0d00a022) => '(st3 (ref (list v2.s - v4.s) 0) (mem+ x1)))
(check (d #x0d00b022) => '(st3 (ref (list v2.s - v4.s) 1) (mem+ x1)))
(check (d #x4d00a022) => '(st3 (ref (list v2.s - v4.s) 2) (mem+ x1)))
(check (d #x4d00b022) => '(st3 (ref (list v2.s - v4.s) 3) (mem+ x1)))
(check (d #x0d00a434) => '(st3 (ref (list v20.d - v22.d) 0) (mem+ x1)))
(check (d #x4d00a454) => '(st3 (ref (list v20.d - v22.d) 1) (mem+ x2)))
(check (d #x0d9f09e1) => '(st1 (ref (list v1.b) 2) (mempost+ x15 1)))
(check (d #x0d8e09e1) => '(st1 (ref (list v1.b) 2) (mempost+ x15 x14)))

(check (d #x1e222020) => '(fcmp s1 s2))
(check (d #x1e202068) => '(fcmp s3 0.0))
(check (d #x1e652080) => '(fcmp d4 d5))
(check (d #x1e6020c8) => '(fcmp d6 0.0))
(check (d #x1e2820f0) => '(fcmpe s7 s8))
(check (d #x1e202138) => '(fcmpe s9 0.0))
(check (d #x1e6b2150) => '(fcmpe d10 d11))
(check (d #x1e602198) => '(fcmpe d12 0.0))


;;; Complete check of the floating-point constant decoding
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

;;; Check that all instructions can be decoded without any crash.
(define (make-all-instructions-port step)
  (define id "all-32le-instructions")
  (define instruction 0)
  (define shift 0)
  (define (next-byte)
    (when (eqv? shift 32)
      (set! shift 0)
      (set! instruction (+ instruction step)))
    (let ((byte (bitwise-and (bitwise-arithmetic-shift-right instruction shift) #xff)))
      (set! shift (fx+ shift 8))
      byte))
  (define (read! bytevector start count)
    (if (> instruction #xffffffff)
        0
        (do ((i 0 (+ i 1))
             (k start (+ k 1)))
            ((or (> instruction #xffffffff) (= i count)) i)
          (bytevector-u8-set! bytevector k (next-byte)))))
  (define get-position #f)
  (define set-position! #f)
  (define close #f)
  (make-custom-binary-input-port id read! get-position set-position! close))

(display "Disassembling (almost) all instructions... ")
(let* ((step 256)
       (p (make-all-instructions-port step)))
  (let lp ((i 0))
    (when (eqv? (bitwise-and i (- (expt 2 29) 1)) 0)
      (display (list (div (* i 100) #xffffffff) '%))
      (flush-output-port (current-output-port)))
    (unless (port-eof? p)
      (guard (con
              ((invalid-opcode? con) #t))
        (get-instruction p #f #x4100))
      (lp (+ i step)))))
(newline)

;;;

(check-report)
(exit (if (check-passed? 120) 0 1))
