#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(import
  (rnrs (6))
  (machine-code disassembler arm-a64))

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
(let* ((step 64)
       (p (make-all-instructions-port step)))
  (let lp ((i 0))
    (unless (port-eof? p)
      (guard (con
              ((invalid-opcode? con) #f))
        (let ((instr (get-instruction p #f #x4100)))
          (unless (eof-object? instr)
            (display (number->string i 16))
            (display #\space)
            (display instr)
            (newline))))
      (lp (+ i step)))))
(newline)
