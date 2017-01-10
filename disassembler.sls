;; -*- mode: scheme; coding: utf-8 -*-
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

;; Generic disassembler support.

(library (machine-code disassembler)
  (export invalid-opcode?
          available-disassemblers get-disassembler
          disassembler? disassembler-name
          disassembler-min-instruction-size
          disassembler-max-instruction-size
          disassembler-instruction-getter)
  (import (rename (machine-code disassembler private)
                  (available-disassemblers p:available-disassemblers)
                  (get-disassembler p:get-disassembler))
          #;(prefix (machine-code disassembler arm-a32) arm-a32:)
          (prefix (machine-code disassembler arm-a64) arm-a64:)
          (prefix (machine-code disassembler i8080) i8080:)
          (prefix (machine-code disassembler m68hc12) m68hc12:)
          (prefix (machine-code disassembler mips) mips:)
          (prefix (machine-code disassembler x86) x86:)
          (rnrs))

  (define register-all-disassemblers
    (let ((done #f))
      (lambda ()
        (unless done
          ;; visit libraries
          (set! done (list x86:get-instruction
                           mips:get-instruction
                           m68hc12:get-instruction
                           i8080:get-instruction
                           #;arm-a32:get-instruction
                           arm-a64:get-instruction))))))

  (define (available-disassemblers)
    (register-all-disassemblers)
    (p:available-disassemblers))

  (define (get-disassembler name)
    (register-all-disassemblers)
    (or (p:get-disassembler name)
        (error 'get-disassembler "This disassembler has not been registered" name))))
