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

;; Disassembler for the A64 instruction set in ARMv8.

;; Source:

;; ARM® Architecture Reference Manual
;; ARMv8, for ARMv8-A architecture profile
;; ARM DDI 0487A.k_iss10775 (ID092916)
;; (Chapter C4)

;; Notes:

;; When an instruction uses pc it is counted from the start of the
;; instruction.

;; This file uses wide lines, 120 characters.

;; The body of the encoders are meant to, with some incremental
;; development effort, become regular and declarative enough that they
;; can be used to create an assembler table.

(library (machine-code disassembler arm-a64)
  (export get-instruction invalid-opcode?)
  (import (rnrs)
          (machine-code disassembler arm-aarch64)
          (machine-code disassembler arm-private)
          (machine-code disassembler private))

  (define-syntax print
    (syntax-rules ()
      #;
      ((_ . args) (begin (for-each display (list . args)) (newline)))
      ((_ . args) (begin 'dummy))))

  (define fxasl fxarithmetic-shift-left)
  (define fxasr fxarithmetic-shift-right)

  (define (sign-extend v size)
    ;; Takes an unsigned integer and recovers the sign.
    (if (>= v (expt 2 (- size 1)))
        (- v (expt 2 size))
        v))

  (define condition-codes
    '#(eq ne cs cc mi pl vs vc hi ls ge lt gt le al nv))

  (define condition-aliases
    '((hs . cs) (lo . cc)))

  ;; General purpose registers
  (define W/WSP-registers
    '#(w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17
          w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 wsp))
  (define X/SP-registers
    '#(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
          x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 sp))
  (define W-registers
    '#(w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17
          w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 wzr))
  (define X-registers
    '#(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
          x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 xzr))
  (define (W/WSP n) (vector-ref W/WSP-registers n))
  (define (X/SP n) (vector-ref X/SP-registers n))
  (define (W n) (vector-ref W-registers n))
  (define (X n) (vector-ref X-registers n))

  ;; Floating point and SIMD
  (define B-registers                   ;8-bit
    '#(b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17
          b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31))
  (define H-registers                   ;16-bit
    '#(h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15 h16 h17
          h18 h19 h20 h21 h22 h23 h24 h25 h26 h27 h28 h29 h30 h31))
  (define S-registers                   ;32-bit
    '#(s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17
          s18 s19 s20 s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31))
  (define D-registers                   ;64-bit
    '#(d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17
          d18 d19 d20 d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31))
  (define Q-registers                   ;128-bit
    '#(q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17
          q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31))
  (define (B n) (vector-ref B-registers n))
  (define (H n) (vector-ref H-registers n))
  (define (S n) (vector-ref S-registers n))
  (define (D n) (vector-ref D-registers n))
  (define (Q n) (vector-ref Q-registers n))

  ;; SIMD vector registers
  (define (V-size-bits size)
    (case size
      ((B) 8)
      ((H) 16)
      ((S) 32)
      ((D) 64)
      (else (error 'V-size-bits "Invalid size" size))))

  (define (%V% n lanes size)
    (assert (fx<=? 0 n 31))
    (assert (memv size '(B H S D)))
    (assert (or (not lanes) (memv (fx* lanes (V-size-bits size)) '(64 128))))
    (let* ((shape (string-append (if lanes (number->string lanes) "")
                                 (string-downcase (symbol->string size))))
           (reg (string->symbol (string-append "v" (number->string n) "." shape))))
      reg))

  (define (V n lanes size)
    ;; For these register names: Vn.8B Vn.16B Vn.4H Vn.8H Vn.2S Vn.4S Vn.1D Vn.2D
    (assert lanes)
    (%V% n lanes size))

  (define (V-ref n size element)
    ;; For these vector elements indices: Vn.B[i] Vn.H[i] Vn.S[i] Vn.D[i]
    (assert (fx<? element (fxdiv 128 (V-size-bits size))))
    `(ref ,(%V% n #f size) ,element))

  (define (%V-list% n-first n-last lanes size)
    (assert (memv size '(B H S D)))
    (assert (fx<=? 0 n-first 31))
    (assert (fx<=? 0 n-last 31))
    (cond ((eqv? n-first n-last)
           `(,(%V% n-first lanes size)))
          ((fx=? (fx- n-first 1) n-last) ; [n+1, n] ≡ [0, 31] mod 32.
           `(,(%V% 0 lanes size) ,(%V% 31 lanes size)))
          ((and (fx<? n-first n-last) (fx=? (fx- n-last n-first) 1))
           `(,(%V% n-first lanes size) ,(%V% n-last lanes size)))
          ((and (fx<? n-first n-last) (fx>? (fx- n-last n-first) 2))
           `(,(%V% n-first lanes size) - ,(%V% n-last lanes size)))
          (else
           (let ((last (if (fx<? n-first n-last) n-last (fx+ n-last 32))))
             (do ((n n-first (fx+ n 1))
                  (reg* '() (cons (%V% (fxand n #x1f) lanes size) reg*)))
                 ((fx>? n last) (reverse reg*)))))))

  (define (V-list n-first n-last lanes size)
    ;; Vector register lists contain all registers from n-first to
    ;; n-last. The range can wrap around modulo 32. This is used for
    ;; things like { V0.16B - V31.16B } and { V1.2D V2.2D }.
    (assert lanes)
    (%V-list% n-first n-last lanes size))

  (define (V-list-ref n-first n-last size idx)
    ;; Used for representing vector element lists: { V1.D - V2.D }[1]
    `(ref ,(%V-list% n-first n-last #f size) ,idx))

  ;; Control registers
  (define (C n) `(C ,n FIXME))

  ;; System registers defined in (machine-code disassembler arm-aarch64).
  (define (system-reg op0 op1 crn crm op2)
    (let ((fields (list op0 op1 crn crm op2)))
      (or (exists (lambda (def)
                    (and (equal? (cdr def) fields)
                         (car def)))
                  system-registers)
          `(S ,op0 ,op1 ,crn ,crm ,op2))))

;;; Various utilities

  (define (lsl v shift)
    (cond ((and (integer? v) (fixnum? shift))
           (bitwise-arithmetic-shift-left v shift))
          ((eqv? shift 0)
           v)
          (else
           `(lsl ,v ,shift))))

  (define (pc-rel pc offset)
    (if (number? pc)
        (+ pc offset)
        `(+ pc ,offset)))

  (define (pc-rel-page pc offset)
    (if pc
        (pc-rel (bitwise-and pc (bitwise-not 4095)) offset)
        (pc-rel `(bitwise-and pc ,(bitwise-not 4095)) offset)))

  ;; Pure memory reference
  (define (mem+ reg offset)
    (if (eqv? offset 0) `((mem+ ,reg)) `((mem+ ,reg ,offset))))

  ;; Memory reference that after the reference increments the register.
  (define (mempost+ reg offset)
    (if (eqv? offset 0) `((mempost+ ,reg)) `((mempost+ ,reg ,offset))))

  ;; Memory reference that before the reference increments the register.
  (define (mempre+ reg offset)
    (if (eqv? offset 0) `((mempre+ ,reg)) `((mempre+ ,reg ,offset))))

  ;; Decodes bit masks. Returns two interpretations: bitfield and logical immediate.
  (define (decode-bit-masks width N imms immr immediate?)
    ;; Replicates a bit battern from `from` bits wide to `to` bits wide.
    (define (replicate x from to)
      (if (fx<? from to)
          (replicate (bitwise-ior x (bitwise-arithmetic-shift-left x from))
                     (fx* from 2)
                     to)
          (bitwise-bit-field x 0 to)))
    (let ((len (fx- (fxlength (fxior (bitwise-arithmetic-shift-left N 6) (fxxor imms #b111111)))
                    1)))
      (when (fx<? len 0)
        (raise-UD "Reserved len value in bit masks"))
      (assert (>= width (bitwise-arithmetic-shift-left 1 len)))
      (let ((levels (- (bitwise-arithmetic-shift-left 1 len) 1)))
        (when (and immediate? (fx=? (fxand imms levels) levels))
          (raise-UD "Reserved S value in bit masks"))
        (let ((S (fxand imms levels))
              (R (fxand immr levels))
              (esize (bitwise-arithmetic-shift-left 1 len)))
          (values
            ;; Something like ror((1 << (S + 1)) - 1, R), replicated.
            (let* ((welem (- (bitwise-arithmetic-shift-left 1 (fx+ S 1)) 1))
                   (wmask (replicate (bitwise-rotate-bit-field welem 0 esize (fx- esize R))
                                     esize width)))
              wmask)
            ;; Something like (1 << (S - R)) - 1, replicated.
            (let* ((diff (bitwise-bit-field (fx- S R) 0 len))
                   (telem (- (bitwise-arithmetic-shift-left 1 (fx+ diff 1)) 1))
                   (tmask (replicate telem esize width)))
              tmask))))))

  (define (decode-bit-mask-immediate size N imms immr)
    (let-values (((imm _) (decode-bit-masks size N imms immr #t)))
      imm))

  (define (decode-shift op reg amount)
    (if (eqv? amount 0)
        reg
        (case op
          ((#b00) `(lsl ,reg ,amount))
          ((#b01) `(lsr ,reg ,amount))
          ((#b10) `(asr ,reg ,amount))
          ((#b11) `(ror ,reg ,amount)))))

  (define (decode-shift/no-ror op reg amount)
    (case op
      ((#b11) (raise-UD "Reserved use of ROR"))
      (else
       (decode-shift op reg amount))))

  (define (condition-code cond*)
    (vector-ref condition-codes cond*))

  (define (inverted-condition-code cond*)
    (vector-ref condition-codes (fxxor cond* #b0001)))

  (define (decode-prefetch n)
    (let ((type (case (fxbit-field n 4 5)
                  ((#b00) "PLD")
                  ((#b01) "PLI")
                  ((#b10) "PST")
                  (else #f)))
          (target (case (fxbit-field n 1 3)
                    ((#b00) "L1")
                    ((#b01) "L2")
                    ((#b10) "L3")
                    (else #f)))
          (policy (case (fxand n #b1)
                    ((#b0) "KEEP")
                    ((#b1) "STRM"))))
      (if (and type target policy)
          (string->symbol (string-append type target policy))
          n)))

  ;; Expands a floating point immediate
  (define (vfp-expand-imm imm8)
    (let ((S (fxbit-field imm8 7 8))
          (exp (fx- (fxxor #b100 (fxbit-field imm8 4 7)) 3))
          (mantissa (/ (fx+ 16 (fxbit-field imm8 0 4)) 16)))
      (inexact (* (expt -1 S) (expt 2 exp) mantissa))))

;;; Decode tables

  (define-encoding (main pc instr (31) (28 op0) (24))
    (select pc instr
            #;unallocated
            data-processing/imm
            branch/exception/system
            loads/stores
            data-processing/reg
            data-processing/simd&fp))

  #;
  (define-encoding (unallocated pc instr (31) (28 (= #b00)) (26))
    (match (instr)))

;;; C4.2

  (define-encoding (data-processing/imm pc instr (31) (28 (= #b100)) (25 op0) (22))
    (select pc instr
            add/subtract
            bitfield
            extract
            logical/imm
            move-wide/imm
            pc-rel-addr))

  (define (page-immediate imm12 shift)
    (case shift
      ((#b00) imm12)
      ((#b01) (fxasl imm12 12))
      (else (raise-UD "Reserved shift value"))))

  ;; C4.2.1
  (define-encoding (add/subtract pc instr (31 sf) (30 op) (29 S) (28 (= #b10001)) (23 shift) (21 imm12) (9 Rn) (4 Rd))
    (match (sf op S)
      [(0 0 0) `(add ,(W/WSP Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
      [(0 0 1) `(adds ,(W Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
      [(0 1 0) `(sub ,(W/WSP Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
      [(0 1 1) `(subs ,(W Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
      [(1 0 0) `(add ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
      [(1 0 1) `(adds ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
      [(1 1 0) `(sub ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
      [(1 1 1) `(subs ,(X Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]))

  ;; C4.2.2
  (define-encoding (bitfield pc instr (31 sf) (30 opc) (28 (= #b100110)) (22 N) (21 immr) (15 imms) (9 Rn) (4 Rd))
    (match (sf opc N)
      [(0 #b00 0) `(sbfm ,(W Rd) ,(W Rn) ,immr ,imms)]
      [(0 #b01 0) `(bfm ,(W Rd) ,(W Rn) ,immr ,imms)]
      [(0 #b10 0) `(ubfm ,(W Rd) ,(W Rn) ,immr ,imms)]
      [(1 #b00 1) `(sbfm ,(X Rd) ,(X Rn) ,immr ,imms)]
      [(1 #b01 1) `(bfm ,(X Rd) ,(X Rn) ,immr ,imms)]
      [(1 #b10 1) `(ubfm ,(X Rd) ,(X Rn) ,immr ,imms)]))

  ;; C4.2.3
  (define-encoding (extract pc instr (31 sf) (30 op21) (28 (= #b100111)) (22 N) (21 o0) (20 Rm) (15 imms) (9 Rn) (4 Rd))
    (match (sf op21 N o0 imms)
      [(0 00 0 0 'b0xxxxx) `(extr ,(W Rd) ,(W Rn) ,(W Rm) ,imms)]
      [(1 00 1 0 'bxxxxxx) `(extr ,(X Rd) ,(X Rn) ,(X Rm) ,imms)]))

  ;; C4.2.4
  (define-encoding (logical/imm pc instr (31 sf) (30 opc) (28 (= #b100100)) (22 N) (21 immr) (15 imms) (9 Rn) (4 Rd))
    (match (sf opc N)
      [(0 #b00 #b0) `(and ,(W/WSP Rd) ,(X Rn) ,(decode-bit-mask-immediate 32 N imms immr))]
      [(0 #b01 #b0) `(orr ,(W/WSP Rd) ,(X Rn) ,(decode-bit-mask-immediate 32 N imms immr))]
      [(0 #b10 #b0) `(eor ,(W/WSP Rd) ,(X Rn) ,(decode-bit-mask-immediate 32 N imms immr))]
      [(0 #b11 #b0)
       (if (= Rd #b11111)
           `(tst ,(W Rn) ,(decode-bit-mask-immediate 32 N imms immr))
           `(ands ,(W Rd) ,(W Rn) ,(decode-bit-mask-immediate 32 N imms immr)))]
      [(1 #b00 'bx) `(and ,(X/SP Rd) ,(X Rn) ,(decode-bit-mask-immediate 64 N imms immr))]
      [(1 #b01 'bx) `(orr ,(X/SP Rd) ,(X Rn) ,(decode-bit-mask-immediate 64 N imms immr))]
      [(1 #b10 'bx) `(eor ,(X/SP Rd) ,(X Rn) ,(decode-bit-mask-immediate 64 N imms immr))]
      [(1 #b11 'bx)
       (if (= Rd #b11111)
           `(tst ,(X Rn) ,(decode-bit-mask-immediate 64 N imms immr))
           `(ands ,(X Rd) ,(W Rn) ,(decode-bit-mask-immediate 64 N imms immr)))]))

  ;; C4.2.5
  (define-encoding (move-wide/imm pc instr (31 sf) (30 opc) (28 (= #b100101)) (22 hw) (20 imm16) (4 Rd))
    (match (sf opc)
      [(0 #b00) `(movn ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]
      [(0 #b10) `(movz ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]
      [(0 #b11) `(movk ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]
      [(1 #b00) `(movn ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]
      [(1 #b10) `(movz ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]
      [(1 #b11) `(movk ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]))

  ;; C4.2.6
  (define-encoding (pc-rel-addr pc instr (31 op) (30 immlo) (28 (= #b10000)) (23 immhi) (4 Rd))
    (match (op)
      [(0)
       `(adr ,(X Rd) ,(pc-rel pc (fxior (fxasl immhi 2) immlo)))]
      [(1)
       `(adrp ,(X Rd)
              ,(pc-rel-page pc (bitwise-arithmetic-shift-left (fxior (fxasl immhi 2) immlo) 12)))]))

;;; C4.3

  (define-encoding (branch/exception/system pc instr (31 op0) (28 (= #b101)) (25 op1) (21))
    (select pc instr
            compare&branch/imm
            cond-branch/imm
            exception
            system
            test&branch/imm
            uncond-branch/imm
            uncond-branch/reg))

  ;; C4.3.1
  (define-encoding (compare&branch/imm pc instr (31 sf) (30 (= #b011010)) (24 op) (23 imm19) (4 Rt))
    (match (sf op)
      [(0 0) `(cbz ,(W Rt) ,(pc-rel pc (* imm19 4)))]
      [(0 1) `(cbnz ,(W Rt) ,(pc-rel pc (* imm19 4)))]
      [(1 0) `(cbz ,(X Rt) ,(pc-rel pc (* imm19 4)))]
      [(1 1) `(cbnz ,(X Rt) ,(pc-rel pc (* imm19 4)))]))

  ;; C4.3.2
  (define-encoding (cond-branch/imm pc instr (31 (= #b0101010)) (24 o1) (23 imm19) (4 o0) (3 cond*))
    (match (o1 o0)
      [(0 0)
       `(,(string->symbol (string-append "b." (symbol->string (condition-code cond*))))
         ,(pc-rel pc (bitwise-arithmetic-shift-left (sign-extend imm19 19) 2)))]))

  ;; C4.3.3
  (define-encoding (exception pc instr (31 (= #b11010100)) (23 opc) (20 imm16) (4 op2) (1 LL))
    (match (opc op2 LL)
      [(#b000 #b000 #b01) `(svc ,imm16)]
      [(#b000 #b000 #b10) `(hvc ,imm16)]
      [(#b000 #b000 #b11) `(smc ,imm16)]
      [(#b001 #b000 #b00) `(brk ,imm16)]
      [(#b010 #b000 #b00) `(hlt ,imm16)]
      [(#b101 #b000 #b01) `(dcps1 ,imm16)]
      [(#b101 #b000 #b10) `(dcps2 ,imm16)]
      [(#b101 #b000 #b11) `(dcps3 ,imm16)]))

  ;; C4.3.4
  (define-encoding (system pc instr (31 (= #b1101010100)) (21 L) (20 op0) (18 op1) (15 CRn) (11 CRm) (7 op2) (4 Rt))
    (match (L op0 op1 CRn CRm op2 Rt)
      [(0 #b00 'bxxx #b0100 'bxxxx 'bxxx #b11111) (msr/immediate pc instr)]
      [(0 #b00 #b011 #b0010 (!= #b0000) 'bxxx #b11111) `(hint ,(fxior (fxasl CRm 3) op2))]
      [(0 #b00 #b011 #b0010 #b0000 #b000 #b11111) `(nop)]
      [(0 #b00 #b011 #b0010 #b0000 #b001 #b11111) `(yield)]
      [(0 #b00 #b011 #b0010 #b0000 #b010 #b11111) `(wfe)]
      [(0 #b00 #b011 #b0010 #b0000 #b011 #b11111) `(wfi)]
      [(0 #b00 #b011 #b0010 #b0000 #b100 #b11111) `(sev)]
      [(0 #b00 #b011 #b0010 #b0000 #b101 #b11111) `(sevl)]
      [(0 #b00 #b011 #b0010 #b0000 'b11x #b11111) `(hint ,(fxior (fxasl CRm 3) op2))]
      [(0 #b00 #b011 #b0011 'bxxxx #b010 #b11111) `(clrex ,CRm)]
      ;; [(0 #b00 #b011 #b0011 'bxxxx #b100 #b11111) `(dsb )] FIXME
      ;; [(0 #b00 #b011 #b0011 'bxxxx #b101 #b11111) `(dmb )] FIXME
      [(0 #b00 #b011 #b0011 'bxxxx #b110 #b11111) `(isb ,(case CRm ((#b1111) 'SY) (else CRm)))]
      [(0 #b01 'bxxx 'bxxxx 'bxxxx 'bxxx 'bxxxxx) `(sys ,op1 (C CRn) ,(C CRm) ,op2 ,Rt)] ;TODO: aliases
      [(0 'b1x 'bxxx 'bxxxx 'bxxxx 'bxxx 'bxxxxx) `(msr ,(system-reg op0 op1 CRn CRm op2) ,(X Rt))]
      [(1 #b01 'bxxx 'bxxxx 'bxxxx 'bxxx 'bxxxxx) `(sysl ,(X Rt) ,op1 ,(C CRn) ,(C CRm) ,op2)]
      [(1 'b1x 'bxxx 'bxxxx 'bxxxx 'bxxx 'bxxxxx) `(mrs ,(X Rt) ,(system-reg op0 op1 CRn CRm op2))]))

  (define-encoding (msr/immediate pc instr (31 (= #b1101010100000)) (18 op1) (15 (= #b0100)) (11 CRm) (7 op2)
                                  (4 (= #b11111)))
    (match (op1 op2)
      [(#b000 #b101) `(msr SPSel ,CRm)]
      [(#b011 #b110) `(msr DAIFSet ,CRm)]
      [(#b011 #b111) `(msr DAIFClr ,CRm)]))

  ;; C4.3.5
  (define-encoding (test&branch/imm pc instr (31 b5) (30 (= #b011011)) (24 op) (23 b40) (18 imm14) (4 Rt))
    (match (op)
      [(0)
       (let ((R (if (eqv? b5 #b1) X W)))
         `(tbz ,(R Rt) ,(fxior (fxasl b5 6) b40) (pc-rel pc ,(fxasl imm14 2))))]
      [(1)
       (let ((R (if (eqv? b5 #b1) X W)))
         `(tbnz ,(R Rt) ,(fxior (fxasl b5 6) b40) (pc-rel pc ,(fxasl imm14 2))))]))

  ;; C4.3.6
  (define-encoding (uncond-branch/imm pc instr (31 op) (30 (= #b00101)) (25 imm26))
    (match (op)
      [(0) `(b ,(pc-rel pc (fx* (sign-extend imm26 26) 4)))]
      [(1) `(bl ,(pc-rel pc (fx* (sign-extend imm26 26) 4)))]))

  ;; C4.3.7
  (define-encoding (uncond-branch/reg pc instr (31 (= #b1101011)) (24 opc) (20 op2) (15 op3) (9 Rn) (4 op4))
    (match (opc op2 op3 Rn op4)
      [(#b0000 #b11111 #b000000 'bxxxxx #b00000) `(br ,(X Rn))]
      [(#b0001 #b11111 #b000000 'bxxxxx #b00000) `(blr ,(X Rn))]
      [(#b0010 #b11111 #b000000 'bxxxxx #b00000) (if (eqv? Rn 30) '(ret) `(ret ,(X Rn)))]
      [(#b0100 #b11111 #b000000 #b11111 #b00000) '(eret)]
      [(#b0101 #b11111 #b000000 #b11111 #b00000) '(drps)]))

;;; C4.4

  (define-encoding (loads/stores pc instr (31 op0) (30) (29 op1) (27 (= #b1)) (26 op2) (25 (= #b0)) (24 op3)
                                 (22) (21 op4) (15) (11 op5) (9))
    (select pc instr
            load/store-adv-simd-multi
            load/store-adv-simd-multi-postidx
            load/store-adv-simd-single
            load-store/adv-simd-single-postidx
            load-register-literal
            load/store-exclusive
            load/store-no-alloc-pair/offset
            load/store-reg/imm-postidx
            load/store-reg/imm-preidx
            load/store-reg/reg-offset
            load/store-reg/unprivileged
            load/store-reg/unscaled-imm
            load/store-reg/unsigned-imm
            load/store-regpair/offset
            load/store-regpair/postidx
            load/store-regpair/preidx))

  ;; C4.4.1
  (define-encoding (load/store-adv-simd-multi pc instr (31 (= #b0)) (30 Q) (29 (= #b0011000)) (22 L)
                                              (21 (= #b000000)) (15 opcode) (11 size) (9 Rn) (4 Rt))
    (match (L opcode)))

  ;; C4.4.2
  (define-encoding (load/store-adv-simd-multi-postidx pc instr (31 (= #b0)) (30 Q) (29 (= #b0011001)) (22 L)
                                                      (21 (= #b0)) (20 Rm) (15 opcode) (11 size) (9 Rn) (4 Rt))
    (match (L Rm opcode)))

  ;; C4.4.3
  (define-encoding (load/store-adv-simd-single pc instr (31 (= #b0)) (30 Q) (29 (= #b0011010)) (22 L) (21 R)
                                               (20 (= #b00000)) (15 opcode) (12 S) (11 size) (9 Rn) (4 Rt))
    (match (L R opcode S size)))

  ;; C4.4.4
  (define-encoding (load-store/adv-simd-single-postidx pc instr (31 (= #b0)) (30 Q) (29 (= #b0011011)) (22 L) (21 R)
                                                       (20 Rm) (15 opcode) (12 S) (11 size) (9 Rn) (4 Rt))
    (match (L R Rm opcode S size)))

  ;; C4.4.5
  (define-encoding (load-register-literal pc instr (31 opc) (29 (= #b011)) (26 V) (25 (= #b00)) (23 imm19) (4 Rt))
    (match (opc V)
      [(#b00 0) `(ldr ,(W Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b01 0) `(ldr ,(X Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b10 0) `(ldrsw ,(X Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b11 0) `(prfm ,(decode-prefetch Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b00 1) `(ldr ,(S Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b01 1) `(ldr ,(D Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]
      [(#b10 1) `(ldr ,(Q Rt) ,(pc-rel pc (lsl (sign-extend imm19 19) 2)))]))

  ;; C4.4.6
  (define-encoding (load/store-exclusive pc instr (31 size) (29 (= #b001000)) (23 o2) (22 L) (21 o1) (20 Rs) (15 o0)
                                         (14 Rt2) (9 Rn) (4 Rt))
    (match (size o2 L o1 o0)
      [(#b00 0 0 0 0) `(stxrb ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b00 0 0 0 1) `(stlxrb ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b00 0 1 0 0) `(ldxrb ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b00 0 1 0 1) `(ldaxrb ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b00 1 0 0 1) `(stlrb ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b00 1 1 0 1) `(ldarb ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 0 0 0 0) `(stxrh ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 0 0 0 1) `(stlxrh ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 0 1 0 0) `(ldxrh ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 0 1 0 1) `(ldaxrh ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 1 0 0 1) `(stlrh ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b01 1 1 0 1) `(ldarh ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 0 0 0) `(stxr ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 0 0 1) `(stlxr ,(W Rs) ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 0 1 0) `(stxp ,(W Rs) ,(W Rt) ,(W Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 0 1 1) `(stlxp ,(W Rs) ,(W Rt) ,(W Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 1 0 0) `(ldxr ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 1 0 1) `(ldaxr ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 1 1 0) `(ldxp ,(W Rt) ,(W Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b10 0 1 1 1) `(ldaxp ,(W Rt) ,(W Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b10 1 0 0 1) `(stlr ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b10 1 1 0 1) `(ldar ,(W Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 0 0 0) `(stxr ,(W Rs) ,(X Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 0 0 1) `(stlxr ,(W Rs) ,(X Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 0 1 0) `(stxp ,(W Rs) ,(X Rt) ,(X Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 0 1 1) `(stlxp ,(W Rs) ,(X Rt) ,(X Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 1 0 0) `(ldxr ,(X Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 1 0 1) `(ldaxr ,(X Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 1 1 0) `(ldxp ,(X Rt) ,(X Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b11 0 1 1 1) `(ldaxp ,(X Rt) ,(X Rt2) ,(mem+ (X/SP Rn) 0))]
      [(#b11 1 0 0 1) `(stlr ,(X Rt) ,(mem+ (X/SP Rn) 0))]
      [(#b11 1 1 0 1) `(ldar ,(X Rt) ,(mem+ (X/SP Rn) 0))]))

  ;; C4.4.7
  (define-encoding (load/store-no-alloc-pair/offset pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b000)) (22 L)
                                                    (21 imm7) (14 Rt2) (9 Rn) (4 Rt))
    (match (opc V L)
      [(#b00 0 0) `(stnp ,(W Rt) ,(W Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 0 1) `(ldnp ,(W Rt) ,(W Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 0) `(stnp ,(S Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 1) `(ldnp ,(S Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 1 0) `(stnp ,(D Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b01 1 1) `(ldnp ,(D Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 0) `(stnp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 1) `(ldnp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 1 0) `(stnp ,(Q Rt) ,(Q Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]
      [(#b10 1 1) `(ldnp ,(Q Rt) ,(Q Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]))

  ;; C4.4.8
  (define-encoding (load/store-reg/imm-postidx pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (22 opc)
                                               (21 (= #b0)) (20 imm9) (11 (= #b01)) (9 Rn) (4 Rt))
    (match (size V opc)
      [(#b00 0 #b00) `(strb ,(W Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b01) `(ldrb ,(W Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b10) `(ldrsb ,(X Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b11) `(ldrsb ,(W Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b00) `(str ,(B Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b01) `(ldr ,(B Rt) ,@(mempost+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b10) `(str ,(Q Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) 4)))]
      [(#b00 1 #b11) `(ldr ,(Q Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) 4)))]
      [(#b01 0 #b00) `(strh ,(W Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b01) `(ldrh ,(W Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b10) `(ldrsh ,(X Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b11) `(ldrsh ,(W Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 1 #b00) `(str ,(H Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 1 #b01) `(ldr ,(H Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b00) `(str ,(W Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b01) `(ldr ,(W Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b10) `(ldrsw ,(X Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 1 #b00) `(str ,(S Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 1 #b01) `(ldr ,(S Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 0 #b00) `(str ,(X Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 0 #b01) `(ldr ,(X Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 1 #b00) `(str ,(D Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 1 #b01) `(ldr ,(D Rt) ,@(mempost+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]))

  ;; C4.4.9
  (define-encoding (load/store-reg/imm-preidx pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                              (21 (= #b0)) (20 imm9) (11 (= #b11)) (9 Rn) (4 Rt))
    (match (size V opc)
      [(#b00 0 #b00) `(strb ,(W Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b01) `(ldrb ,(W Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b10) `(ldrsb ,(X Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b11) `(ldrsb ,(W Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b00) `(str ,(B Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b01) `(ldr ,(B Rt) ,@(mempre+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b10) `(str ,(Q Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) 4)))]
      [(#b00 1 #b11) `(ldr ,(Q Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) 4)))]
      [(#b01 0 #b00) `(strh ,(W Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b01) `(ldrh ,(W Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b10) `(ldrsh ,(X Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 0 #b11) `(ldrsh ,(W Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 1 #b00) `(str ,(H Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b01 1 #b01) `(ldr ,(H Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b00) `(str ,(W Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b01) `(ldr ,(W Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 0 #b10) `(ldrsw ,(X Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 1 #b00) `(str ,(S Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b10 1 #b01) `(ldr ,(S Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 0 #b00) `(str ,(X Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 0 #b01) `(ldr ,(X Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 1 #b00) `(str ,(D Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]
      [(#b11 1 #b01) `(ldr ,(D Rt) ,@(mempre+ (X/SP Rn) (lsl (sign-extend imm9 9) size)))]))

  ;; C4.4.10
  (define-encoding (load/store-reg/reg-offset pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                              (21 (= #b1)) (20 Rm) (15 option) (12 S*) (11 (= #b10)) (9 Rn) (4 Rt))
    (match (size V opc)
      [(#b00 0 #b00) `(strb ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 0 #b01) `(ldrb ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 0 #b10) `(ldrsb ,(X Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 0 #b11) `(ldrsb ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 1 #b00) `(str ,(B Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 1 #b01) `(ldr ,(B Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b00 1 #b10) `(str ,(Q Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 4))))]
      [(#b00 1 #b11) `(ldr ,(Q Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 4))))]
      [(#b01 0 #b00) `(strh ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b01 0 #b01) `(ldrh ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b01 0 #b10) `(ldrsh ,(X Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b01 0 #b11) `(ldrsh ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b01 1 #b00) `(str ,(H Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b01 1 #b01) `(ldr ,(H Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option S*)))]
      [(#b10 0 #b00) `(str ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 2))))]
      [(#b10 0 #b01) `(ldr ,(W Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 2))))]
      [(#b10 0 #b10) `(ldrsw ,(X Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 2))))]
      [(#b10 1 #b00) `(str ,(S Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 2))))]
      [(#b10 1 #b01) `(ldr ,(S Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 2))))]
      [(#b11 0 #b00) `(str ,(X Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 3))))]
      [(#b11 0 #b01) `(ldr ,(X Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 3))))]
      [(#b11 0 #b10) `(prfm ,(decode-prefetch Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 3))))]
      [(#b11 1 #b00) `(str ,(D Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 3))))]
      [(#b11 1 #b01) `(ldr ,(D Rt) ,@(mem+ (X/SP Rn) (extend/lsl Rm option (fx* S* 3))))]))

  (define (extend/lsl m option shift)
    (case option
      ((#b010) `(uxtw ,(W m) ,shift))
      ((#b011) `(lsl ,(X m) ,shift))
      ((#b110) `(sxtw ,(W m) ,shift))
      ((#b111) `(sxtx ,(X m) ,shift))
      (else
       (raise-UD "Invalid extend" m option shift))))

  ;; C4.4.11
  (define-encoding (load/store-reg/unprivileged pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                                (21 (= #b0)) (20 imm9) (11 (= #b10)) (9 Rn) (4 Rt))
    (match (size V opc)
      [(#b00 0 #b00) `(sttrb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b01) `(ldtrb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b10) `(ldtrsb ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b11) `(ldtrsb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b00) `(sttrh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b01) `(ldtrh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b10) `(ldtrsh ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b11) `(ldtrsh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b00) `(sttr ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b01) `(ldtr ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b10) `(ldtrsw ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 0 #b00) `(sttr ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 0 #b01) `(ldtr ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]))

  ;; C4.4.12
  (define-encoding (load/store-reg/unscaled-imm pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                                (21 (= #b0)) (20 imm9) (11 (= #b00)) (9 Rn) (4 Rt))
    (match (size V opc)
      [(#b00 0 #b00) `(sturb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b01) `(ldurb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b10) `(ldursb ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 0 #b11) `(ldursb ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b00) `(stur ,(B Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b01) `(ldur ,(B Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b10) `(stur ,(Q Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b00 1 #b11) `(ldur ,(Q Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b00) `(sturh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b01) `(ldurh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b10) `(ldursh ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 0 #b11) `(ldursh ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 1 #b00) `(stur ,(H Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b01 1 #b01) `(ldur ,(H Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b00) `(stur ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b01) `(ldur ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 0 #b10) `(ldursw ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 1 #b00) `(stur ,(S Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b10 1 #b01) `(ldur ,(S Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 0 #b00) `(stur ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 0 #b01) `(ldur ,(X Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 0 #b10) `(prfm ,(decode-prefetch Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 1 #b00) `(stur ,(D Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]
      [(#b11 1 #b01) `(ldur ,(D Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]))

  ;; C4.4.13
  (define-encoding (load/store-reg/unsigned-imm pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b01)) (23 opc)
                                                (21 imm12) (9 Rn) (4 Rt))
    ;; The immediate is generally shifted left by (fxior (fxasl (fxand opc #b10) 1) size).
    (match (size V opc)
      [(#b00 0 #b00) `(strb ,(W Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 0 #b01) `(ldrb ,(W Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 0 #b10) `(ldrsb ,(X Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 0 #b11) `(ldrsb ,(W Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 1 #b00) `(str ,(B Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 1 #b01) `(ldr ,(B Rt) ,@(mem+ (X/SP Rn) imm12))]
      [(#b00 1 #b10) `(str ,(Q Rt) ,@(mem+ (X/SP Rn) (lsl imm12 4)))]
      [(#b00 1 #b11) `(ldr ,(Q Rt) ,@(mem+ (X/SP Rn) (lsl imm12 4)))]
      [(#b01 0 #b00) `(strh ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b01 0 #b01) `(ldrh ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b01 0 #b10) `(ldrsh ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b01 0 #b11) `(ldrsh ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b01 1 #b00) `(str ,(H Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b01 1 #b01) `(ldr ,(H Rt) ,@(mem+ (X/SP Rn) (lsl imm12 1)))]
      [(#b10 0 #b00) `(str ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
      [(#b10 0 #b01) `(ldr ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
      [(#b10 0 #b10) `(ldrsw ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 2)))]
      [(#b10 1 #b00) `(str ,(S Rt) ,@(mem+ (X/SP Rn) (lsl imm12 2)))]
      [(#b10 1 #b01) `(ldr ,(S Rt) ,@(mem+ (X/SP Rn) (lsl imm12 2)))]
      [(#b11 0 #b00) `(str ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
      [(#b11 0 #b01) `(ldr ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
      [(#b11 0 #b10) `(prfm ,(decode-prefetch Rt) ,@(mem+ (X/SP Rn) (lsl imm12 3)))]
      [(#b11 1 #b00) `(str ,(D Rt) ,@(mem+ (X/SP Rn) (lsl imm12 3)))]
      [(#b11 1 #b01) `(ldr ,(D Rt) ,@(mem+ (X/SP Rn) (lsl imm12 3)))]))

  ;; C4.4.14
  (define-encoding (load/store-regpair/offset pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b010)) (22 L) (21 imm7)
                                              (14 Rt2) (9 Rn) (4 Rt))
    (match (opc V L)
      [(#b00 0 0) `(stp ,(W Rt) ,(W Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 0 1) `(ldp ,(W Rt) ,(W Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 0) `(stp ,(S Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 1) `(ldp ,(S Rt) ,(S Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 0 1) `(ldpsw ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 1 0) `(stp ,(D Rt) ,(D Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b01 1 1) `(ldp ,(D Rt) ,(D Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 0) `(stp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 1) `(ldp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 1 0) `(stp ,(Q Rt) ,(D Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]
      [(#b10 1 1) `(ldp ,(Q Rt) ,(D Rt2) ,@(mem+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]))

  ;; C4.4.15
  (define-encoding (load/store-regpair/postidx pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b001)) (22 L) (21 imm7)
                                               (14 Rt2) (9 Rn) (4 Rt))
    (match (opc V L)
      [(#b00 0 0) `(stp ,(W Rt) ,(W Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 0 1) `(ldp ,(W Rt) ,(W Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 0) `(stp ,(S Rt) ,(S Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 1) `(ldp ,(S Rt) ,(S Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 0 1) `(ldpsw ,(X Rt) ,(X Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 1 0) `(stp ,(D Rt) ,(D Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b01 1 1) `(ldp ,(D Rt) ,(D Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 0) `(stp ,(X Rt) ,(X Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 1) `(ldp ,(X Rt) ,(X Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 1 0) `(stp ,(Q Rt) ,(D Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]
      [(#b10 1 1) `(ldp ,(Q Rt) ,(D Rt2) ,@(mempost+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]))

  ;; C4.4.16
  (define-encoding (load/store-regpair/preidx pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b011)) (22 L) (21 imm7)
                                              (14 Rt2) (9 Rn) (4 Rt))
    (match (opc V L)
      [(#b00 0 0) `(stp ,(W Rt) ,(W Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 0 1) `(ldp ,(W Rt) ,(W Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 0) `(stp ,(S Rt) ,(S Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b00 1 1) `(ldp ,(S Rt) ,(S Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 0 1) `(ldpsw ,(X Rt) ,(X Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 2)))]
      [(#b01 1 0) `(stp ,(D Rt) ,(D Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b01 1 1) `(ldp ,(D Rt) ,(D Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 0) `(stp ,(X Rt) ,(X Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 0 1) `(ldp ,(X Rt) ,(X Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 3)))]
      [(#b10 1 0) `(stp ,(Q Rt) ,(D Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]
      [(#b10 1 1) `(ldp ,(Q Rt) ,(D Rt2) ,@(mempre+ (X/SP Rn) (fxasl (sign-extend imm7 7) 4)))]))

;;; C4.5

  (define-encoding (data-processing/reg pc instr (31) (30 op0) (29) (28 op1) (27 (= #b101)) (24 op2) (20) (11 op3) (10))
    (select pc instr
            add/sub-ext-reg
            add/sub-shift-reg
            add/sub-with-carry
            cond-compare/imm
            cond-compare/reg
            conditional-select
            data-processing/1src
            data-processing/2src
            data-processing/3src
            logical/shifted-reg))

  ;; C4.5.1
  (define-encoding (add/sub-ext-reg pc instr (31 sf) (30 op) (29 S) (28 (= #b01011)) (23 opt) (21 (= #b1)) (20 Rm)
                                    (15 option) (12 imm3) (9 Rn) (4 Rd))
    (match (sf op S opt)
      [(0 0 0 #b00) `(add ,(W/WSP Rd) ,(W/WSP Rn) ,(extend-reg/lsl 32 Rd Rn Rm option imm3))]
      [(0 0 1 #b00) `(adds ,(W/WSP Rd) ,(W/WSP Rn) ,(extend-reg/lsl 32 Rd Rn Rm option imm3))]
      [(0 1 0 #b00) `(sub ,(W/WSP Rd) ,(W/WSP Rn) ,(extend-reg/lsl 32 Rd Rn Rm option imm3))]
      [(0 1 1 #b00) `(subs ,(W/WSP Rd) ,(W/WSP Rn) ,(extend-reg/lsl 32 Rd Rn Rm option imm3))]
      [(1 0 0 #b00) `(add ,(X/SP Rd) ,(X/SP Rn) ,(extend-reg/lsl 64 Rd Rn Rm option imm3))]
      [(1 0 1 #b00) `(adds ,(X/SP Rd) ,(X/SP Rn) ,(extend-reg/lsl 64 Rd Rn Rm option imm3))]
      [(1 1 0 #b00) `(sub ,(X/SP Rd) ,(X/SP Rn) ,(extend-reg/lsl 64 Rd Rn Rm option imm3))]
      [(1 1 1 #b00) `(subs ,(X/SP Rd) ,(X/SP Rn) ,(extend-reg/lsl 64 Rd Rn Rm option imm3))]))

  (define (extend-reg/lsl bits d n m option shift)
    (unless (fx<=? shift 4)
      (raise-UD "Unallocated add/sub-ext-reg op" `(shift ,shift)))
    (let ((X (if (eqv? bits 32) W X)))
      (case option
        ((#b000) `(uxtb ,(W m) ,shift))
        ((#b001) `(uxth ,(W m) ,shift))
        ((#b010)
         (if (and (eqv? bits 32) (or (eqv? d #b11111) (eqv? n #b11111)))
             (lsl (W m) shift)
             `(uxtw ,(W m) ,shift)))
        ((#b011)
         (if (and (eqv? bits 32) (or (eqv? d #b11111) (eqv? n #b11111)))
             (lsl (W m) shift)
             `(uxtx ,(X m) ,shift)))
        ((#b100) `(sxtb ,(W m) ,shift))
        ((#b101) `(sxth ,(W m) ,shift))
        ((#b110) `(sxtw ,(W m) ,shift))
        ((#b111) `(sxtx ,(X m) ,shift))
        (else                           ;should never happen
         (raise-UD "Invalid extend" m option shift)))))

  ;; C4.5.2
  (define-encoding (add/sub-shift-reg pc instr (31 sf) (30 op) (29 S) (28 (= #b01011)) (23 shift) (21 (= #b0))
                                      (20 Rm) (15 imm6) (9 Rn) (4 Rd))
    (match (sf op S)
      [(0 0 0) `(add ,(W Rd) ,(W Rn) ,(decode-shift/no-ror shift (W Rm) imm6))]
      [(0 0 1) `(adds ,(W Rd) ,(W Rn) ,(decode-shift/no-ror shift (W Rm) imm6))]
      [(0 1 0) `(sub ,(W Rd) ,(W Rn) ,(decode-shift/no-ror shift (W Rm) imm6))]
      [(0 1 1) `(subs ,(W Rd) ,(W Rn) ,(decode-shift/no-ror shift (W Rm) imm6))]
      [(1 0 0) `(add ,(X Rd) ,(X Rn) ,(decode-shift/no-ror shift (X Rm) imm6))]
      [(1 0 1) `(adds ,(X Rd) ,(X Rn) ,(decode-shift/no-ror shift (X Rm) imm6))]
      [(1 1 0) `(sub ,(X Rd) ,(X Rn) ,(decode-shift/no-ror shift (X Rm) imm6))]
      [(1 1 1) `(subs ,(X Rd) ,(X Rn) ,(decode-shift/no-ror shift (X Rm) imm6))]))

  ;; C4.5.3
  (define-encoding (add/sub-with-carry pc instr (31 sf) (30 op) (29 S) (28 (= #b11010000)) (20 Rm) (15 opcode2)
                                       (9 Rn) (4 Rd))
    (match (sf op S opcode2)
      [(0 0 0 #b000000) `(adc ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 1 #b000000) `(adcs ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 1 0 #b000000) `(sbc ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 1 1 #b000000) `(sbcs ,(W Rd) ,(W Rn) ,(W Rm))]
      [(1 0 0 #b000000) `(adc ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 1 #b000000) `(adcs ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 1 0 #b000000) `(sbc ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 1 1 #b000000) `(sbcs ,(X Rd) ,(X Rn) ,(X Rm))]))

  ;; C4.5.4
  (define-encoding (cond-compare/imm pc instr (31 sf) (30 op) (29 S) (28 (= #b11010010)) (20 imm5) (15 cond*)
                                     (11 (= #b1)) (10 o2) (9 Rn) (4 o3) (3 nzcv))
    (match (sf op S o2 o3)
      [(0 0 1 0 0) `(ccmn ,(W Rn) ,imm5 ,nzcv ,(condition-code cond*))]
      [(0 1 1 0 0) `(ccmp ,(W Rn) ,imm5 ,nzcv ,(condition-code cond*))]
      [(1 0 1 0 0) `(ccmn ,(X Rn) ,imm5 ,nzcv ,(condition-code cond*))]
      [(1 1 1 0 0) `(ccmp ,(X Rn) ,imm5 ,nzcv ,(condition-code cond*))]))

  ;; C4.5.5
  (define-encoding (cond-compare/reg pc instr (31 sf) (30 op) (29 S) (28 (= #b11010010)) (20 Rm) (15 cond*) (11 (= #b0))
                                     (10 o2) (9 Rn) (4 o3) (3 nzcv))
    (match (sf op S o2 o3)
      [(0 0 1 0 0) `(ccmn ,(W Rn) ,(W Rm) ,nzcv ,(condition-code cond*))]
      [(0 1 1 0 0) `(ccmp ,(W Rn) ,(W Rm) ,nzcv ,(condition-code cond*))]
      [(1 0 1 0 0) `(ccmn ,(X Rn) ,(X Rm) ,nzcv ,(condition-code cond*))]
      [(1 1 1 0 0) `(ccmp ,(X Rn) ,(X Rm) ,nzcv ,(condition-code cond*))]))

  ;; C4.5.6
  (define-encoding (conditional-select pc instr (31 sf) (30 op) (29 S) (28 (= #b11010100)) (20 Rm) (15 cond*) (11 op2)
                                       (9 Rn) (4 Rd))
    (match (sf op S op2)
      [(0 0 0 #b00) `(csel ,(W Rd) ,(W Rn) ,(W Rm) ,(condition-code cond*))]
      [(0 0 0 #b01)
       (cond ((and (!= Rm #b11111) (!&= cond* 'b111x) (!= Rn #b11111) (= Rn Rm))
              `(cinc ,(W Rd) ,(inverted-condition-code cond*)))
             ((and (= Rm #b11111) (!&= cond* 'b111x) (= Rn #b11111))
              `(cset ,(W Rd) ,(inverted-condition-code cond*)))
             (else
              `(csinc ,(W Rd) ,(W Rn) ,(W Rm) ,(condition-code cond*))))]
      ;; TODO: aliases for these
      [(0 1 0 #b00) `(csinv ,(W Rd) ,(W Rn) ,(W Rm) ,(condition-code cond*))]
      [(0 1 0 #b01) `(csneg ,(W Rd) ,(W Rn) ,(W Rm) ,(condition-code cond*))]
      [(1 0 0 #b00) `(csel ,(X Rd) ,(X Rn) ,(X Rm) ,(condition-code cond*))]
      [(1 0 0 #b01) `(csinc ,(X Rd) ,(X Rn) ,(X Rm) ,(condition-code cond*))]
      [(1 1 0 #b00) `(csinv ,(X Rd) ,(X Rn) ,(X Rm) ,(condition-code cond*))]
      [(1 1 0 #b01) `(csneg ,(X Rd) ,(X Rn) ,(X Rm) ,(condition-code cond*))]))

  ;; C4.5.7
  (define-encoding (data-processing/1src pc instr (31 sf) (30 (= #b1)) (29 S) (28 (= #b11010110)) (20 opcode2)
                                         (15 opcode) (9 Rn) (4 Rd))
    (match (sf S opcode2 opcode)
      [(0 0 #b00000 #b000000) `(rbit ,(W Rd) ,(W Rn))]
      [(0 0 #b00000 #b000001) `(rev16 ,(W Rd) ,(W Rn))]
      [(0 0 #b00000 #b000010) `(rev ,(W Rd) ,(W Rn))]
      [(0 0 #b00000 #b000100) `(clz ,(W Rd) ,(W Rn))]
      [(0 0 #b00000 #b000101) `(cls ,(W Rd) ,(W Rn))]
      [(1 0 #b00000 #b000000) `(rbit ,(X Rd) ,(X Rn))]
      [(1 0 #b00000 #b000001) `(rev16 ,(X Rd) ,(X Rn))]
      [(1 0 #b00000 #b000010) `(rev32 ,(X Rd) ,(X Rn))]
      [(1 0 #b00000 #b000011) `(rev ,(X Rd) ,(X Rn))]
      [(1 0 #b00000 #b000100) `(clz ,(X Rd) ,(X Rn))]
      [(1 0 #b00000 #b000101) `(cls ,(X Rd) ,(X Rn))]))

  ;; C4.5.8
  (define-encoding (data-processing/2src pc instr (31 sf) (30 (= #b0)) (29 S) (28 (= #b11010110)) (20 Rm)
                                         (15 opcode) (9 Rn) (4 Rd))
    (match (sf S opcode)
      [(0 0 #b000010) `(udiv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b000011) `(sdiv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b001000) `(lslv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b001001) `(lsrv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b001010) `(asrv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b001011) `(rorv ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010000) `(crc32b ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010001) `(crc32h ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010010) `(crc32w ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010100) `(crc32cb ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010101) `(crc32ch ,(W Rd) ,(W Rn) ,(W Rm))]
      [(0 0 #b010110) `(crc32cw ,(W Rd) ,(W Rn) ,(W Rm))]
      [(1 0 #b000010) `(udiv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b000011) `(sdiv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b001000) `(lslv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b001001) `(lsrv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b001010) `(asrv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b001011) `(rorv ,(X Rd) ,(X Rn) ,(X Rm))]
      [(1 0 #b010011) `(crc32x ,(W Rd) ,(W Rn) ,(X Rm))]
      [(1 0 #b010111) `(crc32cx ,(W Rd) ,(W Rn) ,(X Rm))]))

  ;; C4.5.9
  (define-encoding (data-processing/3src pc instr (31 sf) (30 op54) (28 (= #b11011)) (23 op31) (20 Rm) (15 o0)
                                         (14 Ra) (9 Rn) (4 Rd))
    (match (sf op54 op31 o0 Ra)
      [(0 #b00 #b000 0 'bxxxxx) `(madd ,(W Rd) ,(W Rn) ,(W Rm) ,(W Ra))]
      [(0 #b00 #b000 1 'bxxxxx) `(msub ,(W Rd) ,(W Rn) ,(W Rm) ,(W Ra))]
      [(1 #b00 #b000 0 'bxxxxx) `(madd ,(X Rd) ,(X Rn) ,(X Rm) ,(X Ra))]
      [(1 #b00 #b000 1 'bxxxxx) `(madd ,(X Rd) ,(X Rn) ,(X Rm) ,(X Ra))]
      [(1 #b00 #b001 0 'bxxxxx) `(smaddl ,(X Rd) ,(W Rn) ,(W Rm) ,(X Ra))]
      [(1 #b00 #b001 1 'bxxxxx) `(smsubl ,(X Rd) ,(W Rn) ,(W Rm) ,(X Ra))]
      [(1 #b00 #b010 0 #b11111) `(smulh ,(X Rd) ,(W Rn) ,(W Rm))]
      [(1 #b00 #b101 0 'bxxxxx) `(umaddl ,(X Rd) ,(W Rn) ,(W Rm) ,(X Ra))]
      [(1 #b00 #b101 1 'bxxxxx) `(umsubl ,(X Rd) ,(W Rn) ,(W Rm) ,(X Ra))]
      [(1 #b00 #b110 0 #b11111) `(umulh ,(X Rd) ,(W Rn) ,(W Rm))]))

  ;; C4.5.10
  (define-encoding (logical/shifted-reg pc instr (31 sf) (30 opc) (28 (= #b01010)) (23 shift) (21 N) (20 Rm)
                                        (15 imm6) (9 Rn) (4 Rd))
    (match (sf opc N imm6)
      [(0 #b00 0 'b0xxxxx) `(and ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b00 1 'b0xxxxx) `(bic ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b01 0 'b0xxxxx) `(orr ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b01 1 'b0xxxxx) `(orn ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b10 0 'b0xxxxx) `(eor ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b10 1 'b0xxxxx) `(eon ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b11 0 'b0xxxxx) `(ands ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(0 #b11 1 'b0xxxxx) `(bics ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))]
      [(1 #b00 0 'bxxxxxx) `(and ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b00 1 'bxxxxxx) `(bic ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b01 0 'bxxxxxx) `(orr ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b01 1 'bxxxxxx) `(orn ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b10 0 'bxxxxxx) `(eor ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b10 1 'bxxxxxx) `(eon ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b11 0 'bxxxxxx) `(ands ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]
      [(1 #b11 1 'bxxxxxx) `(bics ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]))

;;; C4.6

  (define-encoding (data-processing/simd&fp pc instr (31 op0) (27 (= #b111)) (24 op1) (22 op2) (18 op3) (16)
                                            (15 op4) (9))
    (select pc instr
            adv-simd-across-lanes
            adv-simd-copy
            adv-simd-extract
            adv-simd-modified-imm
            adv-simd-permute
            adv-simd-scalar-copy
            adv-simd-scalar-pairwise
            adv-simd-scalar-shift-imm
            adv-simd-scalar-3-diff
            adv-simd-scalar-3-same
            adv-simd-scalar-2reg-misc
            adv-simd-scalar-x-idx-elem
            adv-simd-shift-by-imm
            adv-simd-table-lookup
            adv-simd-3-diff
            adv-simd-3-same
            adv-simd-2reg-misc
            adv-simd-vec-x-idx-elem
            crypto-aes
            crypto-3-reg-sha
            crypto-2-reg-sha
            fp-compare
            fp-cond-compare
            fp-cond-select
            fp-data-processing-1src
            fp-data-processing-2src
            fp-data-processing-3src
            fp-imm
            conv-fp<->fx
            conv-fp<->int))

  ;; C4.6.1
  (define-encoding (adv-simd-across-lanes pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size)
                                          (21 (= #b11000)) (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.2
  (define-encoding (adv-simd-copy pc instr (31 (= #b0)) (30 Q) (29 op) (28 (= #b01110000)) (20 imm5) (15 (= #b0))
                                  (14 imm4) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (Q op imm5 imm4)))

  ;; C4.6.3
  (define-encoding (adv-simd-extract pc instr (31 (= #b0)) (30 Q) (29 (= #b101110)) (23 op2) (21 (= #b0)) (20 Rm)
                                     (15 (= #b0)) (14 imm4) (10 (= #b0)) (9 Rn) (4 Rd))
    (match (op2)))

  ;; C4.6.4
  (define-encoding (adv-simd-modified-imm pc instr (31 (= #b0)) (30 Q) (29 op) (28 (= #b0111100000)) (18 a) (17 b)
                                          (16 c) (15 cmode) (11 o2) (10 (= #b1)) (9 d) (8 e) (7 f) (6 g) (5 h) (4 Rd))
    (match (Q op cmode o2)))

  ;; C4.6.5
  (define-encoding (adv-simd-permute pc instr (31 (= #b0)) (30 Q) (29 (= #b001110)) (23 size) (21 (= #b0)) (20 Rm)
                                     (15 (= #b0)) (14 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (opcode)))

  ;; C4.6.6
  (define-encoding (adv-simd-scalar-copy pc instr (31 (= #b01)) (29 op) (28 (= #b11110000)) (20 imm5) (15 (= #b0))
                                         (14 imm4) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (op imm5 imm4)))

  ;; C4.6.7
  (define-encoding (adv-simd-scalar-pairwise pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b11000))
                                             (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.8
  (define-encoding (adv-simd-scalar-shift-imm pc instr (31 (= #b01)) (29 U) (28 (= #b111110)) (22 immh) (18 immb)
                                              (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (U immh opcode)
      [(0 'b1xxx #b00000) `(sshr ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b00010) `(ssra ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b00100) `(srshr ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b00110) `(srsra ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b01010) `(shl ,(D Rd) ,(D Rn) ,(fx- (fxior (fxasl immh 3) immb) 64))]
      [(0 'b0001 #b01110) `(sqshl ,(B Rd) ,(B Rn) ,(fx- (fxior (fxasl immh 3) immb) 8))]
      [(0 'b001x #b01110) `(sqshl ,(H Rd) ,(H Rn) ,(fx- (fxior (fxasl immh 3) immb) 16))]
      [(0 'b01xx #b01110) `(sqshl ,(S Rd) ,(S Rn) ,(fx- (fxior (fxasl immh 3) immb) 32))]
      [(0 'b1xxx #b01110) `(sqshl ,(D Rd) ,(D Rn) ,(fx- (fxior (fxasl immh 3) immb) 64))]
      [(0 'b0001 #b10010) `(sqshrn ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(0 'b001x #b10010) `(sqshrn ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(0 'b01xx #b10010) `(sqshrn ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(0 'b0001 #b10011) `(sqrshrn ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(0 'b001x #b10011) `(sqrshrn ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(0 'b01xx #b10011) `(sqrshrn ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(0 'b01xx #b11100) `(scvtf ,(S Rd) ,(S Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b11100) `(scvtf ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(0 'b01xx #b11111) `(fcvtzs ,(S Rd) ,(S Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(0 'b1xxx #b11111) `(fcvtzs ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b00000) `(ushr ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b00010) `(usra ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b00100) `(urshr ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b00110) `(ursra ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b01000) `(sri ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b01010) `(sli ,(D Rd) ,(D Rn) ,(fx- (fxior (fxasl immh 3) immb) 64))]
      [(1 'b0001 #b01100) `(sqshlu ,(B Rd) ,(B Rn) ,(fx- (fxior (fxasl immh 3) immb) 8))]
      [(1 'b001x #b01100) `(sqshlu ,(H Rd) ,(H Rn) ,(fx- (fxior (fxasl immh 3) immb) 16))]
      [(1 'b01xx #b01100) `(sqshlu ,(S Rd) ,(S Rn) ,(fx- (fxior (fxasl immh 3) immb) 32))]
      [(1 'b1xxx #b01100) `(sqshlu ,(D Rd) ,(D Rn) ,(fx- (fxior (fxasl immh 3) immb) 64))]
      [(1 'b0001 #b01110) `(uqshl ,(B Rd) ,(B Rn) ,(fx- (fxior (fxasl immh 3) immb) 8))]
      [(1 'b001x #b01110) `(uqshl ,(H Rd) ,(H Rn) ,(fx- (fxior (fxasl immh 3) immb) 16))]
      [(1 'b01xx #b01110) `(uqshl ,(S Rd) ,(S Rn) ,(fx- (fxior (fxasl immh 3) immb) 32))]
      [(1 'b1xxx #b01110) `(uqshl ,(D Rd) ,(D Rn) ,(fx- (fxior (fxasl immh 3) immb) 64))]
      [(1 'b0001 #b10000) `(sqshrun ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(1 'b001x #b10000) `(sqshrun ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b10000) `(sqshrun ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b0001 #b10001) `(sqrshrun ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(1 'b001x #b10001) `(sqrshrun ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b10001) `(sqrshrun ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b0001 #b10010) `(uqshrn ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(1 'b001x #b10010) `(uqshrn ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b10010) `(uqshrn ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b0001 #b10011) `(uqrshrn ,(B Rd) ,(H Rn) ,(fx- 16 (fxior (fxasl immh 3) immb)))]
      [(1 'b001x #b10011) `(uqrshrn ,(H Rd) ,(S Rn) ,(fx- 32 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b10011) `(uqrshrn ,(S Rd) ,(D Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b11100) `(ucvtf ,(S Rd) ,(S Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b11100) `(ucvtf ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]
      [(1 'b01xx #b11111) `(fcvtzu ,(S Rd) ,(S Rn) ,(fx- 64 (fxior (fxasl immh 3) immb)))]
      [(1 'b1xxx #b11111) `(fcvtzu ,(D Rd) ,(D Rn) ,(fx- 128 (fxior (fxasl immh 3) immb)))]))

  ;; C4.6.9
  (define-encoding (adv-simd-scalar-3-diff pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b1))
                                           (20 Rm) (15 opcode) (11 (= #b00)) (9 Rn) (4 Rd))
    (match (U opcode)))

  ;; C4.6.10
  (define-encoding (adv-simd-scalar-3-same pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b1))
                                           (20 Rm) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.11
  (define-encoding (adv-simd-scalar-2reg-misc pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b10000))
                                              (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.12
  (define-encoding (adv-simd-scalar-x-idx-elem pc instr (31 (= #b01)) (29 U) (28 (= #b11111)) (23 size) (21 L) (20 M)
                                               (19 Rm) (15 opcode) (11 H) (10 (= #b0)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.13
  (define-encoding (adv-simd-shift-by-imm pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b011110)) (22 (!= #b0000) immh)
                                          (18 immb) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (U opcode)))

  ;; C4.6.14
  (define-encoding (adv-simd-table-lookup pc instr (31 (= #b0)) (30 Q) (29 (= #b001110)) (23 op2) (21 (= #b0))
                                          (20 Rm) (15 (= #b0)) (14 len) (12 op) (11 (= #b00)) (9 Rn) (4 Rd))
    (match (op2 len op)))

  ;; C4.6.15
  (define-encoding (adv-simd-3-diff pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 (= #b1))
                                    (20 Rm) (15 opcode) (11 (= #b00)) (9 Rn) (4 Rd))
    (match (U opcode)))

  ;; C4.6.16
  (define-encoding (adv-simd-3-same pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 ( = #b1))
                                    (20 Rm) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.17
  (define-encoding (adv-simd-2reg-misc pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 (= #b10000))
                                       (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.18
  (define-encoding (adv-simd-vec-x-idx-elem pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01111)) (23 size) (21 L)
                                            (20 M) (19 Rm) (15 opcode) (11 H) (10 (= #b0)) (9 Rn) (4 Rd))
    (match (U size opcode)))

  ;; C4.6.19
  (define-encoding (crypto-aes pc instr (31 (= #b01001110)) (23 size) (21 (= #b10100)) (16 opcode) (11 (= #b10))
                               (9 Rn) (4 Rd))
    (match (size opcode)
      [(#b00 #b00100) `(aese ,(V Rd 16 'B) ,(V Rn 16 'B))]
      [(#b00 #b00101) `(aesd ,(V Rd 16 'B) ,(V Rn 16 'B))]
      [(#b00 #b00110) `(aesmc ,(V Rd 16 'B) ,(V Rn 16 'B))]
      [(#b00 #b00111) `(aesimc ,(V Rd 16 'B) ,(V Rn 16 'B))]))

  ;; C4.6.20
  (define-encoding (crypto-3-reg-sha pc instr (31 (= #b01011110)) (23 size) (21 (= #b0)) (20 Rm) (15 (= #b0))
                                     (14 opcode) (11 (= #b00)) (9 Rn) (4 Rd))
    (match (size opcode)
      [(#b00 #b000) `(sha1c ,(Q Rd) ,(S Rd) ,(V Rm 4 'S))]
      [(#b00 #b001) `(sha1p ,(Q Rd) ,(S Rd) ,(V Rm 4 'S))]
      [(#b00 #b010) `(sha1m ,(Q Rd) ,(S Rd) ,(V Rm 4 'S))]
      [(#b00 #b011) `(sha1su0 ,(V Rd 4 'S) ,(V Rn 4 'S) ,(V Rm 4 'S))]
      [(#b00 #b100) `(sha256h ,(Q Rd) ,(Q Rd) ,(V Rm 4 'S))]
      [(#b00 #b101) `(sha256h2 ,(Q Rd) ,(Q Rd) ,(V Rm 4 'S))]
      [(#b00 #b110) `(sha256su1 ,(V Rd 4 'S) ,(V Rn 4 'S) ,(V Rm 4 'S))]))

  ;; C4.6.21
  (define-encoding (crypto-2-reg-sha pc instr (31 (= #b01011110)) (23 size) (21 (= #b10100)) (16 opcode) (11 (= #b10))
                                     (9 Rn) (4 Rd))
    (match (size opcode)
      [(#b00 #b00000) `(sha1h ,(S Rd) ,(S Rn))]
      [(#b00 #b00001) `(sha1su1 ,(V Rd 4 'S) ,(V Rn 4 'S))]
      [(#b00 #b00010) `(sha256su0 ,(V Rd 4 'S) ,(V Rn 4 'S))]))

  ;; C4.6.22
  (define-encoding (fp-compare pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1)) (20 Rm)
                               (15 op) (13 (= #b1000)) (9 Rn) (4 opcode2))
    (match (M S type op opcode2)
      [(0 0 #b00 #b00 #b00000) (fcmp pc instr)]
      [(0 0 #b00 #b00 #b01000) (fcmp pc instr)]
      [(0 0 #b00 #b00 #b10000) (fcmpe pc instr)]
      [(0 0 #b00 #b00 #b11000) (fcmpe pc instr)]
      [(0 0 #b01 #b00 #b00000) (fcmp pc instr)]
      [(0 0 #b01 #b00 #b01000) (fcmp pc instr)]
      [(0 0 #b01 #b00 #b10000) (fcmpe pc instr)]
      [(0 0 #b01 #b00 #b11000) (fcmpe pc instr)]))

  (define-encoding (fcmp pc instr (31 (= #b00011110)) (23 type) (21 (= #b1)) (20 Rm) (15 (= #b001000))
                         (9 Rn) (4 opc) (2 (= #b000)))
    (match (type opc Rm)
      [(#b00 #b00 'bxxxxx) `(fcmp ,(S Rn) ,(S Rm))]
      [(#b00 #b01 #b00000) `(fcmp ,(S Rn) #i0.0)]
      [(#b01 #b00 'bxxxxx) `(fcmp ,(D Rn) ,(D Rm))]
      [(#b01 #b01 #b00000) `(fcmp ,(D Rn) #i0.0)]))

  (define-encoding (fcmpe pc instr (31 (= #b00011110)) (23 type) (21 (= #b1)) (20 Rm) (15 (= #b001000))
                          (9 Rn) (4 opc) (2 (= #b000)))
    (match (type opc Rm)
      [(#b00 #b10 'bxxxxx) `(fcmpe ,(S Rn) ,(S Rm))]
      [(#b00 #b11 #b00000) `(fcmpe ,(S Rn) #i0.0)]
      [(#b01 #b10 'bxxxxx) `(fcmpe ,(D Rn) ,(D Rm))]
      [(#b01 #b11 #b00000) `(fcmpe ,(D Rn) #i0.0)]))

  ;; C4.6.23
  (define-encoding (fp-cond-compare pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                    (20 Rm) (15 cond*) (11 (= #b01)) (9 Rn) (4 op) (3 nzcv))
    (match (M S type op)))

  ;; C4.6.24
  (define-encoding (fp-cond-select pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                   (20 Rm) (15 cond*) (11 (= #b11)) (9 Rn) (4 Rd))
    (match (M S type)))

  ;; C4.6.25
  (define-encoding (fp-data-processing-1src pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                            (20 opcode) (14 (= #b10000)) (9 Rn) (4 Rd))
    (match (M S type opcode)))

  ;; C4.6.26
  (define-encoding (fp-data-processing-2src pc instr (31 M) (30 (= #b0)) (29 S*) (28 (= #b11110)) (23 type) (21 (= #b1))
                                            (20 Rm) (15 opcode) (11 (= #b10)) (9 Rn) (4 Rd))
    (match (M S* type opcode)
      [(0 0 #b00 #b0000) `(fmul ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0001) `(fdiv ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0010) `(fadd ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0011) `(fsub ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0100) `(fmax ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0101) `(fmin ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0110) `(fmaxnm ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b0111) `(fminnm ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b00 #b1000) `(fnmul ,(S Rd) ,(S Rn) ,(S Rm))]
      [(0 0 #b01 #b0000) `(fmul ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0001) `(fdiv ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0010) `(fadd ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0011) `(fsub ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0100) `(fmax ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0101) `(fmin ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0110) `(fmaxnm ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b0111) `(fminnm ,(D Rd) ,(D Rn) ,(D Rm))]
      [(0 0 #b01 #b1000) `(fnmul ,(D Rd) ,(D Rn) ,(D Rm))]))

  ;; C4.6.27
  (define-encoding (fp-data-processing-3src pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11111)) (23 type) (21 o1)
                                            (20 Rm) (15 o0) (14 Ra) (9 Rn) (4 Rd))
    (match (M S type o1 o0)))

  ;; C4.6.28
  (define-encoding (fp-imm pc instr (31 M) (30 (= #b0)) (29 S*) (28 (= #b11110)) (23 type) (21 (= #b1)) (20 imm8)
                           (12 (= #b100)) (9 imm5) (4 Rd))
    (match (M S* type imm5)
      [(0 0 #b00 #b00000) `(fmov ,(S Rd) ,(vfp-expand-imm imm8))]
      [(0 0 #b01 #b00000) `(fmov ,(X Rd) ,(vfp-expand-imm imm8))]))

  ;; C4.6.29
  (define-encoding (conv-fp<->fx pc instr (31 sf) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b0))
                                 (20 rmode) (18 opcode) (15 scale) (9 Rn) (4 Rd))
    (match (sf S type rmode opcode scale)))

  ;; C4.6.30
  (define-encoding (conv-fp<->int pc instr (31 sf) (30 (= #b0)) (29 S*) (28 (= #b11110)) (23 type) (21 (= #b1))
                                  (20 rmode) (18 opcode) (15 (= #b000000)) (9 Rn) (4 Rd))
    (match (sf S* type rmode opcode)
      [(0 0 #b00 #b00 #b000) `(fcvtns ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b00 #b001) `(fcvtnu ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b00 #b010) `(scvtf ,(S Rd) ,(W Rn))]
      [(0 0 #b00 #b00 #b011) `(ucvtf ,(S Rd) ,(W Rn))]
      [(0 0 #b00 #b00 #b100) `(fcvtas ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b00 #b101) `(fcvtau ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b00 #b110) `(fmov ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b00 #b111) `(fmov ,(S Rd) ,(W Rn))]
      [(0 0 #b00 #b01 #b000) `(fcvtps ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b01 #b001) `(fcvtpu ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b10 #b000) `(fcvtms ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b10 #b001) `(fcvtmu ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b11 #b000) `(fcvtzs ,(W Rd) ,(S Rn))]
      [(0 0 #b00 #b11 #b001) `(fcvtzu ,(W Rd) ,(S Rn))]
      [(0 0 #b01 #b00 #b000) `(fcvtns ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b00 #b001) `(fcvtnu ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b00 #b010) `(scvtf ,(D Rd) ,(W Rn))]
      [(0 0 #b01 #b00 #b011) `(ucvtf ,(D Rd) ,(W Rn))]
      [(0 0 #b01 #b00 #b100) `(fcvtas ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b00 #b101) `(fcvtau ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b01 #b000) `(fcvtps ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b01 #b001) `(fcvtpu ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b10 #b000) `(fcvtms ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b10 #b001) `(fcvtmu ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b11 #b000) `(fcvtzs ,(W Rd) ,(D Rn))]
      [(0 0 #b01 #b11 #b001) `(fcvtzu ,(W Rd) ,(D Rn))]
      [(1 0 #b00 #b00 #b000) `(fcvtns ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b00 #b001) `(fcvtnu ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b00 #b010) `(scvtf ,(S Rd) ,(X Rn))]
      [(1 0 #b00 #b00 #b011) `(ucvtf ,(S Rd) ,(X Rn))]
      [(1 0 #b00 #b00 #b100) `(fcvtas ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b00 #b101) `(fcvtau ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b01 #b000) `(fcvtps ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b01 #b001) `(fcvtpu ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b10 #b000) `(fcvtms ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b10 #b001) `(fcvtmu ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b11 #b000) `(fcvtzs ,(X Rd) ,(S Rn))]
      [(1 0 #b00 #b11 #b001) `(fcvtzu ,(X Rd) ,(S Rn))]
      [(1 0 #b01 #b00 #b000) `(fcvtns ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b00 #b001) `(fcvtnu ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b00 #b010) `(scvtf ,(D Rd) ,(X Rn))]
      [(1 0 #b01 #b00 #b011) `(ucvtf ,(D Rd) ,(X Rn))]
      [(1 0 #b01 #b00 #b100) `(fcvtas ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b00 #b101) `(fcvtau ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b00 #b110) `(fmov ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b00 #b111) `(fmov ,(D Rd) ,(X Rn))]
      [(1 0 #b01 #b01 #b000) `(fcvtps ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b01 #b001) `(fcvtpu ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b10 #b000) `(fcvtms ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b10 #b001) `(fcvtmu ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b11 #b000) `(fcvtzs ,(X Rd) ,(D Rn))]
      [(1 0 #b01 #b11 #b001) `(fcvtzu ,(X Rd) ,(D Rn))]
      [(1 0 #b10 #b01 #b110) `(fmov ,(X Rd) ,(V-ref Rn 'D 1))]
      [(1 0 #b10 #b01 #b111) `(fmov ,(V-ref Rn 'D 1) ,(X Rd))]))

;;; Common API

  (define (decode pc instruction bytes collect)
    (apply collect 'opcode bytes)
    (or (main pc instruction)
        (raise-UD "Unallocated opcode")))

  (define (dummy-collect tag . bytes)
    #f)

  (define (get-u32 port)
    (let ((bv (get-bytevector-n port 4)))
      (and (bytevector? bv)
           (fx=? (bytevector-length bv) 4)
           bv)))

  (define (get-instruction port collect pc)
    (cond ((get-u32 port) =>
           (lambda (bv)
             (let ((instruction (bytevector-u32-ref bv 0 (endianness little))))
               (decode pc instruction
                       (bytevector->u8-list bv)
                       (or collect dummy-collect)))))
          (else
           (eof-object))))

  ;; Generic disassembler support.
  (let ((min 4) (max 4))
    (define (wrap-get-instruction)
      (define get-instruction*
        (case-lambda
          ((port)
           (get-instruction port #f #f))
          ((port collect)
           (get-instruction port collect #f))
          ((port collect pc)
           (get-instruction port collect pc))))
      get-instruction*)
    (register-disassembler
     (make-disassembler 'arm-a64 min max (wrap-get-instruction)))))
