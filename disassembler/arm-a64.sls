;; -*- mode: scheme; coding: utf-8 -*-
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
  (export get-instruction)
  (import (rnrs)
          (machine-code disassembler arm-private)
          (machine-code disassembler private))

  (define-syntax print
    (syntax-rules ()
      #;
      ((_ . args) (begin (for-each display (list . args)) (newline)))
      ((_ . args) (begin 'dummy))))

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
  (define (W/WSP n) (vector-ref W/WSP-registers n))
  (define X/SP-registers
    '#(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
          x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 sp))
  (define (X/SP n) (vector-ref X/SP-registers n))
  (define W-registers
    '#(w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17
          w18 w19 w20 w21 w22 w23 w24 w25 w26 w27 w28 w29 w30 wzr))
  (define (W n) (vector-ref W-registers n))
  (define X-registers
    '#(x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
          x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 xzr))
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

  ;; SIMD vector registers
  (define Vn.8B-registers)

;;; Various utilities

  (define (lsl v shift)
    (if (and (integer? v) (fixnum? shift))
        (bitwise-arithmetic-shift-left v shift)
        `(lsl ,v ,shift)))

  (define (pc-rel pc offset)
    (if pc
        (+ pc offset)
        `(+ pc ,offset)))

  (define (pc-rel-page pc offset)
    (if pc
        (pc-rel (bitwise-and pc (bitwise-not 4095)) offset)
        (pc-rel `(bitwise-and pc ,(bitwise-not 4095)) offset)))

  ;; Pure memory reference
  (define (mem+ reg offset)
    (if (eqv? offset 0) `((mem+ ,reg)) `((mem+ ,reg ,offset))))

  ;; Memory reference that afterwards increments the register.
  (define (mempost+ reg offset)
    (if (eqv? offset 0) `((mempost+ ,reg)) `((mempost+ ,reg ,offset))))

  ;; Memory reference that before the reference increments the register.
  (define (mempre+ reg offset)
    (if (eqv? offset 0) `((mempre+ ,reg)) `((mempre+ ,reg ,offset))))

  ;; Decodes bit masks. Returns two interpretations: bitfield and logical immediate.
  (define (decode-bit-masks width N imms immr immediate?)
    (define (replicate x from to)
      (if (fx<? from to)
          (replicate (bitwise-ior x (bitwise-arithmetic-shift-left x from))
                     (fx* from 2)
                     to)
          (bitwise-bit-field x 0 to)))
    (let ((len (fx- (fxlength (fxior (fxarithmetic-shift-left N 6) (fxxor imms #b111111)))
                    1)))
      (when (fx<? len 0)
        (raise-UD "Reserved len value in bit masks"))
      (assert (>= width (fxarithmetic-shift-left 1 len)))
      (let ((levels (fx- (fxarithmetic-shift-left 1 len) 1)))
        (when (and immediate? (fx=? (fxand imms levels) levels))
          (raise-UD "Reserved S value in bit masks"))
        (let ((S (fxand imms levels))
              (R (fxand immr levels))
              (esize (fxarithmetic-shift-left 1 len)))
          (values
            ;; Something like ror((1 << (S + 1)) - 1, R), replicated.
            (let* ((welem (fx- (fxarithmetic-shift-left 1 (fx+ S 1)) 1))
                   (wmask (replicate (bitwise-rotate-bit-field welem 0 esize (fx- esize R))
                                     esize width)))
              wmask)
            ;; Something like (1 << (S - R)) - 1, replicated.
            (let* ((diff (fxbit-field (fx- S R) 0 len))
                   (telem (fx- (fxarithmetic-shift-left 1 (fx+ diff 1)) 1))
                   (tmask (replicate telem esize width)))
              tmask))))))

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

  (define (decode-reg-extend option)
    (vector-ref '#(uxtb uxth uxtw uxtx sxtb sxth sxtw sxtx) option))

;;; Decode tables

  (define-encoding (main pc instr (31) (28 op0) (24))
    (select pc instr
            unallocated
            data-processing/imm
            branch/exception/system
            loads/stores
            data-processing/reg
            data-processing/simd&fp))

  (define-encoding (unallocated pc instr (31) (28 (= #b00)) (26))
    (decode
     [(and (= instr 0))
      `(%u32 ,instr)]))                 ;it's probably padding

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
      ((#b01) (fxarithmetic-shift-left imm12 12))
      (else (raise-UD "Reserved shift value"))))

  ;; C4.2.1 TODO: There are aliases
  (define-encoding (add/subtract pc instr (31 sf) (30 op) (29 S) (28 (= #b10001)) (23 shift) (21 imm12) (9 Rn) (4 Rd))
    (decode
     [(and (= sf #b0) (= op #b0) (= S #b0)) `(add ,(W/WSP Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b0) (= op #b0) (= S #b1)) `(adds ,(W Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b0) (= op #b1) (= S #b0)) `(sub ,(W/WSP Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b0) (= op #b1) (= S #b1)) `(subs ,(W Rd) ,(W/WSP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b1) (= op #b0) (= S #b0)) `(add ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b1) (= op #b0) (= S #b1)) `(adds ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b1) (= op #b1) (= S #b0)) `(sub ,(X/SP Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]
     [(and (= sf #b1) (= op #b1) (= S #b1)) `(subs ,(X Rd) ,(X/SP Rn) ,(page-immediate imm12 shift))]))

  ;; C4.2.2
  (define-encoding (bitfield pc instr (31 sf) (30 opc) (28 (= #b100110)) (22 N) (21 immr) (15 imms) (9 Rn) (4 Rd))
    )

  ;; C4.2.3
  (define-encoding (extract pc instr (31 sf) (30 op21) (28 (= #b100111)) (22 N) (21 o0) (20 Rm) (15 imms) (9 Rn) (4 Rd))
    )

  ;; C4.2.4
  (define-encoding (logical/imm pc instr (31 sf) (30 opc) (28 (= #b100100)) (22 N) (21 immr) (15 imms) (9 Rn) (4 Rd))
    (decode
     [(and (= sf #b0) (= opc #b11) (= N #b0))
      (let-values (((imm _) (decode-bit-masks 32 N imms immr #t)))
        (if (= Rd #b11111)
            `(tst ,(W Rn) ,imm)
            `(ands ,(W Rd) ,(W Rn) ,imm)))]
     [(and (= sf #b1) (= opc #b11))
      (let-values (((imm _) (decode-bit-masks 64 N imms immr #t)))
        (if (= Rd #b11111)
            `(tst ,(X Rn) ,imm)
            `(ands ,(X Rd) ,(W Rn) ,imm)))]))

  ;; C4.2.5
  (define-encoding (move-wide/imm pc instr (31 sf) (30 opc) (28 (= #b100101)) (22 hw) (20 imm16) (4 Rd))
    ;; TODO: aliases
    (decode
     [(and (= sf #b0) (= opc #b00)) `(movn ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]
     [(and (= sf #b0) (= opc #b10)) `(movz ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]
     [(and (= sf #b0) (= opc #b11)) `(movk ,(W/WSP Rd) ,(lsl imm16 (* hw 16)))]

     [(and (= sf #b1) (= opc #b00)) `(movn ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]
     [(and (= sf #b1) (= opc #b10)) `(movz ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]
     [(and (= sf #b1) (= opc #b11)) `(movk ,(X/SP Rd) ,(lsl imm16 (* hw 16)))]))

  ;; C4.2.6
  (define-encoding (pc-rel-addr pc instr (31 op) (30 immlo) (28 (= #b10000)) (23 immhi) (4 Rd))
    (decode
     [(= op #b0)
      `(adr ,(X Rd) ,(pc-rel pc (fxior (fxarithmetic-shift-left immhi 2) immlo)))]
     [(= op #b1)
      `(adrp ,(X Rd)
             ,(pc-rel-page pc (bitwise-arithmetic-shift-left (fxior (fxarithmetic-shift-left immhi 2) immlo) 12)))]))

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
    (decode
     [(and (= sf #b0) (= op #b0))
      `(cbz ,(W Rt) (+ pc ,(* imm19 4)))]
     [(and (= sf #b0) (= op #b1))
      `(cbnz ,(W Rt) (+ pc ,(* imm19 4)))]
     [(and (= sf #b1) (= op #b0))
      `(cbnz ,(X Rt) (+ pc ,(* imm19 4)))]
     [(and (= sf #b1) (= op #b1))
      `(cbnz ,(X Rt) (+ pc ,(* imm19 4)))]))

  ;; C4.3.2
  (define-encoding (cond-branch/imm pc instr (31 (= #b0101010)) (24 o1) (23 imm19) (4 o0) (3 cond*))
    (decode
     [(and (= o1 #b0) (= o0 #b0))
      `(,(string->symbol (string-append "b." (symbol->string (vector-ref condition-codes cond*))))
        ,(pc-rel pc (bitwise-arithmetic-shift-left (sign-extend imm19 19) 2)))]))

  ;; C4.3.3
  (define-encoding (exception pc instr (31 (= #b11010100)) (23 opc) (20 imm16) (4 op2) (1 LL))
    )

  ;; C4.3.4
  (define-encoding (system pc instr (31 (= #b1101010100)) (21 L) (20 op0) (18 op1) (15 CRn) (11 CRm) (7 op2) (4 Rt))
    (decode
     [(and (= L #b0) (= op0 #b00) (= op1 #b011) (= CRn #b0010) (= CRm #b0000)
           (= op2 #b000) (= Rt #b11111))
      '(nop)]))

  ;; C4.3.5
  (define-encoding (test&branch/imm pc instr (31 b5) (30 (= #b011011)) (24 op) (23 b40) (18 imm14) (4 Rt))
    (decode
     [(= op #b0)
      (let ((R (if (eqv? b5 #b1) X W)))
        `(tbz ,(R Rt) ,(fxior (fxarithmetic-shift-left b5 6) b40) (pc-rel pc ,(fxarithmetic-shift-left imm14 2))))]
     [(= op #b1)
      (let ((R (if (eqv? b5 #b1) X W)))
        `(tbnz ,(R Rt) ,(fxior (fxarithmetic-shift-left b5 6) b40) (pc-rel pc ,(fxarithmetic-shift-left imm14 2))))]))

  ;; C4.3.6
  (define-encoding (uncond-branch/imm pc instr (31 op) (30 (= #b00101)) (25 imm26))
    (decode
     [(= op #b0)
      `(b ,(pc-rel pc (fx* (sign-extend imm26 26) 4)))]
     [(= op #b1)
      `(bl ,(pc-rel pc (fx* (sign-extend imm26 26) 4)))]))

  ;; C4.3.7
  (define-encoding (uncond-branch/reg pc instr (31 (= #b1101011)) (24 opc) (20 op2) (15 op3) (9 Rn) (4 op4))
    (decode
     [(and (= opc #b0010) (= op2 #b11111) (= op3 #b000000) (= op4 #b00000))
      (if (eqv? Rn 30)
          '(ret)
          `(ret ,(X Rn)))]))

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
                                              (21 (= #b000000)) (15 opcode) (11 size) (9 Rn) (4 Rt)))

  ;; C4.4.2
  (define-encoding (load/store-adv-simd-multi-postidx pc instr (31 (= #b0)) (30 Q) (29 (= #b0011001)) (22 L)
                                                      (21 (= #b0)) (20 Rm) (15 opcode) (11 size) (9 Rn) (4 Rt)))

  ;; C4.4.3
  (define-encoding (load/store-adv-simd-single pc instr (31 (= #b0)) (30 Q) (29 (= #b0011010)) (22 L) (21 R)
                                               (20 (= #b00000)) (15 opcode) (12 S) (11 size) (9 Rn) (4 Rt)))

  ;; C4.4.4
  (define-encoding (load-store/adv-simd-single-postidx pc instr (31 (= #b0)) (30 Q) (29 (= #b0011011)) (22 L) (21 R)
                                                       (20 Rm) (15 opcode) (12 S) (11 size) (9 Rn) (4 Rt)))

  ;; C4.4.5
  (define-encoding (load-register-literal pc instr (31 opc) (29 (= #b011)) (26 V) (25 (= #b00)) (23 imm19) (4 Rt)))

  ;; C4.4.6
  (define-encoding (load/store-exclusive pc instr (31 size) (29 (= #b001000)) (23 o2) (22 L) (21 o1) (20 Rs) (15 o0)
                                         (14 Rt2) (9 Rn) (4 Rt)))

  ;; C4.4.7
  (define-encoding (load/store-no-alloc-pair/offset pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b000)) (22 L)
                                                    (21 imm7) (14 Rt2) (9 Rn) (4 Rt)))

  ;; C4.4.8
  (define-encoding (load/store-reg/imm-postidx pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (22 opc)
                                               (21 (= #b0)) (20 imm9) (11 (= #b01)) (9 Rn) (4 Rt)))

  ;; C4.4.9
  (define-encoding (load/store-reg/imm-preidx pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                              (21 (= #b0)) (20 imm9) (11 (= #b11)) (9 Rn) (4 Rt)))

  ;; C4.4.10
  (define-encoding (load/store-reg/reg-offset pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                              (21 (= #b1)) (20 Rm) (15 option) (12 S) (11 (= #b10)) (9 Rn) (4 Rt))
    (decode
     [(and (= size #b10) (= V #b0) (= opc #b01))
      (if (not (eqv? (fxand option #b010) #b010))
          (raise-UD "Unallocated in load/store-reg/reg-offset" (list 'option option))
          (let ((R ((if (fxbit-set? option 0) X W) Rm)))
            `(ldr ,(X Rt)
                  ,@(mem+ (X/SP Rn)
                          (case option
                            ((#b011) (lsl R (fx* size S)))
                            (else `(,(decode-reg-extend option) ,R ,(fx* size S))))))))]))

  ;; C4.4.11
  (define-encoding (load/store-reg/unprivileged pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                                (21 (= #b0)) (20 imm9) (11 (= #b10)) (9 Rn) (4 Rt)))

  ;; C4.4.12
  (define-encoding (load/store-reg/unscaled-imm pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b00)) (23 opc)
                                                (21 (= #b0)) (20 imm9) (11 (= #b00)) (9 Rn) (4 Rt))
    (decode
     [(and (= size #b10) (= V #b0) (= opc #b00))
      `(stur ,(W Rt) ,@(mem+ (X/SP Rn) (sign-extend imm9 9)))]))

  ;; C4.4.13
  (define-encoding (load/store-reg/unsigned-imm pc instr (31 size) (29 (= #b111)) (26 V) (25 (= #b01)) (23 opc)
                                                (21 imm12) (9 Rn) (4 Rt))
    (decode
     [(and (= size #b00) (= V #b0) (= opc #b00)) `(strb ,(W Rt) ,@(mem+ (X/SP Rn) imm12))]

     [(and (= size #b10) (= V #b0) (= opc #b00)) `(str ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
     [(and (= size #b10) (= V #b0) (= opc #b01)) `(ldr ,(W Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
     [(and (= size #b11) (= V #b0) (= opc #b00)) `(str ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]
     [(and (= size #b11) (= V #b0) (= opc #b01)) `(ldr ,(X Rt) ,@(mem+ (X/SP Rn) (lsl imm12 size)))]))

  ;; C4.4.14
  (define-encoding (load/store-regpair/offset pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b010)) (22 L) (21 imm7)
                                              (14 Rt2) (9 Rn) (4 Rt))
    (decode
     [(and (= opc #b10) (= V 0) (= L 0))
      `(stp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fx* (sign-extend imm7 7) 8)))]
     [(and (= opc #b10) (= V 0) (= L 1))
      `(ldp ,(X Rt) ,(X Rt2) ,@(mem+ (X/SP Rn) (fx* (sign-extend imm7 7) 8)))]))

  ;; C4.4.15
  (define-encoding (load/store-regpair/postidx pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b001)) (22 L) (21 imm7)
                                               (14 Rt2) (9 Rn) (4 Rt))
    (decode
     [(and (= opc #b10) (= V 0) (= L 1))
      `(ldp ,(X Rt) ,(X Rt2) ,@(mempost+ (X/SP Rn) (fx* (sign-extend imm7 7) 8)))]))

  ;; C4.4.16
  (define-encoding (load/store-regpair/preidx pc instr (31 opc) (29 (= #b101)) (26 V) (25 (= #b011)) (22 L) (21 imm7)
                                              (14 Rt2) (9 Rn) (4 Rt))
    (decode
     [(and (= opc #b10) (= V 0) (= L 0))
      `(stp ,(X Rt) ,(X Rt2) ,@(mempre+ (X/SP Rn) (fx* (sign-extend imm7 7) 8)))]))

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
  (define-encoding (add/sub-ext-reg pc instr (31 sf) (30 op) (28 (= #b01011)) (23 opt) (21 (= #b1)) (20 Rm) (15 option)
                                    (12 imm3) (9 Rn) (4 Rd)))

  ;; C4.5.2
  (define-encoding (add/sub-shift-reg pc instr (31 sf) (30 op) (29 S) (28 (= #b01011)) (23 shift) (21 (= #b0))
                                      (20 Rm) (15 imm6) (9 Rn) (4 Rd))
    ;; TODO: aliases
    (decode
     [(and (= sf #b0) (= op #b1) (= S #b1))
      `(subs ,(W Rd) ,(W Rn) ,(decode-shift/no-ror shift (W Rm) imm6))]
     [(and (= sf #b1) (= op #b1) (= S #b1))
      `(subs ,(X Rd) ,(X Rn) ,(decode-shift/no-ror shift (X Rm) imm6))]))

  ;; C4.5.3
  (define-encoding (add/sub-with-carry pc instr (31 sf) (30 op) (29 S) (28 (= #b11010000)) (20 Rm)
                                       (15 opcode2) (9 Rn) (4 Rd)))

  ;; C4.5.4
  (define-encoding (cond-compare/imm pc instr (31 sf) (30 op) (28 (= #b11010010)) (20 imm5) (15 cond*) (11 (= #b1))
                                     (10 o2) (9 Rn) (4 o3) (3 nzcv)))

  ;; C4.5.5
  (define-encoding (cond-compare/reg pc instr (31 sf) (30 op) (29 S) (28 (= #b11010010)) (20 Rm) (15 cond*) (11 (= #b0))
                                     (10 o2) (9 Rn) (4 o3) (3 nzcv)))

  ;; C4.5.6
  (define-encoding (conditional-select pc instr (31 sf) (30 op) (29 S) (28 (= #b11010100)) (20 Rm) (15 cond*) (11 op2)
                                       (9 Rn) (4 Rd)))

  ;; C4.5.7
  (define-encoding (data-processing/1src pc instr (31 sf) (30 (= #b1)) (29 S) (28 (= #b11010110)) (20 opcode2)
                                         (15 opcode) (9 Rn) (4 Rd)))

  ;; C4.5.8
  (define-encoding (data-processing/2src pc instr (31 sf) (30 (= #b0)) (29 S) (28 (= #b11010110)) (20 Rm)
                                         (15 opcode) (9 Rn) (4 Rd)))

  ;; C4.5.9
  (define-encoding (data-processing/3src pc instr (31 sf) (30 op54) (28 (= #b11011)) (23 op31) (20 Rm) (15 o0)
                                         (14 Ra) (9 Rn) (4 Rd)))

  ;; C4.5.10
  (define-encoding (logical/shifted-reg pc instr (31 sf) (30 opc) (28 (= #b01010)) (23 shift) (21 N) (20 Rm)
                                        (15 imm6) (9 Rn) (4 Rd))
    (decode
     [(and (= sf 0) (= opc #b01) (= N 0))
      (cond ((and (= shift #b00) (= imm6 #b000000) (= Rn #b11111))
             `(mov ,(W Rd) ,(W Rm)))
            (else
             `(orr ,(W Rd) ,(W Rn) ,(decode-shift shift (W Rm) imm6))))]
     [(and (= sf 1) (= opc #b01) (= N 0))
      (cond ((and (= shift #b00) (= imm6 #b000000) (= Rn #b11111))
             `(mov ,(X Rd) ,(X Rm)))
            (else
             `(orr ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))))]

     [(and (= sf 1) (= opc #b10) (= N 0))
      ;; TODO: shift can check that imm6<5> is valid if it knows the size
      `(eor ,(X Rd) ,(X Rn) ,(decode-shift shift (X Rm) imm6))]))

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
                                          (21 (= #b11000)) (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.2
  (define-encoding (adv-simd-copy pc instr (31 (= #b0)) (30 Q) (29 op) (28 (= #b01110000)) (20 imm5) (15 (= #b0))
                                  (14 imm4) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.3
  (define-encoding (adv-simd-extract pc instr (31 (= #b0)) (30 Q) (29 (= #b101110)) (23 op2) (21 (= #b0)) (20 Rm)
                                     (15 (= #b0)) (14 imm4) (10 (= #b0)) (9 Rn) (4 Rd)))

  ;; C4.6.4
  (define-encoding (adv-simd-modified-imm pc instr (31 (= #b0)) (30 Q) (29 op) (28 (= #b0111100000)) (18 a) (17 b)
                                          (16 c) (15 cmode) (11 o2) (10 (= #b1)) (9 d) (8 e) (7 f) (6 g) (5 f) (4 Rd)))

  ;; C4.6.5
  (define-encoding (adv-simd-permute pc instr (31 (= #b0)) (30 Q) (29 (= #b001110)) (23 size) (21 (= #b0)) (20 Rm)
                                     (15 (= #b0)) (14 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.6
  (define-encoding (adv-simd-scalar-copy pc instr (31 (= #b01)) (29 op) (28 (= #b11110000)) (20 imm5) (15 (= #b0))
                                         (14 imm4) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.7
  (define-encoding (adv-simd-scalar-pairwise pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b11000))
                                             (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.8
  (define-encoding (adv-simd-scalar-shift-imm pc instr (31 (= #b01)) (29 U) (28 (= #b111110)) (22 immh) (18 immb)
                                              (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.9
  (define-encoding (adv-simd-scalar-3-diff pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b1))
                                           (20 Rm) (15 opcode) (11 (= #b00)) (9 Rn) (4 Rd)))

  ;; C4.6.10
  (define-encoding (adv-simd-scalar-3-same pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b1))
                                           (20 Rm) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.11
  (define-encoding (adv-simd-scalar-2reg-misc pc instr (31 (= #b01)) (29 U) (28 (= #b11110)) (23 size) (21 (= #b10000))
                                              (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.12
  (define-encoding (adv-simd-scalar-x-idx-elem pc instr (31 (= #b01)) (29 U) (28 (= #b11111)) (23 size) (21 L) (20 M)
                                               (19 Rm) (15 opcode) (11 H) (10 (= #b0)) (9 Rn) (4 Rd)))

  ;; C4.6.13
  (define-encoding (adv-simd-shift-by-imm pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b011110)) (22 (!= #b0000) immh)
                                          (18 immb) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.14
  (define-encoding (adv-simd-table-lookup pc instr (31 (= #b0)) (30 Q) (29 (= #b001110)) (23 op2) (21 (= #b0))
                                          (20 Rm) (15 (= #b0)) (14 len) (12 op) (11 (= #b00)) (9 Rn) (4 Rd)))

  ;; C4.6.15
  (define-encoding (adv-simd-3-diff pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 (= #b1))
                                    (20 Rm) (15 opcode) (11 (= #b00)) (9 Rn) (4 Rd)))

  ;; C4.6.16
  (define-encoding (adv-simd-3-same pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 ( = #b1))
                                    (20 Rm) (15 opcode) (10 (= #b1)) (9 Rn) (4 Rd)))

  ;; C4.6.17
  (define-encoding (adv-simd-2reg-misc pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01110)) (23 size) (21 (= #b10000))
                                       (16 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.18
  (define-encoding (adv-simd-vec-x-idx-elem pc instr (31 (= #b0)) (30 Q) (29 U) (28 (= #b01111)) (23 size) (21 L)
                                            (20 M) (19 Rm) (15 opcode) (11 H) (10 (= #b0)) (9 Rn) (4 Rd)))

  ;; C4.6.19
  (define-encoding (crypto-aes pc instr (31 (= #b01001110)) (23 size) (21 (= #b10100)) (16 opcode) (11 (= #b10))
                               (9 Rn) (4 Rd)))

  ;; C4.6.20
  (define-encoding (crypto-3-reg-sha pc instr (31 (= #b01011110)) (23 size) (21 (= #b0)) (20 Rm) (15 (= #b0))
                                     (14 opcode) (11 (= #b00)) (9 Rn) (4 Rd)))

  ;; C4.6.21
  (define-encoding (crypto-2-reg-sha pc instr (31 (= #b01011110)) (23 size) (21 (= #b10100)) (16 opcode) (11 (= #b10))
                                     (9 Rn) (4 Rd)))

  ;; C4.6.22
  (define-encoding (fp-compare pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1)) (20 Rm)
                               (15 op) (13 (= #b1000)) (9 Rn) (4 opcode2))
    #;
    (select pc instr
            fcmp
            fcmpe)

    (decode
     [(and (= M #b0) (= S #b0) (= type #b00) (= op #b00) (= opcode2 #b00000))
      (fcmp pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b00) (= op #b00) (= opcode2 #b01000))
      (fcmp pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b00) (= op #b00) (= opcode2 #b10000))
      (fcmpe pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b00) (= op #b00) (= opcode2 #b11000))
      (fcmpe pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b01) (= op #b00) (= opcode2 #b00000))
      (fcmp pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b01) (= op #b00) (= opcode2 #b01000))
      (fcmp pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b01) (= op #b00) (= opcode2 #b10000))
      (fcmpe pc instr)]
     [(and (= M #b0) (= S #b0) (= type #b01) (= op #b00) (= opcode2 #b11000))
      (fcmpe pc instr)]))

  (define-encoding (fcmp pc instr (31 (= #b00011110)) (23 #;(= 'b0x) type) (21 (= #b1)) (20 Rm) (15 (= #b001000))
                         (9 Rn) (4 #;(= 'b0x) opc) (2 (= #b000)))
    (decode
     [(and (= type #b00) (= opc #b00))
      `(fcmp ,(vector-ref S-registers Rn) ,(vector-ref S-registers Rm))]))

  (define-encoding (fcmpe pc instr (31 (= #b00011110)) (23 #;(= 'b0x) type) (21 (= #b1)) (20 Rm) (15 (= #b001000))
                          (9 Rn) (4 #;(= 'b1x) opc) (2 (= #b000)))
    (decode
     [(and (= type #b00) (= opc #b10))
      `(fcmpe ,(vector-ref S-registers Rn) ,(vector-ref S-registers Rm))]
     [(and (= type #b00) (= Rm #b00000) (= opc #b11))
      `(fcmpe ,(vector-ref S-registers Rn) #i0.0)]
     [(and (= type #b01) (= opc #b10))
      `(fcmpe ,(vector-ref D-registers Rn) ,(vector-ref D-registers Rm))]
     [(and (= type #b01) (= Rm #b00000) (= opc #b11))
      `(fcmpe ,(vector-ref D-registers Rn) #i0.0)]))

  ;; C4.6.23
  (define-encoding (fp-cond-compare pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                    (20 Rm) (15 cond*) (11 (= #b01)) (9 Rn) (4 op) (3 nzcv)))

  ;; C4.6.24
  (define-encoding (fp-cond-select pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                   (20 Rm) (15 cond*) (11 (= #b11)) (9 Rn) (4 Rd)))

  ;; C4.6.25
  (define-encoding (fp-data-processing-1src pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                            (20 opcode) (14 (= #b10000)) (9 Rn) (4 Rd)))

  ;; C4.6.26
  (define-encoding (fp-data-processing-2src pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                            (20 Rm) (15 opcode) (11 (= #b10)) (9 Rn) (4 Rd)))

  ;; C4.6.27
  (define-encoding (fp-data-processing-3src pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11111)) (23 type) (21 o1)
                                            (20 Rm) (15 o0) (14 Ra) (9 Rn) (4 Rd)))

  ;; C4.6.28
  (define-encoding (fp-imm pc instr (31 M) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1)) (20 imm8)
                           (12 (= #b100)) (9 imm5) (4 Rd)))

  ;; C4.6.29
  (define-encoding (conv-fp<->fx pc instr (31 sf) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b0))
                                 (20 rmode) (18 opcode) (15 scale) (9 Rn) (4 Rd)))

  ;; C4.6.30
  (define-encoding (conv-fp<->int pc instr (31 sf) (30 (= #b0)) (29 S) (28 (= #b11110)) (23 type) (21 (= #b1))
                                  (20 rmode) (18 opcode) (15 (= #b000000)) (9 Rn) (4 Rd)))

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
