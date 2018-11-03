#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2008, 2009, 2010, 2011, 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; TODO: More systematic testing.

(import
  (rnrs (6))
  (machine-code assembler x86)
  (machine-code disassembler x86)
  (machine-code tests check))

(define-syntax test
  (syntax-rules (=>)
    ((_ instruction mode)
     (check (round-trip instruction mode) => instruction))
    ((_ instruction mode => expected)
     (check (round-trip instruction mode) => expected))))

(define-syntax testf
  (syntax-rules (=>)
    ((_ instruction mode)
     (check (guard (con (else 'not-encodable))
              (encode instruction mode))
            => 'not-encodable))))

(define (round-trip instruction mode)
  ;; Check that it's possible to encode the given instruction and then
  ;; disassemble it.
  (let*-values (((bv symbols) (assemble (list '(%origin 0)
                                              `(%mode ,mode)
                                              instruction)))
                ((port) (open-bytevector-input-port bv))
                ((bytes-returned) 0))
    (let ((instruction
           (get-instruction port mode
                            (lambda (_ . bytes)
                              (set! bytes-returned (+ (length bytes)
                                                      bytes-returned))
                              #f)
                            #f)))
      (unless (eof-object? (lookahead-u8 port))
        (error 'round-trip "After disassembly there are bytes unread."
               (get-instruction port mode #f #f)))
      (unless (= bytes-returned (bytevector-length bv))
        (error 'round-trip "There are bytes missing in the collector function."
               bytes-returned (bytevector-length bv)))
      instruction)))

(define (encode instruction mode)
  (let-values (((bv syms) (assemble (list '(%origin 0)
                                          `(%mode ,mode)
                                          instruction))))
    bv))

(test '(hlt) 64)

(test '(ret) 64)
(test '(ret 4) 64)

(test '(mov ax #xffff) 64)
(test '(mov eax #xffff) 64)
(test '(mov rax #xffff) 64)
(test '(mov r8 #xffff) 64)
(test '(mov r8w #xffff) 64)
(test '(mov r8d #xffff) 64)

(test '(mov rsp #x100010) 64)

;;; REX

(test '(mov al 1) 64)
(test '(mov r8b 1) 64)

;;; Operand size

(test '(mov ax (mem16+ rax)) 64)
(test '(mov eax (mem32+ rax)) 64)
(test '(mov rax (mem64+ rax)) 64)

(test '(in al dx) 64)
(test '(in ax dx) 64)
(test '(in eax dx) 64)

(test '(mov cr15 r15) 64)
(test '(mov cr15 rax) 64)
(test '(mov cr3 r15) 64)
(test '(mov dr5 r13) 64)
(test '(mov dr5 rax) 64)
(test '(mov dr14 rax) 64)

(test '(mov (mem8+ rax) #xff) 64)
(test '(mov (mem16+ rax) #xffff) 64)
(test '(mov (mem32+ rax) #xffffffff) 64)
(test '(mov (mem64+ rax) #x-7fffffff) 64
      => '(mov (mem64+ rax) #xffffffff80000001))

(test '(cbw) 64)
(test '(cwde) 64)
(test '(cdqe) 64)
(test '(cwd) 64)
(test '(cdq) 64)
(test '(cqo) 64)

;; Default operand size of 64:
(check (encode '(jmp rax) 64) => #vu8(#xff #xe0))
(testf '(jmp eax) 64)
(check (encode '(jmp ax) 64) => #vu8(#x66 #xff #xe0))
(check (encode '(jmp eax) 32) => #vu8(#xff #xe0))

;;; Segment and address size override

(test '(stos (mem8+ edi) al) 64)
(test '(stos (mem8+ rdi) al) 64)
(test '(stos (mem16+ rdi) ax) 64)
(test '(stos (mem32+ rdi) eax) 64)
(test '(stos (mem64+ rdi) rax) 64)
(test '(movs (mem32+ rdi) (mem32+ rsi)) 64)
(test '(movs (mem32+ rdi) (mem32+ fs rsi)) 64)
(testf '(movs (mem32+ rdi) (mem32+ rsi es)) 64)

(test '(movs (mem8+ es edi) (mem8+ ds esi)) 32)
(test '(movs (mem8+ es edi) (mem8+ es esi)) 32)
(test '(movs (mem8+ es edi) (mem8+ fs esi)) 32)

(test '(movs (mem+ es edi) (mem16+ esi)) 32
      => '(movs (mem16+ es edi) (mem16+ ds esi)))

;;; Prefixes

(test '(rep.stos (mem8+ rdi) al) 64)
(test '(rep.stos (mem32+ rdi) eax) 64)
(test '(repz.scas rax (mem64+ rdi)) 64)
(test '(repnz.scas rax (mem64+ rdi)) 64)
(test '(lock.xchg (mem64+ rax) rbx) 64)

;;; Various memory references

(test '(mov r15 (mem+ rax 12 (* rbx wordsize))) 64
      => '(mov r15 (mem64+ rax 12 (* rbx 8))))

(test '(mov rax (mem64+ rax #x0a00)) 64)
(test '(mov (mem64+ rax #x0a00) rax) 64)

(test '(mov rax (mem64+ rip 0)) 64)
(test '(mov rax (mem64+ rip #x100)) 64)
(test '(mov rax (mem64+ 0)) 64)
(test '(mov rax (mem64+ rbp)) 64
      => '(mov rax (mem64+ rbp 0)))
(test '(mov rax (mem64+ rbp 0)) 64)
(test '(mov rax (mem64+ rbp #xff)) 64)
(test '(mov rax (mem64+ rbp #x100)) 64)
(test '(mov rax (mem64+ rax)) 64)

(test '(mov rax (mem64+ rax 1)) 64)
(test '(mov rax (mem64+ rax 127)) 64)
(test '(mov rax (mem64+ rax 128)) 64)

(test '(mov rax (mem64+ rax -1)) 64)
(test '(mov rax (mem64+ rax -128)) 64)
(test '(mov rax (mem64+ rax -129)) 64)


(test '(mov rax (mem64+ rax rbx)) 64
      => '(mov rax (mem64+ rax (* rbx 1))))

(test '(mov rax (mem64+ (* rbx 4))) 64
      => '(mov rax (mem64+ 0 (* rbx 4))))

(test '(mov rax (mem64+ rdx (* rbx 4))) 64)

(test '(mov rax (mem64+ rdx 0 (* rbx 8))) 64
      => '(mov rax (mem64+ rdx (* rbx 8))))

(test '(mov rax (mem64+ rdx 127 (* rbx 8))) 64)

(test '(mov rax (mem64+ rdx 128 (* rbx 8))) 64)


(test '(mov rax (mem64+ rbp (* 8 rbx))) 64
      => '(mov rax (mem64+ rbp 0 (* rbx 8))))


(test '(mov r15 (mem64+ r15 -1 (* r15 8))) 64)

;; 32-bit memory in 64-bit mode
(test '(mov r15 (mem64+ edi -1 (* eax 8))) 64)
(test '(mov r15 (mem64+ 0)) 64)
(test '(mov r15d (mem32+ edi -1)) 64)
(test '(mov eax (mem32+ ebp 0)) 64)

;;; 64-bit stuff in 32-bit mode

(testf '(mov r15d eax) 32)
(testf '(addpd xmm14 xmm15) 32)

;;; SSE

(test '(addpd xmm14 xmm15) 64)
(test '(addpd xmm0 (mem128+ r14)) 64)
(test '(cvtsi2ss xmm1 rax) 64)
(test '(movq rdx xmm1) 64)
(test '(movd xmm1 eax) 64)
(test '(ucomiss xmm1 xmm2) 64)

;;; VEX

;; (test '(vpermil2ps xmm0 xmm1 xmm2 xmm3 13) 64) ;3-byte prefix
;; (test '(vandps xmm1 xmm2 xmm3) 64)      ;2-byte prefix

;; (test '(vandps xmm1 xmm2 (mem128+ r8)) 64)

;; ;; Swizzling r/m and /is4
;; (test '(vpermil2ps xmm0 xmm1 xmm2 (mem128+ rbx -1) 13) 64)
;; (test '(vpermil2ps xmm0 xmm1 (mem128+ rbx -1) xmm3 13) 64)

;;; NOP

(test '(nop) 16)
(test '(nop) 32)
(test '(nop) 64)
(test '(pause) 16)
(test '(pause) 32)
(test '(pause) 64)
(test '(xchg eax eax) 16)
(test '(xchg rax rax) 64)
(test '(xchg r8w ax) 64)
(test '(xchg r8d eax) 64)
(test '(xchg r8 rax) 64)
(test '(xchg ax ax) 32)

;;; Group 15

(test '(lfence) 64)
(test '(mfence) 64)
(test '(sfence) 64)

(test '(fxsave (mem+ rax)) 64)
(test '(xsave (mem+ rax)) 64)
(test '(clflush (mem8+ rax)) 64)

;;; Moffset

;; Disabled for now.

;; (test '(mov (mem64+ #xff00ff00ffffffff) rax) 64)

;; The problem with these: the Eb opsyntax eagerly accepts them, but
;; then can't encode them. Solution: make Eb (etc) stricter?

;; XXX: also test that larger addresses can't get through
      ;; (mov (mem+ #xff00ff00ffffffff) al)
      ;; (mov (mem+ #xff00ff00ffffffff) ax)
      ;; (mov (mem+ #xff00ff00ffffffff) eax)
      ;; (mov (mem+ #xff00ff00ffffffff) rax)
;; XXX: also test in 16- and 32-bit modes

      ;; (mov (mem+ #xff00ff00ffffffff) al)
      ;; (mov (mem+ #xff00ff00ffffffff) ax)
      ;; (mov (mem+ #xff00ff00ffffffff) eax)
;; (mov (mem+ #xff00ff00ffffffff) rax)

(check-report)
(exit (if (check-passed? 106) 0 1))
