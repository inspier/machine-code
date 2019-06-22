#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; fcdisasm - The Full-Color Disassembler
;; Copyright © 2008, 2009, 2010, 2011, 2016, 2017, 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; This program is an example of how to use (industria disassembler)
;; and a novelty: first disassembler to go *full color* for the hex
;; dump!

(import
  (rnrs (6))
  (machine-code format elf)
  (only (machine-code disassembler)
        invalid-opcode?
        available-disassemblers get-disassembler
        disassembler-max-instruction-size
        disassembler-instruction-getter))

;; Print an instruction with hexadecimal numbers.
(define (print-instr/sexpr i)
  (cond ((pair? i)
         (display "(")
         (let lp ((i i))
           (unless (null? i)
             (print-instr/sexpr (car i))
             (unless (null? (cdr i))
               (display #\space)
               (lp (cdr i)))))
         (display ")"))
        ((and (number? i) (exact? i) (integer? i))
         (display "#x")
         (display (number->string i 16)))
        (else
         (display i))))

(define (disassemble p disassembler color end-position pc symbols)
  (define (display-addr addr)
    (let ((x (number->string addr 16)))
      (if (< (string-length x) 8)
          (display (make-string (- 8 (string-length x)) #\space)))
      (display x)))
  (define (next-symbol symbols pc)
    (cond ((null? symbols) symbols)
          ((null? (cdr symbols)) symbols)
          ((or (> pc (cadar symbols))
               (= pc (caadr symbols)))
           (next-symbol (cdr symbols) pc))
          (else symbols)))
  (define get-instruction (disassembler-instruction-getter disassembler))
  (define hex-width (+ 1 (* 2 (disassembler-max-instruction-size disassembler))))
  (let lp ((pos (port-position p))
           (pc pc)
           (symbols (next-symbol symbols pc)))
    (let* ((tagged-bytes '())
           (i (guard (con
                      ((invalid-opcode? con)
                       `(bad: ,(condition-message con)
                              ,@(condition-irritants con))))
                (get-instruction p
                                 (lambda x
                                   ;; FIXME: This should handle words,
                                   ;; not only bytes.
                                   (set! tagged-bytes (cons x tagged-bytes)))
                                 pc)))
           (new-pos (port-position p)))
      ;; Print info from the symbol table
      (unless (null? symbols)
        (when (= pc (caar symbols))
          (let ((sym (car symbols)))
            (newline)
            (when color (display "\x1b;[4m"))
            (display (number->string (car sym) 16))
            (display #\-)
            (display (number->string (cadr sym) 16))
            (when color (display "\x1b;[0m"))
            (display #\space)
            (display (caddr sym))
            (newline))))
      ;; Print instructions
      (unless (or (eof-object? i)
                  (and end-position (> new-pos end-position)))
        (display-addr pc)
        (display ": ")
        (for-each (lambda (x)
                    (let ((tag (car x))
                          (bytes (cdr x)))
                      (cond ((eq? tag '/is4)
                             (when color
                               (display "\x1b;[1;34m"))
                             (display (number->string (bitwise-bit-field (car bytes) 4 8) 16))
                             (when color
                               (display "\x1b;[1;37m"))
                             (display (number->string (bitwise-bit-field (car bytes) 0 4) 16)))
                            (else
                             (when color
                               (case tag
                                 ((modr/m sib tfr/exg/sex) (display "\x1b;[1;34m"))
                                 ((opcode) (display "\x1b;[1;32m"))
                                 ((prefix) (display "\x1b;[1;33m"))
                                 ((immediate) (display "\x1b;[1;37m"))
                                 ((disp offset) (display "\x1b;[1;35m"))
                                 (else (display "\x1b;[0m"))))
                             (for-each (lambda (byte)
                                         (when (< byte #x10)
                                           (display #\0))
                                         (display (number->string byte 16)))
                                       bytes)))))
                  (reverse tagged-bytes))
        (when color
          (display "\x1b;[0m"))
        (display (make-string (- hex-width (* 2 (- new-pos pos))) #\space))
        (print-instr/sexpr i)
        (newline)
        (let ((new-pc (+ pc (- new-pos pos))))
          (lp new-pos new-pc (next-symbol symbols new-pc)))))))

(define (elf-architecture-symbol image)
  ;; XXX: Move to (machine-code format elf)
  (let ((machine (elf-image-machine image))
        (endianness (elf-image-endianness image))
        (entry (elf-image-entry image)))
    (cond ((and (= machine EM-ARM) (= (bitwise-and entry #b1) #b1)) 'arm-t32)
          ((and (= machine EM-ARM) (= (bitwise-and entry #b11) #b00)) 'arm-a32)
          ((= machine EM-AARCH6) 'arm-a64)
          ((= machine EM-386) 'x86-32)
          ((= machine EM-X86-64) 'x86-64)
          ((= machine EM-68HC12) 'm68hc12)
          ((= machine EM-MIPS) (if (= endianness ELFDATA2LSB) 'mipsel 'mipsbe))
          (else
           (error 'elf-architecture-symbol
                  "No support for this architecture"
                  (cond ((assv machine elf-machine-names) => cdr)
                        (else machine)))))))

;; Returns a list of (start-addr end-addr symbol) in increasing order.
(define (parse-elf-symbols image)
  (cond ((elf-image-symbols image) =>
         (lambda (symbols)
           (vector-sort! (lambda (s1 s2)
                           (> (elf-symbol-value (cdr s1))
                              (elf-symbol-value (cdr s2))))
                         symbols)
           (let lp ((ret '())
                    (i 0))
             (if (= i (vector-length symbols))
                 ret
                 (let* ((sym (vector-ref symbols i))
                        (name (car sym)) (s (cdr sym)))
                   (if (or (eqv? (elf-symbol-name s) 0)
                           (eqv? (elf-symbol-shndx s) SHN-UNDEF))
                       (lp ret (+ i 1))
                       (lp (cons (list (elf-symbol-value s)
                                       (+ (elf-symbol-value s)
                                          (elf-symbol-size s))
                                       name)
                                 ret)
                           (+ i 1))))))))
        (else '())))

(define (disassemble-file filename arch color)
  (cond ((is-elf-image? filename)
         (display "ELF image detected. Looking for .text section...\n")
         (let* ((image (open-elf-image filename))
                (text (elf-image-section-by-name image ".text")))
           (cond ((and text (= (elf-section-type text) SHT-PROGBITS))
                  (let ((arch (elf-architecture-symbol image))
                        (symbols (parse-elf-symbols image)))
                    (set-port-position! (elf-image-port image)
                                        (elf-section-offset text))
                    (disassemble (elf-image-port image)
                                 (get-disassembler arch)
                                 color
                                 (+ (elf-section-offset text)
                                    (elf-section-size text))
                                 (elf-section-addr text)
                                 symbols)))
                 (else
                  (display "This ELF image has no .text section with executable code.\n")
                  (display "No disassembly for you.\n")))))
        (else
         ;; Assume a DOS .com file.
         (disassemble (open-file-input-port filename)
                      (get-disassembler (string->symbol arch))
                      color #f #x100 '()))))

(define (parse-args args)
  (define (help . msg)
    (let ((x (current-error-port)))
      (when msg (display (car msg) x) (newline x) (newline x))
      (display "fcdisasm - Full-color disassembler

Usage:
  fcdisasm [-b|--bits <bits>] [-a|--arch <arch>] [--nocolor] [--] <filename>

The <bits> argument can be either 16 (default), 32 or 64. This is
shorthand for --arch x86-<bits>.

The <arch> argument, used for raw binary files, is one of these:
" x)
      (let lp ((arch* (available-disassemblers)))
        (unless (null? arch*)
          (display (car arch*))
          (unless (null? (cdr arch*))
            (display ", "))
          (lp (cdr arch*))))
      (display ".\n")
      (display "
The --nocolor flag suppresses the color output.

Author: Göran Weinholt <goran@weinholt.se>.
")
      (exit 1)))
  (let lp ((filename #f)
           (color #t)
           (arch "x86-16")
           (args args))
    (cond ((null? args)
           (unless filename
             (help "ERROR: No filename given."))
           (values filename arch color))
          ((or (string=? (car args) "--bits")
               (string=? (car args) "-b"))
           (if (null? (cdr args)) (help "ERROR: -b needs an argument (16, 32, 64)"))
           (cond ((assoc (cadr args) '(("64" . "x86-64") ("32" . "x86-32") ("16" . "x86-16"))) =>
                  (lambda (x)
                    (lp filename color (cdr x) (cddr args))))
                 (else
                  (help "ERROR: invalid argument for --bits flag"))))
          ((member (car args) '("-a" "--arch"))
           (if (null? (cdr args)) (help "ERROR: -a needs an argument"))
           (cond ((memq (string->symbol (cadr args)) (available-disassemblers)) =>
                  (lambda (_)
                    (lp filename color (cadr args) (cddr args))))
                 (else
                  (help "ERROR: invalid argument for --arch flag"))))
          ((string=? (car args) "--nocolor")
           (lp filename #f arch (cdr args)))
          ((string=? (car args) "--")
           (if (not (= (length args) 2)) (help "ERROR: following -- must be only a filename"))
           (if filename (help "ERROR: you can't have it both ways, use -- or don't"))
           (lp (cadr args) color arch (cddr args)))
          (else
           (if filename (help "ERROR: extra arguments on command line"))
           (lp (car args) color arch (cdr args))))))

(define (main args)
  (call-with-values (lambda () (parse-args args))
    disassemble-file))

(main (cdr (command-line)))
(flush-output-port (current-output-port))
