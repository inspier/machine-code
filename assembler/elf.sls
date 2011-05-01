;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Translation of ELF structures into assembler directives.

(library (weinholt assembler elf (1 0 20110501))
  (export elf-32-assembler elf-64-assembler)
  (import (rnrs)
          (weinholt binfmt elf (1))
          (weinholt struct pack))

  ;; 32-bit ELF
  (define (elf-32-assembler x)
    (cond ((elf-section? x)
           `((%u32 ,(or (elf-section-name x) 0)
                   ,(elf-section-type x)
                   ,(elf-section-flags x)
                   ,(elf-section-addr x)
                   ,(elf-section-offset x)
                   ,(elf-section-size x)
                   ,(elf-section-link x)
                   ,(elf-section-info x)
                   ,(elf-section-addralign x)
                   ,(or (elf-section-entsize x)
                        (if (= (elf-section-type x) SHT-SYMTAB)
                            (format-size "LLLCCS")
                            0)))))
          ((elf-segment? x)
           `((%u32 ,(elf-segment-type x)
                   ,(elf-segment-offset x)
                   ,(elf-segment-vaddr x)
                   ,(elf-segment-paddr x)
                   ,(elf-segment-filesz x)
                   ,(elf-segment-memsz x)
                   ,(elf-segment-flags x)
                   ,(elf-segment-align x))))
          ((elf-image? x)
           `((%vu8 ,(pack "!L" ELF-MAGIC))
             (%u8 ,(elf-image-word-size x)
                  ,(elf-image-endianness x)
                  ,(elf-image-version x)
                  ,(elf-image-os-abi x)
                  ,(elf-image-abi-version x))
             (%align 16 0)
             (%u16 ,(elf-image-type x)
                   ,(elf-image-machine x))
             (%u32 ,(elf-image-version x)
                   ,(elf-image-entry x)
                   ,(elf-image-phoff x)
                   ,(elf-image-shoff x)
                   ,(elf-image-flags x))
             (%u16 ,(or (elf-image-ehsize x)
                        (format-size "4xCCxCC7xSSLLLLLSSSSSS"))
                   ,(or (elf-image-phentsize x)
                        (format-size "8L"))
                   ,(elf-image-phnum x)
                   ,(or (elf-image-shentsize x)
                        (format-size "10L"))
                   ,(elf-image-shnum x)
                   ,(elf-image-shstrndx x))))
          ((elf-symbol? x)
           `((%u32 ,(elf-symbol-name x) ,(elf-symbol-value x) ,(elf-symbol-size x))
             (%u8 ,(elf-symbol-info x) ,(elf-symbol-other x))
             (%u16 ,(or (elf-symbol-shndx x) SHN-UNDEF))))
          (else
           (error 'elf-32-assembler
                  "Don't know how to convert this type to assembler directives" x))))

  (define (elf-64-assembler x)
    (cond ((elf-section? x)
           `((%u32 ,(or (elf-section-name x) 0)
                   ,(elf-section-type x))
             (%u64 ,(elf-section-flags x)
                   ,(elf-section-addr x)
                   ,(elf-section-offset x)
                   ,(elf-section-size x))
             (%u32 ,(elf-section-link x)
                   ,(elf-section-info x))
             (%u64 ,(elf-section-addralign x)
                   ,(or (elf-section-entsize x)
                        (if (= (elf-section-type x) SHT-SYMTAB)
                            (format-size "LCCSQQ")
                            0)))))
          ((elf-segment? x)
           `((%u32 ,(elf-segment-type x)
                   ,(elf-segment-flags x))
             (%u64 ,(elf-segment-offset x)
                   ,(elf-segment-vaddr x)
                   ,(elf-segment-paddr x)
                   ,(elf-segment-filesz x)
                   ,(elf-segment-memsz x)
                   ,(elf-segment-align x))))
          ((elf-image? x)
           `((%vu8 ,(pack "!L" ELF-MAGIC))
             (%u8 ,(elf-image-word-size x)
                  ,(elf-image-endianness x)
                  ,(elf-image-version x)
                  ,(elf-image-os-abi x)
                  ,(elf-image-abi-version x))
             (%align 16 0)
             (%u16 ,(elf-image-type x)
                   ,(elf-image-machine x))
             (%u32 ,(elf-image-version x))
             (%u64 ,(elf-image-entry x)
                   ,(elf-image-phoff x)
                   ,(elf-image-shoff x))
             (%u32 ,(elf-image-flags x))
             (%u16 ,(or (elf-image-ehsize x)
                        (format-size "4xCCxCC7xSSLQQQLSSSSSS"))
                   ,(or (elf-image-phentsize x)
                        (format-size "2L6Q"))
                   ,(elf-image-phnum x)
                   ,(or (elf-image-shentsize x)
                        (format-size "LL4QLLQQ"))
                   ,(elf-image-shnum x)
                   ,(elf-image-shstrndx x))))
          ((elf-symbol? x)
           `((%u32 ,(elf-symbol-name x) ,(elf-symbol-value x) ,(elf-symbol-size x))
             (%u8 ,(elf-symbol-info x) ,(elf-symbol-other x))
             (%u16 ,(or (elf-symbol-shndx x) SHN-UNDEF))))
          (else
           (error 'elf-32-assembler
                  "Don't know how to convert this type to assembler directives" x)))))

