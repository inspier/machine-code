# machine-code

[![Build Status](https://travis-ci.org/weinholt/machine-code.svg?branch=master)](https://travis-ci.org/weinholt/machine-code)

This project is about the development of tools that relate to machine
code and object formats; for all architectures.

Here you'll find libraries for working with binary code: assembly,
disassembly, instruction tables, object formats and related areas.

## Current status

The x86/amd64 assembler is operational, but not recommended for
general use. Creating ELF object files is possible only through
crafting the correct assembler directives yourself.

The disassemblers are operational: arm-a64, m68hc12, i8080, x86-64,
x86-32, x86-16, mipsbe, and mipsel.

The ELF library is useful for read operations.

Documentation is sparse.

## Installation

With [Akku.scm](https://akkuscm.org):

```bash
$ akku install machine-code
$ source .akku/bin/activate
```

Otherwise refer to your Scheme implementation's documentation on using
R6RS libraries.

## Usage

Try the fcdisasm program (the real output has color):

```bash
$ fcdisasm /bin/ls | head
ELF image detected. Looking for .text section...
  4028A0: 4157                           (push r15)
  4028A2: 4156                           (push r14)
  4028A4: 4155                           (push r13)
  4028A6: 4154                           (push r12)
  4028A8: 55                             (push rbp)
  4028A9: 53                             (push rbx)
  4028AA: 89FB                           (mov ebx edi)
  4028AC: 4889F5                         (mov rbp rsi)
  4028AF: 4881EC98030000                 (sub rsp #x398)
```

The x86-demo and x86-linux-demo programs build simple object files.

The disassembler libraries all export a `get-instruction` procedure
that takes an input port and a *collect* procedure. (The MIPS
disassembler also takes an endianness, and the x86 disassembler takes
an operating mode). The *collect* procedure can be *#f*; otherwise it
is called with groups of bytes that have been read from the
instruction stream.

## New in 2.0.0

### New disassembler for ARM A64 (used in AArch64, ARMv8)

The new `(machine-code disassembler arm-a64)` library disassembles
64-bit ARM code.

### Generic disassembler library

The new `(machine-code disassembler)` library provides an abstract
interface to all current and future disassemblers.

### Incompatible change

The procedure signatures for all disassemblers has changed. An
additional `pc` argument has been added. It can be the current program
counter (the address of the instruction) or `#f`. If it's `#f` then
symbolic expressions are used in place of a computed PC-relative
address.

## Contributions

Contributions are very welcome as long as they are relevant to the
goals stated at the top of this file, and licensed under the MIT
license. Bug fixes, new libaries and tools are all welcome. External
dependencies must be managed with Akku.scm.

## Contact

The author can be contacted through the issue tracker on GitHub:
https://github.com/weinholt/machine-code/issues
