# machine-code

[![Build Status](https://travis-ci.org/weinholt/machine-code.svg?branch=master)](https://travis-ci.org/weinholt/machine-code)

Machine code is often hidden from view and difficult to work with.
This project aims to lift the veil.

This project is about the development of tools that relate to machine
code and object formats; for all architectures.

Here you'll find libraries for working with binary code: assembly,
disassembly, instruction tables, object formats and related areas.

## Current status

The x86/amd64 assembler is operational, but not recommended for
general use. Creating ELF object files is possible only through
crafting the correct assembler directives yourself.

The disassemblers are operational: x86-16, x86-32, x86-64, hc12,
mipsel, mipsbe, and 8080.

The ELF library is useful for read operations.

Documentation is sparse.

## Installation

First you need a Scheme. Any [R6RS Scheme](http://www.r6rs.org/)
implementation should work.

Clone the repository (or extract the archive):
```bash
git clone https://github.com/weinholt/machine-code
```

The `machine-code` directory should be under a directory in your Scheme
library path. Example for Chez Scheme:

```bash
export CHEZSCHEMELIBDIRS=$PWD:$CHEZSCHEMELIBDIRS
```

## Usage

Try the fcdisasm program (the real output has color):

```bash
$ programs/fcdisasm /bin/ls | head
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
disassembler also takes an endianness). The *collect* procedure can be
*#f*; otherwise it is called with groups of bytes that have been read
from the instruction stream.

## Contributions

Contributions are very welcome as long as they are relevant to the
goals stated at the top of this file, and licensed under the MIT
license. Bug fixes, new libaries and tools are all welcome. External
dependencies in the base libraries should be kept to a minimum, but
are generally okay in the programs and in specialized libraries.

## Contact

The author can be contacted through the issue tracker on GitHub:
https://github.com/weinholt/machine-code/issues
