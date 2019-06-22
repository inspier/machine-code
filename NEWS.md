## New in 2.1.0

The x86 assembler supports the `%equiv` directive. It is used to
define a new assembler label which is bound to an expression.

The x86 assembler supports more SSE instructions.

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
