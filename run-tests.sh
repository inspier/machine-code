#!/bin/bash
set -ex

RUNSCHEME="${RUNSCHEME-}"

function cleanup {
  rm -f x86-demo.image x86-linux-demo.image
}
trap cleanup EXIT

$RUNSCHEME tests/cpu12disasm.sps
$RUNSCHEME tests/string-table.sps
$RUNSCHEME tests/x86asm.sps
$RUNSCHEME tests/x86disasm.sps

$RUNSCHEME programs/x86-demo
$RUNSCHEME programs/x86-linux-demo

case $(uname -m) in
    x86_64|i386)
        chmod +x x86-linux-demo.image
        ./x86-linux-demo.image
        ;;
esac

if [ "x$SCHEME" != "xSagittarius" ]; then
    # Disabled in Sagittarius for now due to bad performance.
    $RUNSCHEME programs/fcdisasm -b 32 x86-demo.image | grep -5 rep
    $RUNSCHEME programs/fcdisasm -b 32 x86-linux-demo.image
    $RUNSCHEME tests/arm-a64.sps
fi
