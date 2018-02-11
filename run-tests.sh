#!/bin/bash
set -ex

function cleanup {
  rm -f x86-demo.image x86-linux-demo.image
}
trap cleanup EXIT

tests/cpu12disasm.sps
tests/string-table.sps
tests/x86asm.sps
tests/x86disasm.sps

programs/x86-demo
programs/x86-linux-demo

case $(uname -m) in
    x86_64|i386)
        chmod +x x86-linux-demo.image
        ./x86-linux-demo.image
        ;;
esac

if [ "x$SCHEME" != "xSagittariusScheme" ]; then
    # Disabled in Sagittarius for now due to bad performance.
    programs/fcdisasm -b 32 x86-demo.image | grep -5 rep
    programs/fcdisasm -b 32 x86-linux-demo.image
    if [ "x$SCHEME" == "xChezScheme" ]; then
        tests/arm-a64.sps
    fi
fi
