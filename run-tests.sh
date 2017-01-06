#!/bin/bash
set -ex

SCHEME="${SCHEME-}"

function cleanup {
  rm -f x86-demo.image x86-linux-demo.image
}
trap cleanup EXIT

$SCHEME tests/arm-a64.sps
$SCHEME tests/cpu12disasm.sps
$SCHEME tests/string-table.sps
$SCHEME tests/x86asm.sps
$SCHEME tests/x86disasm.sps

$SCHEME programs/x86-demo
$SCHEME programs/fcdisasm -b 32 x86-demo.image | grep -5 rep

$SCHEME programs/x86-linux-demo
$SCHEME programs/fcdisasm -b 32 x86-linux-demo.image
case $(uname -m) in
    x86_64|i386)
        chmod +x x86-linux-demo.image
        ./x86-linux-demo.image
        ;;
esac
