#!/bin/bash
for i in llvm gnu aocc intel nvhpc/25.11 ; do ( echo $i ; module load  $i 2>&1 1>/dev/null ; set -x ; $CC int32_to_64.c -O2 -DUSE_INT && ./a.out ;) ; done
echo "=============================================================================================="
for i in llvm gnu aocc intel nvhpc/25.11 ; do ( echo $i ; module load  $i 2>&1 1>/dev/null ; set -x ; $CC int32_to_64.c               && ./a.out ;) ; done
echo "=============================================================================================="
for i in llvm gnu aocc intel nvhpc/25.11 ; do ( echo $i ; module load  $i 2>&1 1>/dev/null ; set -x ; $CC int32_to_64.c -O2           && ./a.out ;) ; done

