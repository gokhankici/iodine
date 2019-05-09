#!/bin/sh

SCRIPT="$HOME/apps/SecVerilog-1.0/Examples/secverilog"

NAME='secverilog'
Z3_FILE="$NAME.z3"
FUN_FILE="$NAME.fun"
VER_FILE="$NAME.v"

echo "running: rm -f a.out $Z3_FILE && $SCRIPT -F $FUN_FILE -z $VER_FILE"

rm -f a.out "$Z3_FILE" && "$SCRIPT" -F "$FUN_FILE" "$VER_FILE"
