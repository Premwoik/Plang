#!/bin/bash

BOARD="arduino:avr:$1"
cd build || exit
clang-format out.h > out2.h
rm out.h
mv out2.h out.h
arduino-cli compile --fqbn "$BOARD"
cd ..