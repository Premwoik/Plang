#!/bin/bash

BOARD="arduino:avr:$1"
PORT="/dev/tty$2"
cd build || exit
#arduino-cli compile --fqbn "$BOARD"
arduino-cli upload -p "$PORT" --fqbn "$BOARD"
cd ..