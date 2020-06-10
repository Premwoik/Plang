#!/bin/bash

BOARD="arduino:avr:$1"
cd build || exit
arduino-cli compile --fqbn "$BOARD"
cd ..