#!/bin/bash
cd ..
#echo ""
#echo "#########################"
#echo "### BUILT RESULT: "
#stack build


echo ""
echo "#########################"
echo "### MyLangCompiler RESULT: "
stack run l1-exe
cd res
clang-format out.h > TestSketch/out.h
#g++ TestSketch/res/out.h
cd TestSketch


echo ""
echo "#########################"
echo "### ARDUINO COMPILER RESULT: "
#arduino-cli compile --fqbn arduino:avr:uno
arduino-cli compile --fqbn arduino:avr:mega
arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:mega
cd ..
cd ..