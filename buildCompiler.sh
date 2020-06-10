#!/bin/bash

echo ""
echo "#########################"
echo "Building pLang compiler:"
stack build
echo "Copying compiler:"
cp /home/prw/CODE/MT/l1/.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/l1-exe/l1-exe /home/prw/CODE/MT/l1/res/PLangCopiler/bin/plang
#cd res
#clang-format out.h > TestSketch/out.h
#g++ TestSketch/res/out.h
#cd TestSketch


#echo ""
#echo "#########################"
echo "Ready!"
#arduino-cli compile --fqbn arduino:avr:uno
#arduino-cli compile --fqbn arduino:avr:mega
#arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:mega
#cd ..
#cd ..