#!/bin/bash

echo ""
echo "#########################"
echo "Building pLang compiler:"
stack build
echo "Copying compiler:"
cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/l1-exe/l1-exe /home/prw/CODE/MT/l1/res/PLangCopiler/bin/plang
echo "Ready!"
