#!/usr/bin/env bash

rm -r build

mkdir build
mkdir build/js

elm-make src/Main.elm --output build/js/barcode_generator.js

if [ $? -eq 0 ]
then
  cp -r src/fonts build
  cp ./src/index.html ./build/index.html
  cp ./src/htmlmain.js ./build/js/htmlmain.js
  cp ./src/html2canvas.js ./build/js/html2canvas.js
fi