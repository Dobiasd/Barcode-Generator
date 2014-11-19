#!/usr/bin/env bash
elm -m --bundle-runtime src/Main.elm
mv build/src/Main.html build/index.html
rmdir build/src
cp -r src/fonts build