#!/usr/bin/env bash
elm-make src/Main.elm --output build/index.html
cp -r src/fonts build