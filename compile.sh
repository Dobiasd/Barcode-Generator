#!/usr/bin/env bash
elm -m src/Main.elm
mv build/src/* build
rmdir build/src