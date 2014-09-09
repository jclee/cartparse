#!/usr/bin/env bash

MY_PATH="`dirname \"$0\"`"
cd "$MY_PATH"
ghc -o parse -Wall -fwarn-incomplete-patterns parse.hs && ./parse
