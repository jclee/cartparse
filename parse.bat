@echo off
pushd "%~dp0"
ghc.exe -o parse.exe -fwarn-incomplete-patterns parse.hs && .\parse.exe
popd
