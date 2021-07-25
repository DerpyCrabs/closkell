#/bin/sh
stack run closkell-exe -- compile --emit-llvm main.ll -f "$1"
llc -mtriple=wasm32-unknown-unknown -O3 -filetype=obj main.ll -o main.o
wasm-ld main.o -o main.wasm --no-entry -allow-undefined --export-all
node ./test/EmitLLVM/run_wasm.js
rm main.o
rm main.ll
rm main.wasm