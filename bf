#!/bin/bash

out=$(basename $1 .bf)
echo $1 | ./bfc > "$out.c"
gcc "$out.c" -O3 -o $out
rm "$out.c"

