#!/bin/bash
day=$1
small=$2

src_file="src/day$1.hs"
in_file="data/$1a.input"
if [ "$small" == "small" ]; then
    in_file="$in_file.small"
fi

runhaskell $src_file < $in_file

