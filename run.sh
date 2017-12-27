#!/bin/bash
day=$1
small=$2

src_file="day$1.hs"
in_file="$1a.input"
if [ "$small" == "small" ]; then
    in_file="$in_file.small"
fi

runhaskell $src_file < $in_file

