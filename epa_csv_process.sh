#!/bin/bash

shopt -s nullglob
files=(/Users/john/code/dust_pic/data/hourly*)
tick=1
for fl in "${files[@]}"
do
    echo $fl
    csvgrep -c 1 -m "06" "$fl" | csvgrep -c 2 -m "027" \
    | csvgrep -c 3 -r "1001|0021|0025|0022|0026"\
    | csvcut -c 3,12,13,4,9,14,15 > "/tmp/file_$tick.csv"
    temps+=("/tmp/file_$tick.csv")
    let "tick++"
done
echo stacking
csvstack "${temps[@]}" \
    > "/Users/john/code/dust_pic/data/epa.csv"
