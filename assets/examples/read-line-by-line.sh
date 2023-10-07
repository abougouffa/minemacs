#!/bin/bash

# https://linuxhandbook.com/bash-read-line-by-line/

file="input-file.txt"

while read -r line; do
    echo -e "$line\n"
done <$file

# OR

cat LHB.txt | while IFS= read -r line; do
    echo "$line"
    echo   # Print a blank line
done
