#!/bin/bash -

echo Processing $# arguments

count=1
for i; do
    # in a real scripts, do something with "$i"
    printf "  Argument %d: '%s'\n" "$count" "$i"

    let "count = count + 1"
done
