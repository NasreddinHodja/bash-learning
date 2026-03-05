#!/bin/bash -

echo Processing $# arguments

count=1
while [ $# -gt 0 ]; do
    # in a real scripts, do something with "$1"
    printf "  Argument %d: '%s'\n" "$count" "$1"

    let "count = count + 1"

    shift # shifts the arguments of the end
done
