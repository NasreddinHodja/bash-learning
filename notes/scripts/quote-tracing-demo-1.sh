#!/bin/sh -

set -x

# Process args using for loop

echo Processing $# args

count=1
for i; do
    printf "  Argument %d: '%s'\n" "$count" "$i"

    count=`expr $count + 1`
done

set +x
