#!/usr/bin/bash

mkdir -p static/scripts

for f in notes/scripts/*; do
    [ -f "$f" ] || continue
    cp "$f" static/scripts/
done
