#!/usr/bin/env sh

(find notes static build.sh watch.sh publish.el -type f | entr ./build.sh) &
WATCH_PID=$!

trap "kill $WATCH_PID" EXIT

browser-sync start --server public --files "public/**/*.*" --no-open --reload-delay 50
