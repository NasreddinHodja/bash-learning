#!/usr/bin/env sh

(while inotifywait -r -e modify,create,delete,move notes static build.sh watch.sh publish.el; do
    ./build.sh
done) &

WATCH_PID=$!
trap "kill $WATCH_PID" EXIT

browser-sync start --server public --files "public/**/*.*" --no-open --reload-delay 50
