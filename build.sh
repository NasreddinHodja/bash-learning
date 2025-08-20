#!/usr/bin/env sh

./publish-scripts.sh
emacs -Q --script build-toc.el
emacs -Q --script publish.el
