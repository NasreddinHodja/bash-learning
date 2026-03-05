#!/bin/bash

echo "This goes to stdout (normal output)"
echo "This also goes to stdout"
echo "This goes to stderr (error output)" >&2
echo "More stdout output"
ls /nonexistent-directory  # This will generate stderr
echo "Final stdout message"
