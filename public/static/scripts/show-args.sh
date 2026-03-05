#!/bin/bash -

arg_count() {
    echo 'argument count:' $#
}

printf 'Number of arguments: %d\n\n' $#

printf 'First arg is: %s\n\n' $1

# without double quotes, these are essencially the same
# these dont preserve whitespace
echo 'All args: ' $*
arg_count $*
echo
echo 'All args: ' $@
arg_count $@
echo

echo

# gives us all args AS A SINGLE ARG
echo 'All args, double quoted:  ' "$*"
arg_count "$*"
echo
# gives us all args as separate individual
# double quoted strings (preserves whitespace)
echo 'All args, double quoted:  ' "$@"
arg_count "$@"
