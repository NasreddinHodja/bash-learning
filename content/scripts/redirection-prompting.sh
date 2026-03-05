#!/bin/sh -

pass=x pass2=y
until [ "$pass" = "$pass2" ]; do
    stty -echo

    printf "Enter new password: "

    read pass < /dev/tty

    printf "\nEnter again: "

    read pass2 < /dev/tty

    stty echo

    echo

    if [ "$pass" = "$pass2" ]; then
        echo Passwords match
    else
        echo "Passwords don\'t match"
    fi
done
