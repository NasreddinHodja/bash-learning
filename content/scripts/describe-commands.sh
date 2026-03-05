#!/bin/bash -

# demonstrate the different kinds of commands

count_unique_users() {
    who | awk '{ print $1 }' | sort -u | wc -l
}

commands=(cd read pwd test printf count_unique_users ls who sort)

for cmd in "${commands[@]}"; do
    type $cmd
done
