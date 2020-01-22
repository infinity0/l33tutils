#!/bin/bash
# Add an ssh key by hostname, if it is not already added.

host="$1"
shift
keyfile=$(ssh -G "$host" | sed -ne 's/identityfile //p' | { read x y; echo $x; })
keyfile="${keyfile/#\~/$HOME}"
if ! ssh-add -l | grep -wF "$keyfile"; then
    ssh-add -t43200 "$@" "$keyfile"
    ssh-add -l | grep -wF "$keyfile"
fi
