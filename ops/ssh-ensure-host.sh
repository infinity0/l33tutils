#!/bin/bash
# Add an ssh key by hostname, if it is not already added.

host="$1"
shift
keyfile=$(ssh -G "$host" | sed -ne 's/identityfile //p' | { read x y; echo $x; })
keyfile="${keyfile/#\~/$HOME}"
if ! ssh-add -l | grep -wF "$(ssh-keygen -l -f "$keyfile" | cut '-d ' -f2)"; then
    ssh-add -t43200 "$@" "$keyfile"
    ssh-add -l | grep -wF "$(ssh-keygen -l -f "$keyfile" | cut '-d ' -f2)"
fi
