#!/bin/sh
# VLC 3 chromecast
#
# 0. Download and install: https://people.debian.org/~infinity0/apt/pool/main/v/vlc/
# 1. Run this script to cast.
#    You can control the playback using the VLC GUI.

IPADDR="$1"; shift
TRUSTDIR="$HOME/.local/share/vlc/ssl/certs"

set -e

# Get the certificate and store it
mkdir -p "$TRUSTDIR"
echo \
  | openssl s_client -showcerts -connect "${IPADDR}:8009" \
  | sed -ne '/BEGIN CERTIFICATE/,/END CERTIFICATE/p' \
  > "$TRUSTDIR/chromecast-$IPADDR.crt"

# Extract the CN
COMMONNAME=$(openssl x509 -in "$TRUSTDIR/chromecast-$IPADDR.crt" -noout -subject \
  | sed -ne 's/.*\bCN\s*=\s*\(.*\)/\1/p')
CN0=$(sed -ne 's/'"$IPADDR"'\s*\(.*\)/\1/p' /etc/hosts)

# Add an entry to /etc/hosts
test "$CN0" = "$COMMONNAME" || {
	set -x
	if [ -n "$CN0" ]; then
		sudo sed -ie 's/'"$IPADDR"'\(\s*\).*/'"$IPADDR\1$COMMONNAME"'/' /etc/hosts
	else
		echo "$IPADDR	$COMMONNAME" | sudo tee -a /etc/hosts
	fi
	set +x
}

# Start VLC!
#vlc --sout="#duplicate{dst=display,dst=chromecast{ip=${COMMONNAME}}}" "$@"
vlc --sout="#chromecast{ip=${COMMONNAME}}" --gnutls-dir-trust="$TRUSTDIR" "$@"
