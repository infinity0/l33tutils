#!/bin/sh
# Simple wrapper around nsupdate. Requires stun(1) from the stun-client package.
#
# Usage: $0 <NAME.conf>
#
# NAME.conf is a shell snippet that defines:
# NS_AUTH - authoritative server to submit updates to
# NS_NAME - hostname to perform the update on
# NS_ZONE - zone that the hostname belongs to
#
# NAME.key must exist and contain a valid key, see nsupdate(1) for details
# NAME.addr should be writeable/createable. It will hold your current IP address.
#
# TODO: figure out a way to run this when ip addr (*of the router*) changes
# TODO: queue prev results and only change if multiple sources agree

CONF="$1"
test -f "$CONF" || { echo >&2 "not a file: $CONF"; exit 2; }

. "$(readlink -f "$CONF")"
test -n "$NS_AUTH" || { echo >&2 "conf didn't define NS_AUTH"; exit 1; }
test -n "$NS_NAME" || { echo >&2 "conf didn't define NS_NAME"; exit 1; }
test -n "$NS_ZONE" || { echo >&2 "conf didn't define NS_ZONE"; exit 1; }

BASE="${CONF%.conf}"
OLDIP="$(cat "$BASE.addr")"

stun_ip() {
	stun "$1" -v 2>&1 | ( err=; while read x; do
		ipp="${x#MappedAddress = }"
		if test -n "$err"; then echo "$x";
		elif [ "$x" != "$ipp" ]; then echo "${ipp%:*}"; exit 0;
		elif [ "$x" != "${x#Primary}" ]; then echo "$x"; exit 1;
		elif [ "$x" != "${x#?rror}" ]; then echo "$x"; err=1;
		fi
	done; exit 2; )
}

get_ip() {
	eval "$1" | tee "$BASE.addr.tmp" | grep -q -P '^\d+\.\d+\.\d+\.\d+$' -
}

shuf <<EOF | while read x; do if get_ip "$x"; then break; fi done
stun_ip stun.l.google.com:19302
stun_ip stun.ideasip.com
stun_ip stun.ekiga.net
stun_ip stun.iptel.org
stun_ip stun.schlund.de
stun_ip stun.voiparound.com
stun_ip stun.voipbuster.com
stun_ip stun.voipstunt.com
stun_ip stun.voxgratia.org
stun_ip numb.viagenie.ca
stun_ip stun.counterpath.com
stun_ip stun.services.mozilla.com
stun_ip stun.sipgate.net
curl -s https://wtfismyip.com/text
curl -s https://api.ipify.org/?format=text
curl -s https://icanhazip.com
curl -s https://check.torproject.org | sed -n -re 's,.*IP address.*>([.0-9]+)<.*,\1,gp'
EOF

IPADDR="$(cat "$BASE.addr.tmp")"
if ! echo "$IPADDR" | grep -q -P '^\d+\.\d+\.\d+\.\d+$' -; then
	echo >&2 "error getting IP: $IPADDR"
	rm "$BASE.addr.tmp"
	exit 1
fi

test "$IPADDR" = "$OLDIP" && exit 0
mv -f "$BASE.addr.tmp" "$BASE.addr"
nsupdate -k "$BASE.key" -v <<EOF
server $NS_AUTH
zone $NS_ZONE
update delete $NS_NAME. A
update add $NS_NAME. 1200 A $IPADDR
send
EOF
