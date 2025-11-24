#!/bin/sh
# Simple wrapper around nsupdate.
#
# Usage: $0 <NAME.conf> <FAILURE CMD>
#
# Files:
# NAME.conf
#   Shell snippet that defines:
#   NS_ZONE
#     FQDN zone that the hostname belongs to, e.g. "example.com"
#   NS_NAME
#     hostname to perform the update on, e.g. "mysub" to update
#     "mysub.example.com" or "@" to update "example.com".
#   IPV
#     IP versions to use, one of {4,6,46}. If using IPv4 then we require stun(1)
#     from the stun-client package.
#   METHOD
#     DNS update method to use, in the following form:
#     nsupdate <NS_AUTH>
#       Submit updates via nsupdate(1). Args:
#       NS_AUTH authoritative server to submit updates to
#     gandi-livedns
#       Submit updates to https://api.gandi.net/v5/livedns
#   SSH_HOST_4
#     If this is non-empty, then IPv4 address lookup commands will be run on
#     this remote host. This is useful if your ISP doesn't provide you with a
#     public IPv4 address that you can configure port forwarding on, in which
#     case you can have this SSH host reverse-tunnel for you. It will need to
#     have both stun(1) and curl(1) installed, for IPv4 address detection.
#
#     You may want to add this to the authorized_keys file to restrict the
#     shell to only the commands that this script runs:
#     restrict,command="$HOME/dyndns-rshell.sh \"$SSH_ORIGINAL_COMMAND\""
# NAME.key
#   Relevant key for your update METHOD. For example, a nsupdate(1) key or a
#   gandi-livedns API key.
# NAME.addr4/addr6
#   Must be writeable. It will hold your current IP address.
#
# TODO: figure out a way to run this when ip addr (*of the router*) changes
# TODO: queue prev results and only change if multiple sources agree
# TODO: IPv6 detection relies on your router giving you a public IPv6 address.
# Note: keep this updated with dyndns-rshell.sh

CONF="$1"
shift
test -f "$CONF" || { echo >&2 "not a file: $CONF"; exit 2; }

. "$(readlink -f "$CONF")"
test -n "$METHOD" || { echo >&2 "conf didn't define METHOD"; exit 1; }
test -n "$NS_NAME" || { echo >&2 "conf didn't define NS_NAME"; exit 1; }
test -n "$NS_ZONE" || { echo >&2 "conf didn't define NS_ZONE"; exit 1; }
IPV="${IPV:-4}"
case "$IPV" in
4)	IPTYPES="addr4";;
6)	IPTYPES="addr6";;
46)	IPTYPES="addr4 addr6";;
*) echo >&2 "invalid IPV: $IPV"; exit 1;;
esac

BASE="${CONF%.conf}"

dbglog() {
	if [ -n "$debug" ]; then
		echo >&2 "$0: $@"
	fi
}

stun_ip() {
	# note: it is important to run all STUN test cases, as this allows the
	# client to timeout properly. if you just run 1 test case, stun(1) will not
	# timeout gracefully if e.g. the remote server is down
	$1 stun -v "$2" 2>&1 | ( err=; while read x; do
		ipp="${x#MappedAddress = }"
		if test -n "$err"; then echo "$x";
		elif [ "$x" != "$ipp" ]; then echo "${ipp%:*}"; return 0;
		elif [ "$x" != "${x#Primary}" ]; then echo "$x"; return 1;
		elif [ "$x" != "${x#?rror}" ]; then echo "$x"; err=1;
		fi
	done; return 2; )
}

read_ip4() {
	local cmd="$1"
	shift
	case "$SSH_HOST_4" in
	"") local run="eval";;
	*)  local run="ssh -n $SSH_HOST_4";;
	esac
	case "$cmd" in
	curl) $run curl --connect-timeout 3 -4 -s "$@";;
	stun) stun_ip "$run" "$@";;
	esac | tee "$BASE.addr4.tmp" | grep -q -P '^\d+\.\d+\.\d+\.\d+$' -
}

get_ip4() {
	shuf <<-EOF | {
	stun stun.l.google.com:19302
	stun stun.counterpath.com
	stun stun.1und1.de
	stun stun.sipgate.net
	stun stun.voipbuster.com
	stun stun.voipstunt.com
	curl https://wtfismyip.com/text
	curl https://icanhazip.com
	EOF
	#stun stun.stunprotocol.org # been redirected to 127.0.0.1
	#curl https://api.ipify.org/?format=text # sometimes is very slow
	while read x; do
		dbglog "$addr<" "$x"
		if read_ip4 $x </dev/null; then
			break
		else
			echo >&2 "failed read_ip4 $x: $(cat "$BASE.addr4.tmp")"
			echo >&2 "trying again with another"
			continue
		fi
	done
	}
}

get_ip6() {
	ip -6 -j addr \
	  | jq -r '.[]
		| .addr_info
		| .[]
		| select(.prefixlen == 128)
		| select(.scope == "global")
		| .local
		| select(startswith("fd") or startswith("fc") | not)' \
	  | head -n1 > "$BASE.addr6.tmp"
}

changes=0
for addr in $IPTYPES; do
	dbglog "$addr>"
	case $addr in
	addr4) ping="ping -4"; get_ip4;;
	addr6) ping="ping -6"; get_ip6;;
	esac
	IPADDR="$(cat "$BASE.$addr.tmp")"
	$ping -n -c1 -w1 "$IPADDR" 2>/dev/null >/dev/null; x=$?
	dbglog "$addr= $IPADDR"
	if [ "$x" -gt 0 ]; then
		echo >&2 "error getting IP: $IPADDR"
		rm "$BASE.$addr.tmp"
		DAYSSINCE="$((($(date +%s) - $(stat -c%Y "$BASE.$addr" || date +%s)) / 60 / 60 / 24))"
		# if we have not successfully gotten our IP address in N full days, then
		# run the failure command with probability 1 in (N+1).
		if [ "$(shuf -i 1-"$((DAYSSINCE + 1))" -n 1)" = 1 ]; then
			echo >&2 "running failure command"
			( set -x; "$@" )
		else
			echo >&2 "not running failure command since it was run recently"
		fi
		exit 1
	fi
	OLDIP="$(cat "$BASE.$addr" 2>/dev/null || echo)"
	if [ "$IPADDR" = "$OLDIP" ]; then
		rm "$BASE.$addr.tmp"
		touch "$BASE.$addr"
	else
		mv -f "$BASE.$addr.tmp" "$BASE.$addr"
		changes=1
	fi
done
if [ "$changes" = 0 ]; then exit 0; fi

output_addresses() {
	local iptype ipaddr
	for addr in $IPTYPES; do
		case $addr in
		addr4) iptype=A;;
		addr6) iptype=AAAA;;
		esac
		ipaddr="$(cat "$BASE.$addr")"
		"format1_${1}" "$domain" "$iptype" "$ipaddr"
	done
}

format1_nsupdate() {
	echo "update add $1. 1200 $2 $3"
}

format1_gandi_livedns() {
	echo -n '{"rrset_type": "'"$2"'", "rrset_ttl": 1200, "rrset_values": ["'"$3"'"]},'
}

update() {
	local method="$1"
	case "$method" in
	nsupdate)
		local ns_auth="$2"
		{
		if [ "$NS_NAME" = "@" ]; then
			local domain="$NS_ZONE"
		else
			local domain="$NS_NAME.$NS_ZONE"
		fi
		cat <<-EOF
		server $ns_auth
		zone $NS_ZONE
		update delete $domain. A
		update delete $domain. AAAA
		EOF
		output_addresses nsupdate
		echo send
		} | nsupdate -k "$BASE.key" -v
		;;
	gandi-livedns)
		local payload=$(
		echo -n '{"items": ['
		output_addresses gandi_livedns
		echo -n ' {}]}'
		)
		! curl -s -H "Authorization: Apikey $(cat "$BASE.key")" \
		  "https://api.gandi.net/v5/livedns/domains/$NS_ZONE/records/$NS_NAME" \
		  -X PUT \
		  -H "Content-Type: application/json" \
		  -d "$payload" \
		  -w ' %{http_code}' | grep -v '201$'
		# ! [etc] -w ' %{http_code}' | grep -v '201$' means to exit 0 only if 201 status code is given
		;;
	esac
}

update $METHOD
