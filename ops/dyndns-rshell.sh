#!/bin/bash
# Keep this updated with dyndns.sh
set -e
args=( $SSH_ORIGINAL_COMMAND )
fail() {
	echo >&2 "not allowed: ${args[@]}"
	exit 1
}
case "${args[0]}" in
curl)
	test "${args[1]}" = "--connect-timeout" || fail
	test "${args[2]}" -gt 0 || fail
	test "${args[3]}" = "-4" || fail
	test "${args[4]}" = "-s" || fail
	[[ "${args[5]}" =~ ^https:// ]] || fail
	test "${#args[@]}" = 6 || fail
	"${args[@]}"
	;;
stun)
	test "${args[1]}" = "-v" || fail
	test "${#args[@]}" = 3 || fail
	"${args[@]}"
	;;
*)
	fail
	;;
esac
