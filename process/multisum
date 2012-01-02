#!/bin/sh
# Download a file from different locations and checksums them from each.
# Useful if a remote file doesn't have a published checksum, or a weak one.
# (cough cough, android sdk).
#
# Security is dependant on your remote hosts being unlikely to be all under
# MITM attack by the same attacker. i.e., more geographically diverse hosts =>
# more security.
#
# Exit codes:
# 1 checksum mismatch!
# 2 error calculating checksums locally
# 3 error retrieving with wget

USAGE="Usage: $0 [-f SSH_CONFIG] <REMOTE> [<HOST> ...]"

# when multiple errors occur on multiple hosts, we return smallest one
# i.e. smallest should be most important
ERR_MISMATCH=1
ERR_LOCAL_SUM=2
ERR_WGET=3
ERR_SYNTAX_CFG=253
ERR_SYNTAX_CMD=254

HEADER="# multisum: default-whitespace-hosts"
DEFAULTS="$HOME/.ssh/config"
abort() { X="$1"; shift; echo >&2 "$@"; exit $X; }
info() { true; }

while getopts f:vh o; do
	case $o in
	v )
		info() { echo >&2 "$@"; }
		NEWOPTS="$NEWOPTS -v"
		;;
	f ) DEFAULTS="$OPTARG";;
	h|\? ) echo >&2 "$USAGE"; exit $ERR_SYNTAX_CMD;;
	esac
done
shift `expr $OPTIND - 1`

TARGET="$1"
if [ -z "$TARGET" ]; then
	abort $ERR_SYNTAX_CMD "need to specify a target"
fi

shift
if [ -z "$1" ]; then
	info "reading defaults from $DEFAULTS"
	if [ "$(head -n1 "$DEFAULTS")" = "$HEADER" ]; then
		HOSTS=$(sed -rn -e 's/^Host  (\S+(\s\S+)*)(  .*)?$/\1/p' "$DEFAULTS")
		if [ -n "$HOSTS" ]; then
			info "using hosts: $(echo $HOSTS)"
			echo "$HOSTS" | xargs "$0" $NEWOPTS "$TARGET"
			exit $?
		else
			abort $ERR_SYNTAX_CFG \
			  "no hosts found, expected lines like" '"Host  [multisum hosts]  [other hosts]"'
		fi
	else
		abort $ERR_SYNTAX_CFG "expected header not found: $HEADER"
	fi
fi

sum() {
	SUM=$("$@")
	echo "$SUM"
	SUM=$(echo "$SUM" | cut '-d ' -f1)
}

DST=$(basename "$TARGET")
wget -nv "$TARGET" -O "$DST" || exit $ERR_WGET
sum md5sum "$DST"; MD5SUM="$SUM" || exit $ERR_LOCAL_SUM
sum sha1sum "$DST"; SHA1SUM="$SUM" || exit $ERR_LOCAL_SUM
sum sha256sum "$DST"; SHA256SUM="$SUM" || exit $ERR_LOCAL_SUM

info "now running on remote hosts."
info "errors will be explicitly reported; no news is good news."

X=256
for remote in "$@"; do
info ">> $remote: "
ssh "$remote" sh <<EOF || { Y=$?; [ $Y -lt $X ] && X=$Y; }
test_sum() {
EXPECT="\$1"
ACTUAL=\$("\$2" "\$3" | cut '-d ' -f1)
if [ "\$EXPECT" != "\$ACTUAL" ]; then
	echo >&2 "\$2 mismatch: \$EXPECT != \$ACTUAL"
	return 1
fi
return 0
}

TEMP=\$(tempfile)
trap 'rm -f '"\$TEMP" EXIT INT TERM KILL
wget -nv "$TARGET" -O "\$TEMP" || exit $ERR_WGET
test_sum "$MD5SUM" md5sum "\$TEMP" || exit $ERR_MISMATCH
test_sum "$SHA1SUM" sha1sum "\$TEMP" || exit $ERR_MISMATCH
test_sum "$SHA256SUM" sha256sum "\$TEMP" || exit $ERR_MISMATCH
EOF
done
exit $X
