#!/bin/sh

USAGE="Usage: $0 [-0|-1|-2|-5|-h] [--] [FINDOPTS]"
FORCE=false
DEBUG=false

while getopts 0125hd o; do
	case $o in
	0 ) SUMS="$SUMS md5";;
	1 ) SUMS="$SUMS sha1";;
	2 ) SUMS="$SUMS sha256";;
	5 ) SUMS="$SUMS sha512";;
	d ) DEBUG=true;;
	h )
		cat <<-EOF
		$USAGE
		Generate hashsums for files in the current directory. Already-calculated sums
		will not be touched; to force recalculation, remove the appropriate file. Extra
		filters can be passed to find(1) by appending them after -- in the command line.

		  -h            This help text.
		  -d            Debug only; show what files would be hashed.
		  -0            Generate md5 sums in MD5SUMS
		  -1            Generate sha1 sums in SHA1SUMS
		  -2            Generate sha256 sums in SHA256SUMS
		  -5            Generate sha512 sums in SHA512SUMS
		EOF
		exit 1
		;;
	\? ) echo $USAGE; exit 1;;
	esac
done
shift `expr $OPTIND - 1`

if [ -z "$SUMS" ]; then echo "$USAGE"; exit 1; fi

FILES="$(find -L . -maxdepth 1 -type f -not -name "*SUMS" "$@" | sort)"

sums() {
	CMD="$1"sum
	DST="$( echo "$1" | tr '[:lower:]' '[:upper:]' )"SUMS
	shift

	if $DEBUG; then CMD="echo \$ $CMD"; fi

	echo "Generating $DST..."
	echo "$FILES" | while read F; do
		MATCH="$(cut '-d ' -f2- $DST | cut -b2- | grep -nxF "$F")"
		if $FORCE || [ -z "$MATCH" ]; then
			$CMD "$F"
		else
			echo "$MATCH" | cut -d: -f1 | { read x; head -n "$x" "$DST"; } | tail -n1
		fi
	done | sort -k2 > "$DST.new"

	if $DEBUG; then cat "$DST.new"; rm "$DST.new";
	else mv "$DST.new" "$DST"; fi
}

for i in $SUMS; do sums "$i"; done
