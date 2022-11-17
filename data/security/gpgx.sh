#!/bin/bash
# Execute a command against decrypted file arguments.

GNUPGBIN="${GNUPGBIN:-gpg}"

USAGE="Usage: $0 [-h|-v|-d$ARGS|-e$ARGS] <CMDLINE>"
VECHO=true

while getopts hve:d: o; do
	case $o in
	v ) VECHO=echo;;
	d ) ARGS_DECRYPT="$OPTARG";;
	e ) ARGS_ENCRYPT="$OPTARG";;
	h )
		cat <<-EOF
		$USAGE

		Execute a command against decrypted file arguments. In <CMDLINE>, any argument
		that ends in *.gpg is decrypted into /dev/shm and the argument is replaced with
		the decrypted target. CMDLINE is then run, with those replaced arguments. If
		gpg-agent is not available, a temporary one is spawned to take passphrases.

		Note that any *.gpg argument must be accessible from the CWD to be decrypted;
		this may interfere with programs that look for files in some other directory.

		  -h            This help text.
		  -v            Verbose output; show extra info about what is being done.
		  -e            Extra args for encrypting, e.g. "-r $another_recipient"
		  -d            Extra args for decrypting, e.g. "--pinentry-mode loopback"
		EOF
		exit 1
		;;
	\? ) echo $USAGE; exit 1;;
	esac
done
shift `expr $OPTIND - 1`

debug() { $VECHO >&2 "gpgx: $@"; }
abort() { local x="$1"; shift; echo >&2 "gpgx: abort: $@"; exit "$x"; }

gpgx_init() {
	test -d /dev/shm || abort 255 "/dev/shm not available"
	TMPDIR="$(mktemp -d /dev/shm/gpgx.XXXXXXX || abort 1 "could not make directory")"
}

gpgx_clean() {
	if [ -e "$TMPDIR/done" ]; then
		for f in "$TMPDIR"/*.orig; do
			if [ -f "${f%.orig}.new" ]; then
				local old="$(readlink "$f")"
				mv -f "${f%.orig}.new" "$old"
			fi
		done
	else
		debug "CHANGES (IF ANY) WERE NOT COMMITTED AND HAVE BEEN LOST!!!"
	fi
	rm -rf "$TMPDIR"
	debug "cleaned up RAM temp dir"
}

gpgx() {
	local args=( $@ )
	local found=false
	for i in "${!args[@]}"; do
		local arg="${args[$i]}"
		if [ "${arg%.gpg}" != "$arg" -o "${arg%.pgp}" != "$arg" ]; then
			local f=$(mktemp "$TMPDIR/plain.XXXXXXXX")
			$GNUPGBIN -d $ARGS_DECRYPT "$arg" > "$f" || abort 5 "could not decrypt $arg"
			sha256sum "$f" > "$f.sha256"
			ln -s "$arg" "$f.orig"
			args[$i]="$f"
			found=true
		fi
	done
	$found || abort 2 "no .gpg or .pgp arguments found; run without gpgx if you really meant this"
	"${args[@]}"
	for f in "$TMPDIR"/*.orig; do
		if ! sha256sum -c --status "${f%.orig}.sha256"; then
			local old="$(readlink "$f")"
			local recipients
			mapfile -t recipients < <(gpg -q --with-colons --list-packets "$old" | grep "pubkey enc packet" | sed -re 's/.*keyid (\w+).*/\1/g')
			$GNUPGBIN -o "${f%.orig}.new" -e ${recipients[@]/#/-r } $ARGS_ENCRYPT "${f%.orig}" || abort 5 "could not encrypt ${f%.orig}"
		fi
	done
	touch "$TMPDIR/done"
}

which "$1" >/dev/null || { echo >&2 "$USAGE"; exit 2; }

gpgx_init
trap 'x=$?; gpgx_clean; trap - EXIT; echo >&2 $x' EXIT HUP INT QUIT PIPE TERM
gpgx "$@"
