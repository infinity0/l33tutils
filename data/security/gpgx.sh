#!/bin/bash
# Execute a command against decrypted file arguments.

USAGE="Usage: $0 [-h|-d] <CMDLINE>"
VECHO=true

while getopts hv o; do
	case $o in
	v ) VECHO=echo;;
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
		EOF
		exit 1
		;;
	\? ) echo $USAGE; exit 1;;
	esac
done
shift `expr $OPTIND - 1`

debug() { $VECHO >&2 "gpgx: $@"; }
abort() { local x="$1"; shift; echo >&2 "gpgx: abort: $@"; exit "$x"; }

NEWAGENT=false
gpgx_init() {
	test -d /dev/shm || abort 255 "/dev/shm not available"
	TMPDIR="$(mktemp -d /dev/shm/gpgx.XXXXXXX || abort 1 "could not make directory")"
	if [ -z "$GPG_AGENT_INFO" ]; then
		if [ -z "$DISPLAY" ]; then
			debug "DISPLAY not available; starting gpg-agent via curses"
			test -x /usr/bin/pinentry-curses || abort 254 "pinentry-curses not installed"
			test -w "$(tty)" || abort 253 "no write permission to $(tty); run 'script /dev/null' first."
			GPG_TTY=$(tty)
			export GPG_TTY
			eval $(gpg-agent --daemon --pinentry-program=/usr/bin/pinentry-curses)
		else
			debug "DISPLAY available; starting gpg-agent via X11"
			eval $(gpg-agent --daemon)
		fi
		NEWAGENT=true
	fi
}

gpgx_clean() {
	if [ -e "$TMPDIR/done" ]; then
		for i in "$TMPDIR"/*.orig; do
			local old="$(readlink "$i")"
			mv -f "$old.new" "$old"
		done
	else
		debug "CHANGES (IF ANY) WERE NOT COMMITTED AND HAVE BEEN LOST!!!"
	fi
	rm -rf "$TMPDIR"
	debug "cleaned up RAM temp dir"
	if $NEWAGENT; then
		kill -TERM $(echo "$GPG_AGENT_INFO" | cut -d: -f2)
		debug "stopped gpg-agent"
	fi
}

gpgx() {
	local args=( $@ )
	local found=false
	for i in "${!args[@]}"; do
		local arg="${args[$i]}"
		if [ "${arg%.gpg}" != "$arg" ]; then
			local f=$(mktemp "$TMPDIR/plain.XXXXXXXX")
			gpg -d "$arg" > "$f" || abort 5 "could not decrypt $arg"
			ln -s "$arg" "$f.orig"
			args[$i]="$f"
			found=true
		fi
	done
	$found || abort 2 "no .gpg arguments found; run without gpgx if you really meant this"
	"${args[@]}"
	for f in "$TMPDIR"/*.orig; do
		local old="$(readlink "$f")"
		gpg -o "$old.new" -e -r "$GNUPGEMAIL" "${f%.orig}" || abort 5 "could not encrypt ${f%.orig}"
	done
	touch "$TMPDIR/done"
}

which "$1" >/dev/null || { echo >&2 "$USAGE"; exit 2; }

gpgx_init
trap 'gpgx_clean' EXIT INT TERM KILL
gpgx "$@"
