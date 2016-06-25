#!/bin/bash
# Generate a "perfect" GPG key, namely:
# - separate uids for name and emails
# - separate keys each for certify/sign/auth, expire=15m
# - separate key for encryption, expire=9m
#
# Tested against GPG 2.1.11, may not work with other versions
#
# Other modes:
# -u: No-prompt update expiry dates of your C master and S/A subkeys.
# -e: No-prompt generate new encryption subkey.

KEYTYPE_CSA=11 # ECC
KEYPARAM_CSA="1
y" # ECC Ed25519, then pass gnupg 2.1 warning about "non-standard"
EXPIRE_CSA="${EXPIRE_CSA:-15m}" # 1 year + 3 months grace period

KEYTYPE_E=12 # ECC
KEYPARAM_E="1
y" # ECC Curve25519, then pass gnupg 2.1 warning about "non-standard"
EXPIRE_E="${EXPIRE_E:-9m}" # 6 months + 3 months grace period

NOEXPORT=false
operation=key_gen
while getopts 'eun' o; do
	case $o in
	e )	operation=key_add_e;;
	u )	operation=key_update_csa;;
	n ) NOEXPORT=true;;
	\? ) cat >&2 <<eof
Usage: $0 NAME [EMAIL ..]
       $0 -e KEYID
       $0 -u KEYID [SUBKEY_INDEX ..]
eof
		exit 2;;
	esac
done
shift $(expr $OPTIND - 1)

case $operation in
	key_gen)
		name="$1"
		test "${#name}" -ge 5 || { echo >&2 "name too short"; exit 2; }
		GNUPGHOME="${GNUPGHOME:-gpgen-home-$(date +%s)}"
		;;
esac

if [ "$(gpg2 --version | head -n1)" != "gpg (GnuPG) 2.1.11" ]; then
	echo >&2 "Unexpected version (expected 2.1.1); abort"
	exit 2
fi

set -e

GENLOG="$GNUPGHOME/$(basename "$0").$$.$(date +%s)"
gpgex() {
    name="$1"; shift
    gpg2 --no-default-keyring --expert --command-fd 0 --status-fd 3 \
      --homedir="$GNUPGHOME" "$@" \
      3>&1 1>/dev/null | tee -a "$GENLOG.$name.log"
}

operation_intro() {
read -p "Press enter to continue or ctrl-c to abort" x
set -x
mkdir -p "$GNUPGHOME"
chmod 700 "$GNUPGHOME"
}

operation_complete() {
local fingerprint="$1"

if ! $NOEXPORT; then
rm -f "$fingerprint.sec.asc" "$fingerprint.pub.asc"
gpgex export -o "$fingerprint.sec.asc" -a --export-options export-minimal --export-secret-subkeys "$fingerprint"
gpgex export -o "$fingerprint.pub.asc" -a --export "$fingerprint"
fi

set +x
echo "Operation complete. Store ./$GNUPGHOME somewhere offline."
echo "It contains your secret master key and a revocation certificate for it."
echo "You may import $fingerprint.{sec,pub}.gpg to an online gpg installation."
}

key_gen() {
local name="$1"; shift

cat <<EOF
Creating key for $name with emails: $@

You should be doing this on an offline airgapped secure machine. You will be
prompted to enter your master password three times - twice to generate and
third to unlock. After that you will be prompted a bunch more times for the
subkey passwords. It's recommended to have the master password be different
from your subkey passwords, but the latter may be the same for convenience.
EOF
operation_intro

gpgex master --full-gen-key <<EOF
$KEYTYPE_CSA
=c
$KEYPARAM_CSA
$EXPIRE_CSA
y
$name


o
EOF

local fingerprint="$(grep KEY_CREATED "$GENLOG.master.log" | { read proc stat keytype fingerprint; echo "$fingerprint"; })"

{
cat <<EOF
1
primary
1
addkey
$KEYTYPE_CSA
=s
$KEYPARAM_CSA
$EXPIRE_CSA
y
y
addkey
$KEYTYPE_CSA
=a
$KEYPARAM_CSA
$EXPIRE_CSA
y
y
addkey
$KEYTYPE_E
$KEYPARAM_E
$EXPIRE_E
y
y
EOF

for email in "$@"; do
cat <<EOF
adduid
$email
$email

EOF
done

echo save
} | gpgex subkeys --edit-key "$fingerprint"

operation_complete "$fingerprint"
} # end key_gen

key_add_e() {
local keyid="$1"
local fingerprint="$(gpg2 --homedir "$GNUPGHOME" --fingerprint --with-colons "$keyid" | grep ^fpr | cut -d: -f10)"
test -n "$fingerprint"

cat <<EOF
Adding new encryption key for $fingerprint

You should be doing this on an offline airgapped secure machine.
EOF
operation_intro

cat <<EOF | gpgex subkeys --edit-key "$fingerprint"
addkey
$KEYTYPE_E
$KEYPARAM_E
$EXPIRE_E
y
y
save
EOF

operation_complete "$fingerprint"
}

key_update_csa() {
local keyid="$1"; shift
local fingerprint="$(gpg2 --homedir "$GNUPGHOME" --fingerprint --with-colons "$keyid" | grep ^fpr | cut -d: -f10)"
test -n "$fingerprint"

cat <<EOF
Updating expiry date for $fingerprint master key and subkeys $@

You should be doing this on an offline airgapped secure machine.
EOF
operation_intro

{
cat <<EOF
expire
$EXPIRE_CSA
y
EOF

for n in "$@"; do
cat <<EOF
key 0
key $n
expire
$EXPIRE_CSA
y
k
EOF
done

echo save
} | gpgex subkeys --edit-key "$fingerprint"

operation_complete "$fingerprint"
}

$operation "$@"
