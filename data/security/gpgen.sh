#!/bin/bash
# Generate a "perfect" GPG key, namely:
# - separate uids for name and emails
# - separate keys each for certify sign auth, expire=1y
# - separate key for encryption, expire=90d
#
# Tested against GPG 2.1.0, may not work with other versions
#
# TODO: sub-script to update expiry dates
# TODO: sub-script to generate new encryption subkey, export only latest one

KEYTYPE_CSA=11 # ECC
KEYPARAM_CSA="1
y" # ECC Curve255, then pass gnupg 2.1 warning
EXPIRE_CSA=1y

KEYTYPE_E=5 # Elgamal
KEYPARAM_E=4096 # Elgamal keylength
EXPIRE_E=90d

NAME="$1"; shift
test "${#NAME}" -ge 5 || { echo >&2 "Usage: $0 NAME [EMAIL ...]"; exit 2; }

if [ "$(gpg2 --version | head -n1)" != "gpg (GnuPG) 2.1.0" ]; then
	echo >&2 "Unexpected version (expected 2.1.0); abort"
	exit 2
fi

cat <<EOF
Creating key for $NAME with emails: $@

You should be doing this on an offline airgapped secure machine. You will be
prompted to enter your master password three times - twice to generate and
third to unlock. After that you will be prompted a bunch more times for the
subkey passwords. It's recommended to have the master password be different
from your subkey passwords, but the latter may be the same for convenience.
EOF
read -p "Press enter to continue or ctrl-c to abort" x

set -e
set -x

prefix="${prefix:-gpgen-home-$(date +%s)}"
mkdir -p "$prefix"
genlog="$prefix/$(basename "$0").$$.$(date +%s)"
gpgex() {
    name="$1"; shift
    gpg2 --no-default-keyring --expert --command-fd 0 --status-fd 3 \
      --homedir="$prefix" "$@" \
      3>&1 1>/dev/null | tee -a "$genlog.$name.log"
}

gpgex master --full-gen-key <<EOF
$KEYTYPE_CSA
=c
$KEYPARAM_CSA
$EXPIRE_CSA
y
$NAME


o
EOF

fingerprint=$(grep KEY_CREATED "$genlog.master.log" | { read proc stat keytype fingerprint; echo "$fingerprint"; })

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

gpgex export -o "$fingerprint.sec.asc" -a --export-secret-subkey "$fingerprint"
gpgex export -o "$fingerprint.pub.asc" -a --export "${fingerprint:32:8}" # use whol fpr when #1787 is fixed

set +x
echo "Key generation complete. Store ./$prefix somewhere offline."
echo "It contains your secret master key and a revocation certificate for it."
echo "You may import $fingerprint.{sec,pub}.gpg to an online gpg installation."
