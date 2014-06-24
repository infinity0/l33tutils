#!/bin/bash
# Generate a "perfect" GPG key, namely:
# - separate uids for name and emails
# - separate keys each for certify sign auth, expire=1y
# - separate key for encryption, expire=90d
#
# Tested against GPG 2.0.22, may not work with other versions
#
# TODO: version check for gpg
# TODO: move to EdDSA when GPG 2.1 is out
# TODO: sub-script to update expiry dates
# TODO: sub-script to generate new encryption subkey, export only latest one

KEYTYPE=8 # RSA
KEYLEN=4096
EXPIRE_CSA=1y
EXPIRE_E=90d

NAME="$1"; shift
test "${#NAME}" -ge 5 || { echo >&2 "Usage: $0 NAME [EMAIL ...]"; exit 2; }

echo "Creating key for $NAME with emails: $@"
echo "You should be doing this on an offline airgapped secure machine."
echo "You will be prompted to enter your password twice, first to generate and second to unlock."
read -p "Press enter to continue or ctrl-c to abort" x

set -e
set -x

genlog=".$(basename "$0").$$.$(date +%s)"
gpgex() {
    name="$1"; shift
    gpg2 --no-default-keyring --expert --command-fd 0 --status-fd 3 \
      --keyring ./pubring.gpg --secret-keyring ./secring.gpg --trustdb-name ./trustdb.gpg \
       "$@" \
      3>&1 1>/dev/null | tee -a "$genlog.$name.log"
}

gpgex master --gen-key <<EOF
$KEYTYPE
s
e
q
$KEYLEN
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
$KEYTYPE
e
q
$KEYLEN
$EXPIRE_CSA
y
y
addkey
$KEYTYPE
s
e
a
q
$KEYLEN
$EXPIRE_CSA
y
y
addkey
$KEYTYPE
s
q
$KEYLEN
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

gpgex export -o "$fingerprint.sec.gpg" --export-secret-subkey "$fingerprint"
gpgex export -o "$fingerprint.pub.gpg" --export "$fingerprint"
gpgex export -o "$fingerprint.rev.gpg" --gen-revoke "$fingerprint" <<EOF
y
0

y
EOF

set +x
echo "Key generation complete. Store ./{pub,sec}ring.gpg somewhere offline."
echo "Store $fingerprint.rev.gpg somewhere safe offline and don't lose it."
echo "You may import $fingerprint.{sec,pub}.gpg to an online gpg installation."
