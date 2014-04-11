#!/bin/sh
# Quick-fire create a password-less $HOST.key and $HOST.csr.
# Note that CACert.org will only keep the HOST and strip everything else.

HOST="$1"
COUNTRY="$2"
EMAIL="$3"

if [ -z "$EMAIL" ]; then
	echo "Usage: $0 <HOST> <COUNTRY> <EMAIL>"
	exit 2
fi

openssl genrsa -passout pass:xxxx -aes256 -out "$HOST.key.orig" 4096
openssl rsa -passin pass:xxxx -in "$HOST.key.orig" -out "$HOST.key"
rm "$HOST.key.orig"
cat > "$HOST.cnf" <<EOF
[ req ]
distinguished_name = req_distinguished_name
[ req_distinguished_name ]
commonName              = Common Name (hostname, IP, or your name)
countryName             = Country Name (2 letter code)
emailAddress            = Email Address
EOF
openssl req -config "$HOST.cnf" -new -key "$HOST.key" -out "$HOST.csr" <<EOF
$HOST
$COUNTRY
$EMAIL
EOF
rm "$HOST.cnf"
# force a line-break
echo
