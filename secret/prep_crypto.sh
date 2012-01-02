#!/bin/sh

USAGE="Usage: $0 <STAGE> <DEVICE>"

STAGE="$1"
case "$STAGE" in
-h | --help | help | 0 | "")
	cat <<-EOF
	$USAGE

	Stages:
	1 | badblocks       check the disk for bad blocks
	2 | urandom         write random data to disk
	3 | cryptsetup      setup the disk using cryptsetup
	EOF
	exit 2
	;;
esac

DEV="$2"
if [ -z "$DEV" ]; then
	echo "need to specify device; one of: ";
	lsblk -bn -r -o NAME -d | while read dev; do
		if [ "$dev" = "$(lsblk -bn -r -o NAME "/dev/$dev")" ]; then
			echo "$dev"
		fi
	done
	exit 1
elif ls -1 "$DEV"?* >/dev/null 2>&1; then
	echo "$DEV already has partitions; abort";
	exit 1
fi

case "$STAGE" in
1|badblocks )
	badblocks -svw -b 65536 "$DEV"
	;;
2|urandom )
	dd if=/dev/urandom of="$DEV" bs=65536 & pid=$!
	sleep 0.1
	while true; do
		kill -USR1 $pid || exit
		sleep 4
	done
	;;
3|cryptsetup )
	echo "not implemented yet"
	exit 1
	;;
esac
