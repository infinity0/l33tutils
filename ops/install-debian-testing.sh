#!/bin/sh
# Create a new bootable Debian testing system, w/o bootloader.
#
# Before using this script, you must create filesystem/s (ideally encrypted and
# under LVM) for your installation, and mount them under $TARGET/.
#

TARGET="${TARGET:-vroot}"

set -ex

debootstrap testing $TARGET/

for i in dev dev/pts proc sys; do
	if ! mountpoint -q "$TARGET/$i"; then
		mount --bind /$i "$TARGET/$i"
	fi
done
cleanup() { for i in dev/pts dev proc sys; do umount "$TARGET/$i"; done; }
trap 'x=$?; cleanup; trap - EXIT; echo $x' EXIT HUP INT QUIT PIPE TERM

cat > "$TARGET/boot/update.sh" <<'EOF'
#!/bin/sh
cd "$(dirname "$0")"
arch="$(dpkg --print-architecture)"
ver="$(apt-cache show --no-all-versions "linux-image-$arch" | sed -n -e 's/Depends: .*\blinux-image-\(.*\)-'"$arch"'\b.*/\1/gp')"

for i in initrd.img vmlinuz; do
  ln -sf "$i-$ver-$arch" "$i"
done
EOF

cat > "$TARGET/etc/apt/apt.conf" <<'EOF'
APT::Default-Release "testing";
EOF

cat > "$TARGET/etc/apt/preferences" <<'EOF'
Package: *
Pin: release a=unstable
Pin-Priority: 300

Package: *
Pin: release a=experimental
Pin-Priority: 200
EOF

cat > "$TARGET/etc/apt/sources.list" <<'EOF'
deb http://security.debian.org/ stable/updates main contrib non-free
deb-src http://security.debian.org/ stable/updates main contrib non-free
deb http://security.debian.org/ testing/updates main contrib non-free
deb-src http://security.debian.org/ testing/updates main contrib non-free

deb http://httpredir.debian.org/debian/ testing main contrib non-free
deb-src http://httpredir.debian.org/debian/ testing main contrib non-free
deb http://httpredir.debian.org/debian/ unstable main contrib non-free
deb-src http://httpredir.debian.org/debian/ unstable main contrib non-free
deb http://httpredir.debian.org/debian/ experimental main contrib non-free
deb-src http://httpredir.debian.org/debian/ experimental main contrib non-free
EOF

cat > "$TARGET/root/setup.sh" <<'EOF'
set -ex
arch="$(dpkg --print-architecture)"

cat > /usr/sbin/policy-rc.d << 'eof'
#!/bin/sh
echo "All runlevel operations denied by policy" >&2
exit 101
eof
chmod a+x /usr/sbin/policy-rc.d

apt-get update
apt-get -y install locales
apt-get -y install lvm2 cryptsetup initramfs-tools network-manager

edit() {
	if [ ! -e "$1" ]; then
		echo "# please fill this file in; you can use your main system's $1 as a template" > "$1"
	fi
	nano "$1"
}

edit /etc/fstab
edit /etc/crypttab
edit /etc/initramfs-tools/conf.d/cryptroot

apt-get -y install firmware-linux-nonfree "linux-image-$arch"
chmod a+x /boot/update.sh
/boot/update.sh
update-initramfs -u

tasksel
apt-get clean
rm -f /usr/sbin/policy-rc.d

echo "root password:"
passwd
read -p "username: " username
adduser "$username"
EOF

LANG=C.UTF-8 chroot "$TARGET" /bin/bash /root/setup.sh
rm "$TARGET/root/setup.sh"
