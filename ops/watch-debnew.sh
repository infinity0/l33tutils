#!/bin/sh
# Collect data on how many packages are in front of you on the NEW queue, and plot them on a graph.
# Recommend add this to a daily cron job.

test -n "$1" || { echo >&2 "Usage: $0 <package>"; exit 2; }
test -f "$1.dat" || cat >"$1.dat" <<EOF
# Queue for $1
Time	Queue length
EOF

set -e

t=$(date +%s)
q=$(curl -s https://ftp-master.debian.org/new.html | sed -n -e '1,/<td class="package">'"$1"'<\/td>/p' | grep '<span class="signature">Fingerprint' | wc -l)
echo >>"$1.dat" "$t	$q"

gnuplot <<EOF
set term png size 1280,800
set output "$1.png"
set xlabel "Date"
set timefmt "%s"
set format x "%Y-%m-%d"
set xdata time
set ylabel "Queue length"
set grid
set key left
plot "$1.dat" usi 1:2 title "Queue length for $1" with linespoints
EOF

tty -s && gnome-open "$1.png"
