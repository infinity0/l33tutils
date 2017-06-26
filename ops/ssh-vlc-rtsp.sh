#!/bin/sh
# Stream videos on-demand via RTSP, from a server where you have SSH access.
# It needs to have vlc and nc installed.

HOST="${HOST:-localhost}"
RTSP_IP="${RTSP_IP:-0.0.0.0}"
RTSP_PORT="${RTSP_PORT:-5554}"
TELNET_PORT="${TELNET_PORT:-4212}"
FILE="${FILE:-~/Videos/No Cameras Allowed 2014 HDRip LKRG/No Cameras Allowed 2014 HDRip LKRG.mp4}"
STREAM_NAME="Stream"

if [ "${FILE#~/}" != "${FILE}" ]; then FILE="$HOME/${FILE#~/}"; fi
FILE_ENC="$(python -c "import urllib, os, sys; print urllib.quote(os.path.expanduser(sys.argv[1]))"  "$FILE")"
PASSWORD="$(head -c20 /dev/urandom | base64)"

# -t -t because of https://unix.stackexchange.com/questions/103699/kill-process-spawned-by-ssh-when-ssh-dies
ssh -t -t "$HOST" "sh -es" <<EOF &
test -f "${FILE}" || echo "file given does not exist! exiting"
vlc -I telnet --telnet-port "$TELNET_PORT" --telnet-password "$PASSWORD" --rtsp-host "$RTSP_IP" --rtsp-port "$RTSP_PORT" &
pid=\$!
trap "kill \$pid" EXIT HUP INT QUIT TERM
sleep 1
nc localhost ${TELNET_PORT} <<EOF2
${PASSWORD}
new ${STREAM_NAME} vod enabled
setup ${STREAM_NAME} input "file://${FILE_ENC}"
setup ${STREAM_NAME} option file-caching=300
quit
EOF2
echo "server setup finished. if your client tried to play too early, now is the time to retry."
wait \$!
EOF
pid=$!
trap "kill $pid" EXIT HUP INT QUIT TERM

sleep 5
vlc --no-loop "$@" "rtsp://${HOST}:${RTSP_PORT}/${STREAM_NAME}"
