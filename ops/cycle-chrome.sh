#!/bin/sh
# If Chrome segfaults while running Chrome Cast, it will keep segfaulting on startup.
# Run this to fix things and get back to where you were.
CHROMIUM_CONFIGDIR="${CHROMIUM_CONFIGDIR:-$HOME/.config/chromium}"
CHROMIUM_PROFILE="${CHROMIUM_PROFILE:-$CHROMIUM_CONFIGDIR/Default}"

rm -rf "${CHROMIUM_PROFILE}/Extensions/pkedcjkdefgpdelpbcmbmeomcjbeemfm/"
sed -i -e "s/load-media-router-component-extension@1/load-media-router-component-extension@0/" "$CHROMIUM_CONFIGDIR/Local State"
chromium &
sleep 1; kill $!; wait $! # otherwise non-exited child processes will clobber the value below
sed -i -e "s/load-media-router-component-extension@0/load-media-router-component-extension@1/" "$CHROMIUM_CONFIGDIR/Local State"
chromium "$@"
