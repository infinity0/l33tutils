#!/usr/bin/python
# Poor man's fingerprint validator, given a shared secret

import sys
import hashlib
import hmac

sec = raw_input("Enter the shared secret, that you got from physically meeting up: ")
your_fp = raw_input("Enter your own fingerprint. All caps, no spaces: ")
their_fp = raw_input("Enter their purported fingerprint, that OTR thinks they are using. All caps, no spaces: ")

your_mac = hmac.new(sec, your_fp, hashlib.sha256).hexdigest()
print "Tell them this is your MAC, over the unvalidated channel: %s" % your_mac
their_mac = raw_input("Enter the MAC that they sent you over the unvalidated channel: ")

if their_mac.strip() == hmac.new(sec, their_fp, hashlib.sha256).hexdigest():
	print "Verified. Their fingerprint and MAC matches the shared secret."
else:
	print "WARNING!!! Either you screwed up the inputs, or someone is fucking with you"
