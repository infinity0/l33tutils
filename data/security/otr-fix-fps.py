#!/usr/bin/python
# Workaround for http://sourceforge.net/p/otr/bugs/24/

from collections import defaultdict
import re
import sys

accounts = defaultdict(set)
entries = defaultdict(set)
alreadyhave = set()

if len(sys.argv) > 1:
	f = open(sys.argv[1])
	fn = f.name
else:
	f = sys.stdin
	fn = "/path/to/otr.fingerprints"

for line in f.readlines():
	line = line.rstrip('\n')
	(dst, src, proto, fp, ver) = line.split('\t')
	if '/' not in src: continue # not XMPP, don't care
	(node, domain, resource) = re.match(r"^(.+?)@(.+?)/(.*)$", src).groups()
	account = "%s@%s" % (node, domain)
	if resource:
		accounts[account].add(resource)
		alreadyhave.add((dst, src, proto, fp, ver))
	if ver: # only care about verified entries
		entries[account].add((dst, proto, fp, ver))

print "cat >> %s << EOF" % fn

for account, resources in accounts.iteritems():
	for (dst, proto, fp, ver) in sorted(entries[account]):
		for resource in resources:
			entry = (dst, "%s/%s" % (account, resource), proto, fp, ver)
			if entry not in alreadyhave:
				print "\t".join(entry)

print "EOF"
