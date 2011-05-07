#!/usr/bin/python

"""
Copyright (C) 2010  Ximin Luo <xl269@cam.ac.uk>
Released under GNU AGPLv3 or later. See http://www.gnu.org/ for details.
"""

import sys, socket, time
from array import array
from twisted.internet import reactor
from twisted.web.resource import Resource, NoResource
from twisted.web.server import Site
from twisted.web.http_headers import Headers

DEFAULT_PORT = 8080
VERSION = 0.8
NAME = "dmgctl.py"
FNAME = NAME


def IPv4ToLong(ip):
	a = array('I')
	a.fromstring(socket.inet_aton(ip))
	if sys.byteorder == 'little': a.byteswap()
	return a.tolist()[0]


def matchLong(subj, test, mask, length):
	if mask > length: raise ValueError("[mask] must be smaller than [length]")
	m = (1<<length)-1 ^ (1<<length-mask)-1
	if test & m != test: raise ValueError("invalid mask: %x/%s" % (test, mask))
	return subj & m == test


def IPv4Match(ip, cidr):
	net, mask = cidr.split('/', 1)
	return matchLong(IPv4ToLong(ip), IPv4ToLong(net), int(mask), 32)


def readlines(prompt, fp=None):
	if fp is None:
		print >>sys.stderr, '%s (blank or "End" to stop):' % prompt
		read = []
		while True:
			try:
				s = raw_input()
				if not s or s == 'End':
					break
				else:
					read.append(s + "\n")
			except EOFError:
				break
		return read
	else:
		return fp.readlines()


def print_flush(s):
	print s
	sys.stdout.flush()


def print_flush_log(s):
	print "%.4f" % time.time(), s
	sys.stdout.flush()


class Server(Resource):

	def __init__(self, allowed=[], msgfile=None):
		Resource.__init__(self)
		self.isLeaf = True
		self.r404 = NoResource()
		self.allowed = list(allowed)
		self.msgfile = msgfile

		for cidr in allowed: IPv4Match("0.0.0.0", cidr) # make sure all valid
		print_flush("# redirection server initialised at %.4f" % time.time())
		print_flush("# allowed subnets = %r" % self.allowed)

	def render_GET(self, request):
		host, path = request.client.host, request.path
		print_flush_log("R %s %s" % (host, path))

		if any(IPv4Match(host, range) for range in self.allowed):
			# TODO something more sophisticated, like forwarding the request
			try:
				fp = open(self.msgfile)
				try:
					return fp.read()
				finally:
					fp.close()
			except:
				return "You're OK (%s)" % host

		return '<meta http-equiv="refresh" content="0;url=http://www.meatspin.com/" />'


if __name__ == '__main__':

	from optparse import OptionParser, OptionGroup, IndentedHelpFormatter
	config = OptionParser(
	  usage = "Usage: %prog [OPTIONS] [PORT]",
	  version = VERSION,
	  description = "Redirects visitors to meatspin, apart from a list of allowed IPs.",
	  formatter = IndentedHelpFormatter(max_help_position=25)
	)

	config.add_option("-a", "--allowed", type="string", metavar="ALLOWED", default=None,
	  help = "file to read allowed CIDR subnets from, 1 per line")
	config.add_option("-f", "--msgfile", type="string", metavar="MSGFILE", default=None,
	  help = "file to output to allowed IPs (read once at startup)")

	(opts, args) = config.parse_args()

	amsg = "Enter CIDR subnets to allow, 1 per line"
	fp = None if opts.allowed is None else open(opts.allowed)
	allowed = [line.strip() for line in readlines(amsg, fp)]

	if len(args) == 0:
		port = DEFAULT_PORT
	elif len(args) == 1:
		port = int(args[0])
	else:
		config.print_help()
		sys.exit(1)

	reactor.listenTCP(port, Site(Server(allowed, opts.msgfile)))
	reactor.run()
