#!/usr/bin/python

"""
Copyright (C) 2010  Ximin Luo <xl269@cam.ac.uk>
Released under GPLv3 or later. See http://www.gnu.org/ for details.

TODO: Make a fastCGI version of this. It must be fastCGI because we need to be
able to invoke a callback 1s after giving a reply back to the httpd
"""

import sys, socket, threading, time
from twisted.web import server, resource
from twisted.internet import reactor
from twisted.web.client import Agent
from twisted.web.http_headers import Headers

DEFAULT_PORT = 8080
VERSION = 0.7
PARALLEL = 32 # try reducing this number if you get "Too many open files" exception

PROTOCOL = {
	'tcp': [socket.SOCK_STREAM,  lambda x: x.accept()[0]  ],
	'udp': [socket.SOCK_DGRAM,   lambda x: x              ],
}


def print_flush(s):
	print s
	sys.stdout.flush()


def print_flush_log(s):
	print "%.4f" % time.time(), s
	sys.stdout.flush()


def sock_open(type, host, port, bind):
	sock = socket.socket(socket.AF_INET, PROTOCOL[type][0])
	sock.setblocking(0)
	try:
		if bind:
			sock.bind(('0.0.0.0', port))
			if type == 'tcp':
				sock.listen(1)
		else:
			sock.connect((host, port))
	except socket.error, e:
		if e[0] != 115: # Operation now in progress
			raise
	return sock


class counter():

	def __init__(self, start=0, max=65535):
		self.lock = threading.Lock()
		self.port = start
		self.max = max

	def nextport(self):
		self.lock.acquire()
		if self.port >= 0:
			self.port += 1
			if self.port > self.max:
				self.port = -1;
		port = self.port
		self.lock.release()
		return port


def handle_request(host, path, delay):
	print_flush_log("R %s %s" % (host, path))

	(tmp, type, port) = path.split('/', 2)
	port = int(port)
	sock = sock_open(type, host, port, False)

	def cbSend():
		try:
			sock.send("GREAT SUCCESS")
			sock.shutdown(socket.SHUT_RDWR)
		except socket.error, e:
			print_flush_log("E %s %s %s %s" % (host, type, port, e))
		finally:
			sock.close()

	delay(1, cbSend)
	return "awaiting connection to %s %s %s" % (host, type, port)


def run_standalone_server(listen):

	class Server(resource.Resource):
		isLeaf = True

		def render_GET(self, request):
			return handle_request(request.client.host, request.path, reactor.callLater)

	reactor.listenTCP(listen, server.Site(Server()))
	reactor.run()


def run_client(rhost, rport, basepath=None):

	dom = 'http://%s:%s%s' % (rhost, rport, '' if not basepath else '/' + basepath.lstrip('/'))
	s = get_start_port()
	ports = dict([(k, counter(s)) for k in PROTOCOL.iterkeys()])
	finis = dict([(k, counter(s+1)) for k in PROTOCOL.iterkeys()])

	def request_send(type, rhost, port):

		def cbRequestEnd():
			fini = finis[type].nextport()
			next = ports[type].nextport()
			if (next > 0):
				request_send(type, rhost, next)
			else:
				for pc in finis.itervalues():
					if pc.port >= 0:
						return

				from twisted.internet.error import ReactorNotRunning
				try:
					reactor.stop()
				except ReactorNotRunning:
					pass

		try:
			sock = sock_open(type, rhost, port, True)
		except socket.error, e:
			if e[0] != 98 and e[0] != 10048:
				raise
			print_flush("skipping test on %s port %s since it is already in use" % (type, port))
			cbRequestEnd()
			return

		d = Agent(reactor).request('GET', '%s/%s/%s' % (dom, type, port),
		    Headers({'User-Agent': ['Connection Tester %s' % VERSION], 'Connection': 'close'}), None)

		def cbResponse(response):
			if response.code == 200:
				try:
					lsock = PROTOCOL[type][1](sock)
					lsock.setblocking(1)
					lsock.settimeout(4.0)

					try:
						w = lsock.recv(8192)
						if type == 'tcp':
							lsock.shutdown(socket.SHUT_RDWR)
						print_flush("%s port %s is visible to %s (%s)" % (type, port, rhost, w))
					finally:
						lsock.close()

				except socket.error, e:
					print_flush("recv failed on %s %s %s %s" % (rhost, type, port, e))
					raise
				finally:
					sock.close()
			else:
				print_flush(response.__dict__)

			cbRequestEnd()

		def cbShutdown(ignored):
			pass

		d.addCallback(cbResponse)
		d.addBoth(cbShutdown)

	for port in range(0, PARALLEL):
		for k in PROTOCOL.iterkeys():
			request_send(k, rhost, ports[k].nextport())

	reactor.run()


def get_start_port():
	try:
		sock = sock_open('tcp', '', 1023, True)
		sock.close()
		return 0
	except socket.error, e:
		if e[0] != 13: # Operation not permitted
			raise
		else:
			return 1024


if __name__ == '__main__':

	from optparse import OptionParser, OptionGroup, IndentedHelpFormatter
	config = OptionParser(
	  usage = "Usage: %prog [OPTIONS] [HOST] [PORT]",
	  description = "Requests the conntest.py server at HOST:PORT to send test packets to the "
	                "local host, reporting the packets that come through. If HOST is omitted, "
	                "runs a server (based on TwistedWeb) to serve requests from conntest.py "
	                "clients. The server-client protocol is HTTP-based, so it should be able "
	                "to get around the vast majority of network firewalls, as long as the "
	                "server is running on an inconspicuous TCP port (8080 by default).",
	  version = VERSION,
	  formatter = IndentedHelpFormatter(max_help_position=25)
	)

	config.add_option("-m", "--pmax", type="int", metavar="REQ", default=32,
	  help="maximum number of parallel requests, 32 by default. Higher values will "
	       "make the test run quicker, but you may run into OS limits which will cause "
	       "some requests to hang/abort, eg. \"too many open files\". Experiment a "
	       "little to find the optimal value for your system.")
	config.add_option("-b", "--bpath", type="string", metavar="PATH", default='',
	  help="this gives the PATH to the conntest.py script on the remote server. by "
	       "default, we assume the remote server is the TwistedWeb-based standalone "
	       "server, and so we use an empty string for this.")

	(opts, args) = config.parse_args()

	PARALLEL = opts.pmax

	if len(args) == 0:
		run_standalone_server(DEFAULT_PORT)
	elif len(args) == 1:
		run_standalone_server(int(args[0]))
	elif len(args) == 2:
		run_client(args[0], int(args[1]), opts.bpath)
	else:
		config.print_help()
		sys.exit(1)
