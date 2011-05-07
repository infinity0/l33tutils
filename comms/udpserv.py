#!/usr/bin/python

import socket
import sys

try:
	server_port = int(sys.argv[1])
except (IndexError, ValueError):
	print "Usage:   udpserv.py [listen port] [forward host] [forward port]"
	print "Listens for UDP packets on the given port and optionally forwards them to a given host."
	sys.exit(2)

host = sys.argv[2] if len(sys.argv) > 2 else None
port = int(sys.argv[3]) if len(sys.argv) > 3 else server_port

if host:
	print "forwarding UDP packets on port %s to %s:%s" % (server_port, host, port)
else:
	print "forwarding UDP packets on port %s to stdout" % server_port

s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
try:
    s.bind(('', server_port))
except socket.error, err:
    print "Couldn't be a udp server on port %d: %s" % (
            server_port, err)
    raise SystemExit

#print s.getsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF)

MAX_TO_READ = 65535

while True:
    datagram = s.recv(MAX_TO_READ)
    if not datagram:
        break
    if host:
        s.sendto(datagram, (host, port))
    else:
        sys.stdout.write(datagram)
s.close()
