#!/usr/bin/python

# portscanner.py: checks for active ports on a given machine
# by Ximin Luo; (very) loosely based off portscanner.py from page 13 of
# http://heather.cs.ucdavis.edu/~matloff/Python/PyThreads.pdf

# Usage:   portscanner.py [host] [maxthreads] [timeout]

import socket, threading, sys, time

class counter():

	def __init__(self):
		self.lock = threading.Lock()
		self.port = 0

	def nextport(self):
		self.lock.acquire()
		if self.port >= 0:
			self.port += 1
			if self.port >= 65536:
				self.port = -1;
		port = self.port
		self.lock.release()
		return port


class scanner(threading.Thread):

	tlist = [] # list of all current scanner threads
	pc = counter()

	def __init__(self):
		threading.Thread.__init__(self)
		self.port = scanner.pc.nextport()

	def run(self):
		while (self.port > 0):
			s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
			s.setblocking(0)
			try:
				print ("opening connection to %s:%s\r" % (host, self.port)),
				s.connect((host, self.port))
			except socket.error, e:
				if e[0] != 115: # Operation now in progress
					raise

			try:
				time.sleep(timo)
				s.send("")
				s.shutdown(socket.SHUT_RDWR)
				print "port %s is open                                             " % self.port
			except socket.error:
				#print "port %s is probably closed (no connection after 1 second)" % self.port
				pass
			finally:
				s.close()
			self.port = scanner.pc.nextport()

def main():
	global host, timo

	if len(sys.argv) < 2:
		print "Usage:   portscanner.py [host] [maxthreads] [timeout]"
		print "Scans a host's TCP ports with connection attempts with a given response timeout (default 1s)."
		sys.exit(2)

	host = sys.argv[1]
	maxt = int(sys.argv[2]) if len(sys.argv) > 2 else 666
	timo = int(sys.argv[3]) if len(sys.argv) > 3 else 1
	start = time.time()

	print "port scanning %s, %s ports at a time, timeout %s seconds" % (host, maxt, timo)

	for i in range(0,maxt):
		sc = scanner()
		scanner.tlist.append(sc)
		sc.start()
	for sc in scanner.tlist:
		sc.join()

	print "scan completed in %s seconds                                " % (time.time() - start)

if __name__ == '__main__':
	main()
