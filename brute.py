#!/usr/bin/python

import sys

a = 'pyfgcrlaoeuidhtnsqjkxbmwvz1234567890PYFGCRLAOEUIDHTNSQJKXBMWVZ'

def next():
	global a, l, i, ss, pos
	if len(ss) == l-1:
		while i == len(a):
			i = pos.pop()
			ss = ss[:-1]
			i+=1
	while len(ss) != l-1:
		pos.append(i)
		ss += a[i]
		i = 0
	s = ss + a[i]
	i+=1
	return s


def run(ll, ii, yy):
	global a, l, i, ss, pos
	l = int(ll)
	i = int(ii)
	ss = ''
	pos = []

	if l <= 0 or i < 0:
		return

	while i > len(a):
		k, i = divmod(i, len(a))
		pos.append(k)
		ss += a[k]

	while len(pos) < l-1:
		pos.insert(0, 0)
		ss = a[0] + ss

	for j in range(int(yy)):
		print next(),


def main(argv):
	if len(argv) < 4 and (len(argv) < 2 or argv[1] != 'auto'):
		print "Usage:   brute.py [length] [start] [items]"
		print "         brute.py auto"
		print "Generates alphanumeric sequences (dvorak keyboard)"
		sys.exit(2)

	if argv[1] == 'auto':
		il = 0;
		while True:
			run(il, 0, pow(len(a), il))
			il+=1

	else:
		run(*argv[1:4])


if __name__ == '__main__':
	main(sys.argv);
