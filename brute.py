#!/usr/bin/python

import sys
from itertools import count
from array import array

AB_TEXT = 'pyfgcrlaoeuidhtnsqjkxbmwvz1234567890PYFGCRLAOEUIDHTNSQJKXBMWVZ'
AB_ASCII = ''.join(chr(i) for i in xrange(32, 127))

def intdefault(val, default):
	if val is None:
		return default
	elif type(val) != int:
		raise ValueError
	elif val < 1:
		return default
	else:
		return val

def string_gen(blen=None, elen=None, alphabet=AB_ASCII):

	blen = intdefault(blen, 1)
	elen = intdefault(elen, None)

	def gen(bytes, pos, length):
		next = pos+1
		if next == length:
			for i in alphabet:
				bytes[pos] = i
				yield bytes
		else:
			for i in alphabet:
				bytes[pos] = i
				for bytes in gen(bytes, next, length):
					yield bytes

	lenrange = count(blen) if elen is None else xrange(blen, elen)
	for length in lenrange:
		bytes = array('c')
		bytes.fromstring(alphabet[0] * length);
		for bytes in gen(bytes, 0, length):
			yield bytes.tostring()


def main(argv):
	try:
		try:
			blen = int(argv[1]) if len(argv) > 1 else None
			elen = int(argv[2]) if len(argv) > 2 else None
		except ValueError:
			raise SyntaxError

		for string in string_gen(blen, elen):
			print string,

	except SyntaxError:
		print >>sys.stderr, "Usage:   brute.py [base_length] [end_length]"
		print >>sys.stderr, "Generates alphanumeric sequences (dvorak keyboard)"
		sys.exit(2)


if __name__ == '__main__':
	main(sys.argv);
