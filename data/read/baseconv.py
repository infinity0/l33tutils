#!/usr/bin/python3
# Inspired by https://github.com/bitcoin/bitcoin/blob/master/src/base58.cpp

import os
import sys

from math import ceil, log
from functools import lru_cache

def _encdec_len(m, n, maxl):
	return [ceil(ceil(l*log(m)/log(n))*log(n)/log(m)) for l in range(1, maxl)]

@lru_cache(256)
def _maybe_ambiguous(m, maxl=127):
	# calculate values of n for which it is *not* true that:
	# forall[src, m]: conv(n, m, conv(m, n, src)) == "\x00" + src
	# l can be any number, really. this should be "big enough" yet still quick.
	std = _encdec_len(m, m-1, maxl)
	assert std == list(range(2, maxl+1))
	nn = [n for n in range(2, m) if _encdec_len(m, n, maxl) != std]
	print("_maybe_ambiguous(%s) = %s" % (m, nn), file=sys.stderr)
	return nn

def _has_ambiguous_length(m, n, l):
	return _encdec_len(m, n, l+4).count(l) > 1

def base_conv(m, n, src):
	if m == n:
		return src
	if m > 256 or n > 256:
		raise ValueError()
	dst = [0] * ceil(len(src) * log(m) / log(n))
	rev = range(len(dst)-1, -1, -1)
	for ch in src:
		if ch >= m:
			raise ValueError("input had invalid character: %s" % ch)
		# optimised way of calculating dst = m*dst + ch
		r = ch
		for i in rev:
			r += m * dst[i]
			dst[i] = r % n
			r //= n
		assert r == 0
	if m < n and dst:
		if m in _maybe_ambiguous(n) and _has_ambiguous_length(n, m, len(dst)):
			# to avoid this, use a special convertor code for this specific n
			print("W: can't unambiguously strip leading zeros for [m=%s, n=%s, len(enc)=%s]; "
				"expect enc-dec AssertionError later..." % (n, m, len(src)), file=sys.stderr)
		else:
			# strip a single excess leading zero when decoding, as per _maybe_ambiguous
			assert dst[0] == 0, "_maybe_ambiguous was short, [m=%s, n=%s, len(enc)=%s]" % (n, m, len(src))
			dst.pop(0)
	return bytes(dst)

def test_enc_dec(m, n, src, p=True):
	dst = base_conv(m, n, src)
	src2 = base_conv(n, m, dst)
	assert src == src2, "enc-dec are not inverses [m=%s, n=%s]:\n  %s, %s, %s" % (m, n, src, dst, src2)
	if p:
		print("PASS", src, "<=>", dst)

M = 256
N = int(sys.argv[1]) if len(sys.argv) > 1 else 253
L = int(sys.argv[2]) if len(sys.argv) > 2 else 65536

test_enc_dec(M, N, b"lololol")
test_enc_dec(M, N, b"\x00lololol")
test_enc_dec(M, N, b"\x00\x00lololol")
test_enc_dec(M, N, b"\x00\xfflololol")
test_enc_dec(M, N, b"\xfflololol")

for i in range(L):
	test_enc_dec(M, N, os.urandom(ord(os.urandom(1))), False)
	print("%s/%s" % (i, L), end='\r')
print("PASS random test lead-zero assumption")
