#!/usr/bin/python3
# Inspired by https://github.com/bitcoin/bitcoin/blob/master/src/base58.cpp

import logging

from math import ceil, log
from functools import lru_cache

# the following private functions are to handle leading zeros (in most cases)
# you can ignore it if you just want to know how the algorithm works

def _encdec_len(m, n, maxl):
	return [ceil(ceil(l*log(m)/log(n))*log(n)/log(m)) for l in range(1, maxl)]

"""
Roughly calculate values of n for which it is *not* true that:

	forall[src, m]: conv(n, m, conv(m, n, src)) == "\x00" + src (*)

or in other words, we can't always strip a single leading zero when decoding.

In practise, this function seems to simply return all n for which log(m)/log(n)
is rational, but we haven't yet proved that this is equivalent to (*) which is
what we really want.

Args:
	m: Source base
	maxl: Limit of calculation, high numbers give more accurate results but
		are much slower. Just use the default (127) if you're unsure.
Returns:
	[n]: List of n that satisfies the (*) condition
"""
@lru_cache(256)
def _maybe_ambiguous(m, maxl=127):
	std = _encdec_len(m, m-1, maxl)
	assert std == list(range(2, maxl+1))
	nn = [n for n in range(2, m) if _encdec_len(m, n, maxl) != std]
	logging.info("_maybe_ambiguous(%s) = %s", m, nn)
	return nn

def _has_ambiguous_length(m, n, l):
	return _encdec_len(m, n, l+4).count(l) > 1

"""
Convert a source-string from one base to another. If converting a normal
bytestring, give this m=256.

This function does a basic conversion; there is no translation from [0-n) to
e.g. the standard base64 character set. Also if log(m)/log(n) is rational (i.e.
_maybe_ambiguous selects the pair) then you will need to do some extra padding
yourself to handle leading zeros correctly; see the base64 standard for ideas.

Args:
	m: Source base, each byte of src must be in the range [0-m)
	n: Target base, each byte of dst will be in the range [0-n)
	src: Source bytestring to convert
Returns:
	dst: Target bytestring
"""
def base_conv(m, n, src):
	if m == n:
		return src
	if m > 256 or n > 256:
		raise ValueError("both bases must be >= 256")
	#### the main algorithm follows
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
	#### the main algorithm ends here; the rest is to handle leading zeros
	if m < n and dst:
		if m in _maybe_ambiguous(n) and _has_ambiguous_length(n, m, len(dst)):
			# to avoid this, use a special convertor code for this specific n
			logging.warn("can't unambiguously strip leading zeros for [m=%s, n=%s, len(enc)=%s]; "
				"expect enc-dec AssertionError later...", n, m, len(src))
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

if __name__ == "__main__":
	import os
	import sys
	logging.getLogger().setLevel(logging.INFO)

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
