#!/usr/bin/env python
"""
Uses DVORAK keyboard.
"""

import sys
from random import choice

char_l = '7890[]&*(){}gcrl/=GCRL?+dhtns-#DHTNS_~bmwvzBMWVZ'
char_r = '123456!"$%^\',.pyf@<>PYFaoeuiAOEUI\;qjkx|:QJKX'
chars = char_l + char_r

from optparse import OptionParser
parser = OptionParser(
	usage = "Usage: %prog [OPTIONS] [LENGTH]",
	description = "Generate random passwords.",
)
parser.add_option("-a", "--alternate", action="store_true", dest="alternate", default=False,
  help="pick characters from alternating sides of the keyboard")

(opts, args) = parser.parse_args()
if len(args) > 0:
	passlen = int(args[0])
	if not opts.alternate:
		pw = "".join(choice(chars) for i in range(passlen))
	else:
		pw = "".join(choice(char_l) if i%2 else choice(char_r) for i in range(passlen))
	print pw

else:
	parser.print_usage(sys.stderr)
