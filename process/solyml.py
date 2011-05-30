#!/usr/bin/python -uO
"""Converts between SOL (Adobe Flash savedata) and YAML."""

import pyamf.sol, yaml#, syck
import sys, os, os.path, shutil, traceback

pyamf.sol.decode_ = pyamf.sol.decode
def decode_unstrict(str):
	return pyamf.sol.decode_(str, False)
pyamf.sol.decode = decode_unstrict


class FileRW(object):

	def __init__(self, ifn, isuf, osuf):
		if not ifn.endswith(isuf):
			raise ValueError("suffix %s does not match: %s" % (isuf, ifn))

		self.ifn = ifn
		self.ofn = ifn[:-len(isuf)] + osuf
		self.idata = None
		self.odata = None

	def __call__(self):

		print "reading %s..." % self.ifn,
		self._load()
		print "done"

		self._convert()

		print "writing %s..." % self.ofn,
		self._save()
		print "done"

	def _convert(self):
		self.odata = self.idata

	def _load(self):
		raise NotImplementedError

	def _save(self):
		raise NotImplementedError


class Sol2YmlRW(FileRW):

	def _load(self):
		self.idata = pyamf.sol.load(self.ifn)

	def _save(self):
		with open(self.ofn, 'w') as fp:
			yaml.dump(self.odata, fp, yaml.CDumper)
			#syck.dump(self.idata)


class Yml2SolRW(FileRW):

	def _load(self):
		with open(self.ifn) as fp:
			self.idata = yaml.load(fp, yaml.CLoader)

	def _save(self):
		pyamf.sol.save(self.odata, self.ofn)


class Sol2SolRW(FileRW):

	def _load(self):
		self.idata = pyamf.sol.load(self.ifn)

	def _convert(self):
		FileRW._convert(self)
		fn = self.ifn + ".bak"
		print "backing up %s to %s..." % (self.ifn, fn),
		shutil.copy(self.ifn, fn)
		print "done"

	def _save(self):
		pyamf.sol.save(self.odata, self.ofn)


def sol2yml(fn):
	return Sol2YmlRW(fn, '.sol', '.yml')()


def yml2sol(fn):
	return Yml2SolRW(fn, '.yml', '.sol')()


def sol2sol(fn):
	return Sol2SolRW(fn, '.sol', '.sol')()


SUBCOMMANDS = ("sol2yml", "yml2sol", "sol2sol")


def run_all(cmd, *files):
	for fn in files:
		try:
			globals()[cmd](fn)
		except Exception, e:
			print "skip fn:"
			traceback.print_exc()


def main(argv):
	cmd = os.path.basename(argv[0])
	if cmd in SUBCOMMANDS:
		return run_all(cmd, *argv[1:])

	cmd = os.path.basename(argv[1])
	if cmd in SUBCOMMANDS:
		return run_all(cmd, *argv[2:])

	help()


def help():
	print 'Usage: solyml.py {yml2sol|sol2yml|sol2sol} <FILE> ...'
	print 'Convert a yml or sol [file] to the other format, or attempt to re-write a corrupted sol file.'


if __name__ == '__main__':
	sys.exit(main(sys.argv))
