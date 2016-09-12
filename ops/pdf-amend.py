#!/usr/bin/python3
"""
Amend PDFs more easily, e.g. to sign contracts with.

Requires: python3-pypdf2, imagemagick, gimp
"""
import argparse
import os
import subprocess
import sys

from PyPDF2 import PdfFileMerger
from PyPDF2.pagerange import PageRange

DENSITY_DPI = 300
C = subprocess.check_call

def main(args):
    parser = argparse.ArgumentParser(
        description='Amend one page of a pdf document.')
    parser.add_argument('orig_pdf', type=str)
    parser.add_argument('page_num_to_amend', type=int)
    parser.add_argument('-a', '--amend-with', dest='amend_with')
    parser.add_argument('-o', '--output')
    args = parser.parse_args(args)

    page = args.page_num_to_amend
    pidx = page - 1
    main, ext = os.path.splitext(args.orig_pdf)
    new_pdf = args.output or "%s.amended%s" % (main, ext)
    amend_with = args.amend_with or "%s.page-%s.amended%s" % (main, page, ext)

    if os.path.exists(amend_with):
        print("using existing amended page %s" % amend_with)
    else:
        print("%s does not exist; I will now open GIMP so you can edit page #%s in that file." % (amend_with, page))
        print("After you have made your edits, do File -> Overwrite %s, then exit GIMP." % amend_with)
        print("(It's safe to 'Discard Changes' after Overwrite.)")
        input("Press enter to continue and run GIMP ")
        amend_with_png = "%s.page-%s.amended.png" % (main, page)
        C(("convert -density %s -units pixelsperinch" % DENSITY_DPI).split() +
          ['%s[%s]' % (args.orig_pdf, pidx), amend_with_png])
        C(["gimp", amend_with_png])
        C(["convert", amend_with_png, amend_with])

    with open(args.orig_pdf, 'rb') as orig, open(amend_with, 'rb') as new:
        pdf = PdfFileMerger()
        pdf.merge(0, orig, pages = PageRange("0:%s" % pidx))
        pdf.merge(pidx, new)
        pdf.merge(pidx+1, orig, pages = PageRange("%s:" % (pidx+1)))
        pdf.write(new_pdf)
        print("wrote %s" % new_pdf)

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
