#!/usr/bin/python3
"""
Amend PDFs more easily, e.g. to sign contracts with.

Requires: python3-pypdf2, imagemagick, gimp
"""
import argparse
import contextlib
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
    parser.add_argument('page_num_to_amend', type=int, nargs='+')
    parser.add_argument('-a', '--amend-with', dest='amend_with', action='append')
    parser.add_argument('-o', '--output')
    args = parser.parse_args(args)

    main, ext = os.path.splitext(args.orig_pdf)
    pages = args.page_num_to_amend
    amend_withs = args.amend_with
    if not amend_withs:
        amend_withs = ["%s.page-%s.amended%s" % (main, page, ext) for page in pages]
    elif len(amend_withs) != len(pages):
        raise ValueError("there must be an equal number of --amend-with as page_num_to_amend")
    new_pdf = args.output or "%s.amended%s" % (main, ext)
    pairs = sorted(zip(pages, amend_withs))

    for (page, amend_with) in pairs:
        pidx = page - 1
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

    with contextlib.ExitStack() as stack:
        orig = stack.enter_context(open(args.orig_pdf, 'rb'))
        pdf = PdfFileMerger()
        pidx = 0
        for (page, amend_with) in pairs:
            if page > pidx + 1:
                pdf.merge(pidx, orig, pages = PageRange("%s:%s" % (pidx, page - 1)))
            pidx = page - 1
            pdf.merge(pidx, stack.enter_context(open(amend_with, 'rb')))
            pidx += 1
        pdf.merge(pidx, orig, pages = PageRange("%s:" % pidx))
        pdf.write(new_pdf)
        print("wrote %s" % new_pdf)

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
