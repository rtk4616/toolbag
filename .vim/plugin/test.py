import os
import re

filename = "mike_test.py"
cwd = os.path.realpath(".")
fullpath = os.path.realpath(
    os.path.join(
        cwd,
        "..",
        "..",
        "..",
        filename,
    )
)

with open(fullpath) as f:
    lines = [line.strip() for line in f.readlines()]
    for linenum, line in enumerate(lines):
        # if re.compile(r"^(class|def|func|\(defun)").search(line):
        if re.compile(r"^(class |def |func\|\(defun )").search(line):
            print "{} is on line {} of {}".format(
                line.split(" ")[1],
                linenum+1,
                filename,
            )
