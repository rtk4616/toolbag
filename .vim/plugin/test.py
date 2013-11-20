import os
import re

filename = "plugin_test.vim"
cwd = os.path.realpath(".")

with open(filename) as f:
    lines = [line.strip() for line in f.readlines()]
    for linenum, line in enumerate(lines):
        if re.compile(r"^(class|def|func|\(defun)").search(line):
            print "{} is on line {} of {}".format(
                line.split(" ")[1],
                linenum+1,
                filename,
            )
