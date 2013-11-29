import os
import vim

import mike_util

filename = "mike_test.py"
cwd = os.path.realpath(".")
# Looking for filename in the home directory.
fullpath = os.path.realpath(
    os.path.join(
        cwd,
        "..",
        "..",
        "..",
        filename,
    )
)

for linenum, line in enumerate(vim.current.buffer):
    to_return = extract_symbol(line)
    if to_return:
        print "{}, {}, {}".format(
            to_return,
            filename,
            linenum+1,
        )
