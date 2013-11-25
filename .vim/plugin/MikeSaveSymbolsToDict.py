import os
import re
import vim

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

for linenum, line in enumerate(vim.current.buffer):
    if re.compile(r"^(class |def |func|\(defun )").search(line):
        function_name = line.split(" ", 1)[1].split("(", 1)[0]
        print "{}, {}, {}".format(
            function_name,
            filename,
            linenum+1,
        )
