import re
import sys

to_remove = re.compile("[!:,/\"'()]")
USEAGE = (
    "usage: {} [MARKDOWN FILE]"
).format(
    sys.argv[0]
)

if len(sys.argv) != 2:
    print USEAGE

with open(sys.argv[1], 'r') as f:
    for line in f.readlines():
        if line.startswith("#"):
            toc_indent = line.count("#") - 1
            toc_text = line.replace("#", "").strip().rstrip(":")
            toc_url = re.sub(
                to_remove,
                "",
                toc_text
            ).strip().replace(
                " ",
                "-"
            ).lower()

            toc_url = "#{}".format(toc_url)
            indent = "    " * toc_indent

            print "{}* [{}]({})".format(
                indent,
                toc_text,
                toc_url,
            )
