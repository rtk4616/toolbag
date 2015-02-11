#!/usr/bin/env python
"""
markdown_toc.py
---------------
AUTHOR: Mike Wilkerson
CREATED: 12/2/2014

DESCRIPTION:
Takes in a markdown file, prints a markdown-formatted table of contents to
stdout.

USAGE: python markdown_toc.py YOUR-FILE.md

EXAMPLE:
I often edit markdown files on OSX using Sublime Text. I usually generate the
TOC and pipe it to pbcopy (the system copy/paste buffer):

$ python markdown_toc.py MY-FILE.md | pbcopy

...Then I'll paste it into Sublime Text!

LICENSE:
Do whatever you want, but at your own risk. I'm not responsible for anything
that happens, and your usage of this script serves as acknowledgment of these
terms.
"""
import re
import sys

to_remove = re.compile("[!?:,/\"'()-.%&]")
USEAGE = (
    "usage: {} [MARKDOWN FILE]"
).format(
    sys.argv[0]
)

OUTPUT = ""

indentations = {}
lines = []

in_code_block = False
for line in sys.stdin:
    if line.startswith('```'):
        in_code_block = not in_code_block
    elif line.startswith("#") and not in_code_block:
        lines.append(line)
        indentations[line.count('#')] = None

for index, key in enumerate(sorted(indentations, key=indentations.get)):
    indentations[key] = index

for line in lines:
    indent_count = indentations[line.count("#")]
    indent = "    " * indent_count
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

    OUTPUT = "\n".join([
        OUTPUT,
        "{}* [{}]({})".format(
            indent,
            toc_text,
            toc_url,
        ),
    ])

print OUTPUT
