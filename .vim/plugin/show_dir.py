import re

lines = [
    'import datetime',
    'from lxml.html import parse',
    'from lxml.html import urljoin, tostring',
    'from lxml.html import ',
    'from datetime import datetime',
    'import os, sys',
]
regex_string = '(from ){0,1}([\w.]+){0,1} *import *([\w., ]+)'
regex = re.compile(regex_string)


def get_imports_list(import_string):
    return [i.strip() for i in import_string.split(',') if i.strip()]


def do_import(import_string):
    exec("import {}".format(the_import))


for line in lines:
    _, from_module, import_modules = regex.search(line).groups()

    if isinstance(from_module, str):
        from_module = from_module.strip()

    if isinstance(import_modules, str):
        import_modules = import_modules.strip()

    if from_module and import_modules:
        # Fully typed import from another module.
        for the_import in get_imports_list(import_modules):
            print "RULE 1: from {} import {}".format(
                from_module,
                the_import,
            )
    elif from_module:
        # Partially-typed import line.
        print "RULE 2: import {}".format(
            from_module,
        )
    elif import_modules:
        # Straight-forward import.
        for the_import in get_imports_list(import_modules):
            print "RULE 3: import {}".format(
                the_import,
            )
    else:
        print "No pattern match found for \"{}\"".format(line)
