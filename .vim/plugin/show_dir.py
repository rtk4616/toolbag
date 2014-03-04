lines = [
    'import datetime',
    'from lxml.html import parse',
    'from lxml.html import urljoin, tostring',
    'from lxml.html import ',
    'from datetime.',
    'from datetime import datetime',
    'import os, sys',
]


def get_imports_list(import_string):
    return [i.strip() for i in import_string.split(',') if i.strip()]


def do_import(import_string):
    try:
        exec("import {}".format(import_string))
        return True
    except ImportError:
        return False


for line in lines:
    print
    print "line: {}".format(line)

    from_module = None
    import_modules = None

    if "import" in line:
        line, import_modules = line.split("import")

    if "from" in line:
        line, from_module = line.split("from")

    print "from_module: {}".format(from_module)
    print "import_modules: {}".format(import_modules)
    print

    # if from_module and import_modules:
    #     # Fully typed import from another module.
    #     for the_import in get_imports_list(import_modules):
    #         print "RULE 1: from {} import {}".format(
    #             from_module,
    #             the_import,
    #         )
    # elif from_module:
    #     # Partially-typed import line.
    #     print "RULE 2: import {}".format(
    #         from_module,
    #     )
    # elif import_modules:
    #     # Straight-forward import.
    #     for the_import in get_imports_list(import_modules):
    #         print "RULE 3: import {}".format(
    #             the_import,
    #         )
    # else:
    #     print "No pattern match found for \"{}\"".format(line)
