import sys


def print_results(the_string):
    print "-" * 80
    print "Final results"
    print "-" * 80
    print the_string


def strip_import_statements(the_string):
    return the_string.replace(
        "from", ""
    ).replace(
        "import", ""
    ).strip()


def get_dir_from_string(the_string):
    try:
        exec("import {}".format(the_string))
        return dir(sys.modules[the_string])
    except ImportError:
        print "ImportError in get_dir_from_string!"
        pass

    try:
        return dir(globals()['parse'])
    except KeyError:
        print "KeyError in get_dir_from_string!"
        pass

    return None


if __name__ == "__main__":
    if len(sys.argv) == 2:
        import_line = sys.argv[1]
        print "import_line was {}".format(import_line)

        if "from" or "import" in import_line:
            # The user has typed "import".

            from_module, to_import = import_line.split('import', 1)
            from_module = strip_import_statements(from_module)
            print "from_module was {}".format(from_module)
            to_import = strip_import_statements(to_import)
            print "to_import was {}".format(to_import)

            if to_import:
                print_results(get_dir_from_string(to_import))
                sys.exit(0)
            else:
                print_results(get_dir_from_string(from_module))
                sys.exit(0)
    else:
        print "usage: {} <name of python module>".format(sys.argv[0])
