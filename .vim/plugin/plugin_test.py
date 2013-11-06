try:
    import fnmatch
    import os
    import re
    import sys
    import vim

    from difflib import SequenceMatcher

    matches = []
    ignored_directories = [
        ".git",
        ".idea",
        "migrations",
        "ve",
    ]
    ignored_extensions = (
        ".swp",
        ".pyc",
    )

    vim.command('call inputsave()')
    vim.command("let user_input = input('Fuzzy filesearch: ')")
    vim.command('call inputrestore()')
    search_string = vim.eval('user_input')
    regex_string = ".*?".join([re.escape(char) for char in search_string])
    the_regex = re.compile(regex_string)

    for root, dirnames, filenames in os.walk('.', topdown=True):
        for ignored_dir in ignored_directories:
            if ignored_dir in dirnames:
                dirnames.remove(ignored_dir)

        for file_name in filenames:
            if file_name.endswith(ignored_extensions):
                continue

            if the_regex.search(file_name.lower()):
                file_path = os.path.realpath(os.path.join(
                    os.getcwd(),
                    root,
                    file_name,
                ))
                matches.append((
                    file_name,
                    file_path.replace(
                        os.path.abspath("."),
                        ""
                    ).replace(
                        file_name,
                        ""
                    ),
                    file_path.replace(" ", "\\ "),
                ))

    to_return = []
    for match_tuple in matches:
        to_return.append((
            SequenceMatcher(
                lambda x: x == " ",
                search_string,
                match_tuple[0],
            ).ratio(),
            match_tuple,
        ))

    to_return = [i[1][2] for i in sorted(to_return,
        key=lambda x: x[0],
        reverse=True
    )]

    vim.command("let l:toReturn = {}".format(to_return))

except ImportError, e:
    print "Error while importing from Python within plugin: {}".format(e)
